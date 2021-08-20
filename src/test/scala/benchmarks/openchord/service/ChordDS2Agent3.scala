package benchmarks.openchord.service

import benchmarks.openchord.data._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.tracing.Logging.AgentMessaging.{received, sent}

import java.util.logging.Logger
import scala.collection.mutable


// ------------------------------------------------------------------------
//      PROTOCOL
// ------------------------------------------------------------------------

/**
 * membership/joining
 */
case class FindSuccessor(sn: Node, identifier: ID) extends
  Message("FindSuccessor", List(sn, identifier))

/** membership */
case class ClosestPrecedingNode(identifier: ID) extends
  Message("ClosestPrecedingNode", List(identifier))

/** membership/joining */
case class SuccessorFound(s: Node) extends
  Message("SuccessorFound", List(s))

/** membership/joining */
case class UpdatePredecessor(pre: Node) extends Message("UpdatePredecessor", List(pre))

/** membership/joining */
case class UpdateFingerTable(s: Node, i: Int) extends Message("UpdateFingerTable", List(s, i))

/** membership/joining */
case class RecSuccessorList(sl: List[Node]) extends Message("RecSuccessorList", List(sl))

// ------------------------------------------------------------------------
// READS AND WRITES
// ------------------------------------------------------------------------

/** Map.write == UploadEntry(Entry) ------> write invocation (this is the one that starts a write of a k-v entry)
 * forwards the entry along the ring till it reaches the correct node and it stores it to its 'entries' table */
case class UploadEntry(e: Entry, originatorNode: Node = null, client: Agent = null) extends
  Message("UploadEntry", List(e.key, e.value, e.id, originatorNode, client))

/**
 *
 * To be sent from persisting node to the originator node.
 */
case class UploadEntryResponse(e: Entry, originatorNode: Node = null, client: Agent) extends
  Message("UploadEntryResponse", List(e.key, e.value, e.id, originatorNode, client))

/** Map.read == Request ------> read (starts recursively finding node responsible for the Request)
 * this is a "read" invocation */
case class Request(originalKey: String = "", node: Node, key: ID, client: Agent = null) extends
  Message("Request", List(originalKey, node, key, client))

/** read response
 * sent from node carrying the entries to the originator node */
case class Response(requestKey: String = "", client: Agent, e: Set[Entry]) extends
  Message("Response", List(requestKey, client, e))

/**
 * Sent from originator of a read/write to client
 */
case class ReadResponse(key: String, value: Any = null) extends Message("ReadResponse", Seq(key, value))

case class WriteResponse(key: String, value: Any = null) extends Message("WriteResponse", Seq(key, value))

// ------------------------------------------------------------------------
//   OpenChord Agent
// ------------------------------------------------------------------------

/**
 * This class work represent the chord actor which acts as an remote endpoint to communicate with the remote chords
 * The class shares same objects with ChordImpl
 *
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *
 *         based on the Akka implementation done by Zepeng Zhao.
 */
class ChordDS2Agent3(val localURL: String = "oc",
                     val node: Node,
                     val ref: References,
                     val e: Entries,
                     val resp: Responses,
                     val be: BackupEntries,
                     log: Boolean = false) extends Agent(localURL) {

  //-------------------------------------------------------------------------------------------
  //                                       Decls and init
  //-------------------------------------------------------------------------------------------
  // don't change fields (constant entire lifecycle of an agent)
  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME) // stateless
  private val localNode = node
  locked = false

  // declarations (only those that change need be captured in local state)
  private val REFERENCES = s"references${LocalState.DELIM}References"
  private val ENTRIES = s"entries${LocalState.DELIM}Entries"
  private val BACKUP_ENTRIES = s"backupEntries${LocalState.DELIM}BackupEntries"
//  private val RESPONSES = s"responses${LocalState.DELIM}Responses"
  private val COUNTER = s"counter${LocalState.DELIM}Int"
//  private val REQUESTS_TRACKER = s"requestsTracker${LocalState.DELIM}RequestsTracker"

  // type aliases
  private type REFERENCES_TYPE = References
  private type ENTRIES_TYPE = Entries
  private type BACKUP_ENTRIES_TYPE = BackupEntries
//  private type RESPONSES_TYPE = Responses
  private type COUNTER_TYPE = Int
//  private type REQUESTS_TRACKER_TYPE = RequestsTracker
  // initializations
  localState(REFERENCES) = ref
  localState(ENTRIES) = e
  localState(BACKUP_ENTRIES) = be
//  localState(RESPONSES) = resp
  localState(COUNTER) = 0
//  localState(REQUESTS_TRACKER) = RequestsTracker() // initially empty requests

  // The SPEC that WGL will capture, that represents the Empty Entries table (check
  private val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"
  type SPEC_TYPE = mutable.Map[Any, Any]
  localState(SPEC) = mutable.Map.empty[Any, Any]

  //-------------------------------------------------------------------------------------------
  //                                     UpdateFingerTable
  //-------------------------------------------------------------------------------------------
  /** once a new node joins a network, it updates the nodes preceding it through this case class */
  //    case UpdateFingerTable(s,i)=>{

  val updateFingerTableAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[UpdateFingerTable]
    if (log) received(m.sender, mm, this)

    var references = localState[REFERENCES_TYPE](REFERENCES) // mutates

    if (!(references.contains(mm.s) && mm.s.equals(localNode)) || mm.i < 5) {
      if (log) logger.info("receive new node[" + mm.s.getURL + "]")
      // copy, mutate, re-assign
      val copy = references.copy // copy
      copy.addReference(mm.s) // mutate
      localState(REFERENCES) = copy // re-assign
      
      // re read references
      references = localState[REFERENCES_TYPE](REFERENCES)
      
      if (mm.i + 1 < 50 && references.getPredecessor != null &&
        !references.getPredecessor.equals(localNode)) {
        if (log) logger.info("inform predecessor about the new joining node[" + mm.s.getURL + "]")

        val msg = UpdateFingerTable(mm.s, mm.i + 1)
        ds.send(this, msg, ds.get(references.getPredecessor.getURL))
        if (log) sent(this, msg, ds.get(references.getPredecessor.getURL))
      }
    } else if (log) logger.info("Node [" + mm.s.getURL + "] already exists")
  }

  /** */
  //-------------------------------------------------------------------------------------------
  //                                     SuccessorFound
  //-------------------------------------------------------------------------------------------
  /** add successor to references table */
  //  case SuccessorFound(s)=>{
  val successorFoundAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)
    val mm = m.asInstanceOf[SuccessorFound]
    val references = localState[REFERENCES_TYPE](REFERENCES)

    // copy, mutate, re-assign
    val copy = references.copy // copy
    copy.addReference(mm.s) // mutate
    localState(REFERENCES) = copy // re-assign
    
  }

  //-------------------------------------------------------------------------------------------
  //                                     RecSuccessorList
  //-------------------------------------------------------------------------------------------

  /** add successor list to references table one by one */
  //  case RecSuccessorList(sl) =>{
  val recSuccessorListAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)

    val mm = m.asInstanceOf[RecSuccessorList]
    val references = localState[REFERENCES_TYPE](REFERENCES)

    
    mm.sl.foreach { x =>
      // copy, mutate, re-assign
      val copy = references.copy // copy
      copy.addReference(x) // mutate
      localState(REFERENCES) = copy // re-assign
    }
  }

  //-------------------------------------------------------------------------------------------
  //                                     UpdatePredecessor
  //-------------------------------------------------------------------------------------------
  //  case UpdatePredecessor(pre) => {
  val updatePredecessorAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)
    val mm = m.asInstanceOf[UpdatePredecessor]
    val references = localState[REFERENCES_TYPE](REFERENCES)

    val copy = references.copy // copy
    copy.updatePredecessor(mm.pre) // mutate
    localState(REFERENCES) = copy // re-assign
  }

  //-------------------------------------------------------------------------------------------
  //                                     FindSuccessor
  //-------------------------------------------------------------------------------------------

  /**
   * find successor of id for remote node rn iteratively in the local finger table
   * and recursively getting closer to the desired one.
   * once reached the successor of id, return the remote node rn with all the references that
   * the successor has.
   */
  //  case FindSuccessor(rn, id)=>{
  val findSuccessorAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[FindSuccessor]
    val rn = mm.sn
    val id = mm.identifier

    var references = localState[REFERENCES_TYPE](REFERENCES) // only mutates

    if (log) received(mm.sender, mm, this)

    //println("find_successor")
    if (log) logger.info("find successors for [" + rn.getURL + "]")
    if (rn != null && id != null) {
      val c = references.getClosestPrecedingNode(id)
      val s: Node = references.getSuccessor
      if (s == null || s.equals(localNode)) {
        if (!rn.equals(localNode)) {

          if (log) logger.info("I am the only node in the network, send my info to node[" + rn.getURL + "]")

          var msg: Message = SuccessorFound(localNode)
          ds.send(this, msg, ds.get(rn.getURL))
          if (log) sent(this, msg, ds.get(rn.getURL))

          msg = UpdatePredecessor(localNode) // update my predecessor
          ds.send(this, msg, ds.get(rn.getURL))
          if (log) sent(this, msg, ds.get(rn.getURL))

          // copy, mutate, re-assign
          val copy = references.copy // copy
          copy.addReference(rn) // mutate
          localState(REFERENCES) = copy // re-assign
        }
      }
      else if (id.isInInterval(localNode.getID, s.getID)) {
        if (log) logger.info("I am the predecessor for node[" + rn.getURL + "]")

        var msg: Message = SuccessorFound(s)
        ds.send(this, msg, ds.get(rn.getURL))
        if (log) sent(this, msg, ds.get(rn.getURL))

        msg = UpdatePredecessor(localNode)
        ds.send(this, msg, ds.get(rn.getURL))
        if (log) sent(this, msg, ds.get(rn.getURL))

        // copy, mutate, re-assign
        val copy = references.copy //copy
        copy.addReference(rn) // mutate
        localState(REFERENCES) = copy // re-assign
        
        //re-read
        references = localState[REFERENCES_TYPE](REFERENCES)

        val temp = references.getSuccessorList.getCopy
        if (temp.nonEmpty) {
          msg = RecSuccessorList(temp)
          ds.send(this, msg, ds.get(rn.getURL))
          if (log) sent(this, msg, ds.get(rn.getURL))
        }
      }
      else {
        var msg: Message = new Message("whatever that will be replaced")

        if (c != null && !c.equals(localNode) && !c.equals(rn)) {

          msg = FindSuccessor(rn, id)
          ds.send(this, msg, ds.get(c.getURL))
          if (log) sent(this, msg, ds.get(c.getURL))

          if (log) logger.info("find closest predecessor[" + c.getURL + "] for node[" + rn.getURL + "],passing task to it")
        }
        else {
          if (log) logger.info("happens to be the successor of node[" + rn.getURL + "]")

          msg = SuccessorFound(localNode)
          ds.send(this, msg, ds.get(rn.getURL))
          if (log) sent(this, msg, ds.get(rn.getURL))

          msg = SuccessorFound(references.getPredecessor)
          ds.send(this, msg, ds.get(rn.getURL))
          if (log) sent(this, msg, ds.get(rn.getURL))

          val temp = references.getSuccessorList.getCopy
          if (temp.nonEmpty) {
            msg = RecSuccessorList(temp)
            ds.send(this, msg, ds.get(rn.getURL))
            if (log) sent(this, msg, ds.get(rn.getURL))
          }
        }
      }
    }
  }
  //-------------------------------------------------------------------------------------------
  //                                     UploadEntry
  //-------------------------------------------------------------------------------------------

  /**
   * Recursively find the successor that is responsible for entry e and upload the entry e to this
   * corresponding successor
   */
  //  case class UploadEntry(e: Entry) extends Message("UploadEntry", List(e.id1, e.k, e.v))
  //  case UploadEntry(e: Entry) =>{
  val uploadEntryAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)

    // use these to fill the find-successor message fields used below
    val key = if (!m.isInstanceOf[UploadEntry]) m.payload[String](0) else m.asInstanceOf[UploadEntry].e.key
    val value = if (!m.isInstanceOf[UploadEntry]) m.payload[Any](1) else m.asInstanceOf[UploadEntry].e.value
    val entryID = if (!m.isInstanceOf[UploadEntry]) HashFunction.createID(key) else m.asInstanceOf[UploadEntry].e.id
    val client = if(!m.isInstanceOf[UploadEntry]) m.sender else m.asInstanceOf[UploadEntry].client

    val e = Entry(entryID, key, value)

    val references = localState[REFERENCES_TYPE](REFERENCES) // only mutates
    var entries = localState[ENTRIES_TYPE](ENTRIES) // only mutates
    var msg: Message = new Message("whatever")

    val cp = references.getClosestPrecedingNode(e.id)
    val succ = references.getSuccessor
    val pre = references.getPredecessor
    if (pre != null && !pre.equals(localNode) && e.id.isInInterval(pre.getID, localNode.getID)) {

      // copy, mutate, re-assign
      val copy = entries.copy // copy
      copy.add(e) // mutate
      localState(ENTRIES) = copy // re-assign
      
      // re-read
      entries = localState[ENTRIES_TYPE](ENTRIES)
      
      if (log) logger.info("Entry with [" + e.key + s"] is uploaded to node: $name")

      //send a response to self node (new reaction) using UploadEntryResponse
      val msg = UploadEntryResponse(e, node, client)
      ds.send(this, msg, this)
      if (log) sent(this, msg, this)

    } else if (cp != null && !cp.equals(localNode)) {

      if (log) logger.info("find key[" + e.key + "]'s closest predecessor, pass the upload task to it")

      msg = UploadEntry(e, localNode, client)
      ds.send(this, msg, ds.get(cp.getURL))
      if (log) sent(this, msg, ds.get(cp.getURL))

    } else if (succ != null) {

      msg = UploadEntry(e, localNode, client)
      ds.send(this, msg, ds.get(succ.getURL))
      if (log) sent(this, msg, ds.get(succ.getURL))

      if (log) logger.info("pass the upload task to successor")

    } else {

      // copy, mutate, re-assign
      val copy = entries.copy // copy
      copy.add(e) // mutate
      localState(ENTRIES) = copy // re-assign
      
      
      if (log) logger.info("Entry with [" + e.key + s"] is uploaded to node: $name")

      //send a response to self node (new reaction) using UploadEntryResponse
      val msg = UploadEntryResponse(e, node, client)
      ds.send(this, msg, this)
      if (log) sent(this, msg, this)

    }
  }

  //-------------------------------------------------------------------------------------------
  //                                     ThisRequest
  //-------------------------------------------------------------------------------------------

  /**
   * Recursively find the successor that is responsible for request req
   * and then return the requested node with a response message.
   */
  //  case class ThisRequest(req: Request) extends Message("ThisRequest", List(req.reqNode, req.reqID, req.reqKey))
  //  case ThisRequest(req)=>{
  val requestAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)
    val references = localState[REFERENCES_TYPE](REFERENCES) // accessed
    val entries = localState[ENTRIES_TYPE](ENTRIES) // accessed

    // new fields
    val key = if (!m.isInstanceOf[Request]) m.payload[String](0) else m.asInstanceOf[Request].originalKey
    val node = if (!m.isInstanceOf[Request]) localNode else m.asInstanceOf[Request].node
    val client = if (!m.isInstanceOf[Request]) m.sender else m.asInstanceOf[Request].client
    // the old req.reqKey is current keyID
    val keyID = if (!m.isInstanceOf[Request]) HashFunction.createID(key) else m.asInstanceOf[Request].key

    val req = Request(key, node, keyID, client) // new each time

    //    val key = req.reqKey
    val cp = references.getClosestPrecedingNode(keyID)
    val succ = references.getSuccessor
    val pre = references.getPredecessor

    var msg = new Message("whatever")
    if (pre != null && !pre.equals(localNode) && keyID.isInInterval(pre.getID, localNode.getID)) {

      msg = Response(key, client, entries.getEntries(req.key))
      ds.send(this, msg, ds.get(req.node.getURL))
      if (log) sent(this, msg, ds.get(req.node.getURL))

    } else if (succ != null && keyID.isInInterval(localNode.getID, succ.getID)) {

      msg = req
      ds.send(this, msg, ds.get(succ.getURL))
      if (log) sent(this, msg, ds.get(succ.getURL))

    } else if (cp != null && !cp.equals(localNode)) {

      msg = req
      ds.send(this, msg, ds.get(cp.getURL))
      if (log) sent(this, msg, ds.get(cp.getURL))

    } else if (succ != null) {

      msg = req
      ds.send(this, msg, ds.get(succ.getURL))
      if (log) sent(this, msg, ds.get(succ.getURL))

    } else {
      msg = Response(key, client, entries.getEntries(req.key))
      ds.send(this, msg, ds.get(req.node.getURL))
      if (log) sent(this, msg, ds.get(req.node.getURL))
    }
  }

  //-------------------------------------------------------------------------------------------
  //                                     Response
  //-------------------------------------------------------------------------------------------
  //  case Response(id, res)=> {
  val responseAction: Action = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)

    val mm = m.asInstanceOf[Response]
    val client = mm.client
    val res = mm.e
    val key = mm.requestKey
    val value = if(res.nonEmpty) res.head.value else ""

//    val responses = localState[RESPONSES_TYPE](RESPONSES) // mutates
//    // Also how to map 'id' to the original 'k' again?!!! use bytes of k as id and append counter?
//    responses.receive(client, res)


    // send response to client
    val msg = ReadResponse(key, value)
    ds.send(this, msg, client)
    if (log) sent(this, msg, client)
  }
  //-------------------------------------------------------------------------------------------
  //                               upload entry response
  //-------------------------------------------------------------------------------------------
  private val uploadEntryResponseAction = new Action + Statement { (m: Message, _: Agent) =>
    if (log) received(m.sender, m, this)

    val mm = m.asInstanceOf[UploadEntryResponse]
    val client = mm.client
    val e = mm.e
    val value = e.value
    val key = e.key

    // notify client (client response)
    val msg = WriteResponse(key, value)
    ds.send(this, msg, client)
    if (log) sent(this, msg, client)
  }

  //-------------------------------------------------------------------------------------------
  //                                     Adding Reactions
  //-------------------------------------------------------------------------------------------

  defaultBehavior += new Message("FindSuccessor") -> findSuccessorAction // used for: membership
  defaultBehavior += new Message("SuccessorFound") -> successorFoundAction // used for: membership
  defaultBehavior += new Message("UpdatePredecessor") -> updatePredecessorAction // membership
  defaultBehavior += new Message("UpdateFingerTable") -> updateFingerTableAction // membership (e.g. joining, leaving)
  defaultBehavior += new Message("RecSuccessorList") -> recSuccessorListAction // membership

  defaultBehavior += new Message("UploadEntry") -> uploadEntryAction // a write (k,v)
  // Mo: added this for sending responses to the clients (e.g. IRed) to form a complete history
  defaultBehavior += new Message("UploadEntryResponse") -> uploadEntryResponseAction // internal response to write -> leads to client-response

  defaultBehavior += new Message("Request") -> requestAction // a read (k)
  defaultBehavior += new Message("Response") -> responseAction // internal response to a read -> leads to client-response

  reactions = defaultBehavior

  specialReactions + (Start(), Statement { (m: Message, _: Agent) =>

    /**
     * Start action (added by Mo)
     * - join the ring => how ??? by sending FindSuccessor? YES, the first branch of the if-else chain
     * - [X] Once a node joins update preceding nodes => UpdateFingerTable(s,i) (does this include updated pre?)
     */
    /* in the harness, I put the name of each agent start sequence to have the name of the node next to it
    and then this agent sends that neighbor a message to find successor.
    * */

    if (log) received(m.sender, m, this)

    if (name != "1") { // not first node, since first one is alone in the ring

      val neighbor = ds.get(m.payload[String](0))
      // empty string for empty key to indicate not-read/not-write search for a successor
      val msg = FindSuccessor(localNode, localNode.id)
      ds.send(this, msg, neighbor)

      if (log) sent(this, msg, neighbor)
    }
  })
  //-------------------------------------------------------------------------------------------
  //                                     Util. Methods
  //-------------------------------------------------------------------------------------------

//  /**
//   * When using this with the <code>Request</code> message, make sure to maintain a mapping
//   * at the original actor to keep track of both the key and the client that made the request.
//   *
//   * @return a string to be used as a request id (unlike the <code>HashFunction.createID(k.getbytes)</code>)
//   */
//  private def makeRequestID(): String = {
//    val reqID = System.currentTimeMillis().toString + "_" + localState[COUNTER_TYPE](COUNTER)
//    localState(COUNTER) = localState[COUNTER_TYPE](COUNTER) + 1
//    reqID
//  }

}