//package benchmarks.openchord.service
//
//import benchmarks.openchord.data._
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
//import edu.utah.cs.gauss.ds2.core.ir.datastructures._
//import edu.utah.cs.gauss.ds2.core.tracing.Logging.AgentMessaging._
//
//import java.util.logging.Logger
//import scala.collection.mutable
//
//
///** Map.read == ThisRequest(Request) ------> read-related (starts recursively finding node responsible for the req:Request)
// * this is a "read" invocation */
//case class ThisRequest(req: Request) extends Message("ThisRequest", List(req.reqNode, req.reqID, req.reqKey))
//
///** read related???
// * an ack (similar to readReplicaAck)... it tracks the acks received so far */
//case class Response(code: String, e: Set[Entry]) extends Message("Response", List(code, e))
//
///** read + write related???
// * joining too */
//case class FindSuccessor(sn: Node, identifier: ID) extends Message("FindSuccessor", List(sn, identifier))
//
////// read + write related??? NO =====> ChordImpl related (not part of chord)
////// returns the successor of this current node receiving it
////case class Successor() extends Message("Successor")
//
//// read + write related???
//// NO ===> this is related to ChordImpl since it sends a bare object (no message wrapping it and no payload)
////case class RequestPredecessor() extends Message("RequestPredecessor")
//
///** read + write related??? */
//case class ClosestPrecedingNode(identifier: ID) extends Message("ClosestPrecedingNode", List(identifier))
//
///** read + write related */
//case class SuccessorFound(s: Node) extends Message("SuccessorFound", List(s))
//
///** joining */
//case class UpdatePredecessor(pre: Node) extends Message("UpdatePredecessor", List(pre))
//
///** joining */
//case class UpdateFingerTable(s: Node, i: Int) extends Message("UpdateFingerTable", List(s, i))
//
///** Map.write == UploadEntry(Entry) ------> write-related (this is the one that starts a write of a k-v entry)
// * forwards the entry along the ring till it reaches the correct node and it stores it to its 'entries' table */
//case class UploadEntry(e: Entry) extends Message("UploadEntry", List(e.id1, e.k, e.v))
//
///** write related in bulk
// * writes them all to the current node (the one receiving it without communication with others) */
//case class UploadEntries(e: Set[Entry]) extends Message("UploadEntries", List(e))
//
///** joining */
//case class RecSuccessorList(sl: List[Node]) extends Message("RecSuccessorList", List(sl))
//
////// read related??? Request in bulk ====> NO, this is related to ChordImpl
////case class RequestAll() extends Message("RequestAll")
//
///** not write nor read (backs up the received entries 'e' in the local backupEntries table with 'e') */
//case class TransmitBackupEntries(n: Node, e: List[Set[Entry]]) extends Message("TransmitBackupEntries", List(n, e))
//
///** write related ---> the appropriate node when found, adds all of 'e' to its 'entries' table
// * forwarding entries from one node to another (started by node 'n') */
//case class CarryEntries(n: Node, e: List[Set[Entry]]) extends Message("CarryEntries", List(n, e))
//
///**
// * This class work represent the chord actor which acts as an remote endpoint to communicate with the remote chords
// * The class shares same objects with ChordImpl
// *
// * @author <br>
// *         Mohammed S. Al-Mahfoudh <br/>
// *         mahfoudh@cs.utah.edu <br/>
// *         SoC - Gauss Group <br/>
// *
// *         based on the Akka implementation done by Zepeng Zhao.
// */
//class ChordDS2Agent2(val localURL: String = "oc",
//                     val node: Node,
//                     val ref: References,
//                     val e: Entries,
//                     val resp: Responses,
//                     val be: BackupEntries,
//                     log: Boolean = false) extends Agent(localURL) {
//
//  //-------------------------------------------------------------------------------------------
//  //                                       Decls and init
//  //-------------------------------------------------------------------------------------------
//  // don't change fields (constant entire lifecycle of an agent)
//  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME) // stateless
//  private val localNode = node
//
//  // declarations (only those that change need be captured in local state)
//  private val REFERENCES = s"references${LocalState.DELIM}References"
//  private val ENTRIES = s"entries${LocalState.DELIM}Entries"
//  private val BACKUP_ENTRIES = s"backupEntries${LocalState.DELIM}BackupEntries"
//  private val RESPONSES = s"responses${LocalState.DELIM}Responses"
//  private val COUNTER = s"counter${LocalState.DELIM}Int"
//
//  // type aliases
//  private type REFERENCES_TYPE = References
//  private type ENTRIES_TYPE = Entries
//  private type BACKUP_ENTRIES_TYPE = BackupEntries
//  private type RESPONSES_TYPE = Responses
//  private type COUNTER_TYPE = Int
//  // initializations
//  localState(REFERENCES) = ref
//  localState(ENTRIES) = e
//  localState(BACKUP_ENTRIES) = be
//  localState(RESPONSES) = resp
//  localState(COUNTER) = 0
//
//  // The SPEC that WGL will capture, that represents the Empty Entries table (check
//  // ChordDS2System.scala to see that)
//  private val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"
//  type SPEC_TYPE = mutable.Map[Any, Any]
//  localState(SPEC) = mutable.Map.empty[Any, Any]
//
//  //-------------------------------------------------------------------------------------------
//  //                                     UpdateFingerTable
//  //-------------------------------------------------------------------------------------------
//  /** once a new node joins a network, it updates the nodes preceding it through this case class */
//  //    case UpdateFingerTable(s,i)=>{
//
//  val updateFingerTableAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[UpdateFingerTable]
//    if (log) received(m.sender, mm, this)
//
//    val references = localState[REFERENCES_TYPE](REFERENCES) // mutates
//
//    if (!(references.contains(mm.s) && mm.s.equals(localNode)) || mm.i < 5) {
//      if (log) logger.info("receive new node[" + mm.s.getURL + "]")
//      references.addReference(mm.s)
//      if (mm.i + 1 < 50 && references.getPredecessor != null &&
//        !references.getPredecessor.equals(localNode)) {
//        if (log) logger.info("inform predecessor about the new joining node[" + mm.s.getURL + "]")
//        val msg = UpdateFingerTable(mm.s, mm.i + 1)
//        ds.send(this, msg, ds.get(references.getPredecessor.getURL))
//        if (log) sent(this, msg, ds.get(references.getPredecessor.getURL))
//      }
//    } else if (log) logger.info("Node [" + mm.s.getURL + "] already exists")
//  }
//
//  //  //-------------------------------------------------------------------------------------------
//  //  //                                     RequestAll
//  //  //-------------------------------------------------------------------------------------------
//  //  /** request all the node references that a nodes stores, including itself */
//  //  //  case RequestAll() => {
//  //  val requestAllAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//  //    val mm = m.asInstanceOf[RequestAll]
//  //    val references = localState[REFERENCES_TYPE](REFERENCES)
//  //
//  //    if(log) received(sender,mm,this)
//  //
//  //    if(log) logger.info("receive node list from [" + sender.name + "]")
//  //
//  //    var nl = references.getSuccessorList.getCopy
//  //    val pre = references.getPredecessor
//  //    if (pre != null)
//  //      nl = nl :+ pre
//  //    nl = nl :+ localNode
//  //    sender ! nl // is this sent to the ChordImpl? looks like it since there is no reaction to it here
//  //
//  //  }
//  /** */
//  //-------------------------------------------------------------------------------------------
//  //                                     SuccessorFound
//  //-------------------------------------------------------------------------------------------
//  /** add successor to references table */
//  //  case SuccessorFound(s)=>{
//  val successorFoundAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[SuccessorFound]
//    val references = localState[REFERENCES_TYPE](REFERENCES)
//
//    references.addReference(mm.s)
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     RecSuccessorList
//  //-------------------------------------------------------------------------------------------
//
//  /** add successor list to references table one by one */
//  //  case RecSuccessorList(sl) =>{
//  val recSuccessorListAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[RecSuccessorList]
//    val references = localState[REFERENCES_TYPE](REFERENCES)
//
//    mm.sl.foreach { x => references.addReference(x) }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     UpdatePredecessor
//  //-------------------------------------------------------------------------------------------
//  //  case UpdatePredecessor(pre) => {
//  val updatePredecessorAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[UpdatePredecessor]
//    val references = localState[REFERENCES_TYPE](REFERENCES)
//
//    references.updatePredecessor(mm.pre)
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     FindSuccessor
//  //-------------------------------------------------------------------------------------------
//
//  /**
//   * find successor of id for remote node rn iteratively in the local finger table
//   * and recursively getting closer to the desired one.
//   * once reached the successor of id, return the remote node rn with all the references that
//   * the successor has.
//   */
//  //  case FindSuccessor(rn, id)=>{
//  val findSuccessorAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[FindSuccessor]
//    val rn = mm.sn
//    val id = mm.identifier
//    val references = localState[REFERENCES_TYPE](REFERENCES) // only mutates
//
//    if (log) received(mm.sender, mm, this)
//
//    //println("find_successor")
//    if (log) logger.info("find successors for [" + rn.getURL + "]")
//    if (rn != null && id != null) {
//      val c = references.getClosestPrecedingNode(id)
//      val s: Node = references.getSuccessor
//      if (s == null || s.equals(localNode)) {
//        if (!rn.equals(localNode)) {
//
//          if (log) logger.info("I am the only node in the network, send my info to node[" + rn.getURL + "]")
//
//          var msg: Message = SuccessorFound(localNode)
//          ds.send(this, msg, ds.get(rn.getURL))
//          if (log) sent(this, msg, ds.get(rn.getURL))
//
//          msg = UpdatePredecessor(localNode)
//          ds.send(this, msg, ds.get(rn.getURL))
//          if (log) sent(this, msg, ds.get(rn.getURL))
//
//          references.addReference(rn)
//        }
//      }
//      else if (id.isInInterval(localNode.getID, s.getID)) {
//        if (log) logger.info("I am the predecessor for node[" + rn.getURL + "]")
//
//        var msg: Message = SuccessorFound(s)
//        ds.send(this, msg, ds.get(rn.getURL))
//        if (log) sent(this, msg, ds.get(rn.getURL))
//
//        msg = UpdatePredecessor(localNode)
//        ds.send(this, msg, ds.get(rn.getURL))
//        if (log) sent(this, msg, ds.get(rn.getURL))
//
//        references.addReference(rn)
//
//        val temp = references.getSuccessorList.getCopy
//        if (temp.nonEmpty) {
//          msg = RecSuccessorList(temp)
//          ds.send(this, msg, ds.get(rn.getURL))
//          if (log) sent(this, msg, ds.get(rn.getURL))
//        }
//      }
//      else {
//        var msg: Message = new Message("whatever that will be replaced")
//
//        if (c != null && !c.equals(localNode)) {
//
//          msg = FindSuccessor(rn, id)
//          ds.send(this, msg, ds.get(c.getURL))
//          if (log) sent(this, msg, ds.get(c.getURL))
//
//          if (log) logger.info("find closest predecessor[" + c.getURL + "] for node[" + rn.getURL + "],passing task to it")
//        }
//        else {
//          if (log) logger.info("happens to be the successor of node[" + rn.getURL + "]")
//
//          msg = SuccessorFound(localNode)
//          ds.send(this, msg, ds.get(rn.getURL))
//          if (log) sent(this, msg, ds.get(rn.getURL))
//
//          msg = SuccessorFound(references.getPredecessor)
//          ds.send(this, msg, ds.get(rn.getURL))
//          if (log) sent(this, msg, ds.get(rn.getURL))
//
//          val temp = references.getSuccessorList.getCopy
//          if (temp.nonEmpty) {
//            msg = RecSuccessorList(temp)
//            ds.send(this, msg, ds.get(rn.getURL))
//            if (log) sent(this, msg, ds.get(rn.getURL))
//          }
//        }
//      }
//    }
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     UploadEntry
//  //-------------------------------------------------------------------------------------------
//
//  /**
//   * Recursively find the successor that is responsible for entry e and upload the entry e to this
//   * corresponding successor
//   */
//  //  case class UploadEntry(e: Entry) extends Message("UploadEntry", List(e.id1, e.k, e.v))
//  //  case UploadEntry(e: Entry) =>{
//  val uploadEntryAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    if (log) received(m.sender, m, this)
//
//    val e = new Entry(m.payload[ID](0), m.payload[String](1), m.payload[Serializable](2))
//
//    /**
//     * val k1 = "k1"
//     * val entryID1 = HashFunction.createID(k1.getBytes()) // one write to use this
//     */
//
//    val references = localState[REFERENCES_TYPE](REFERENCES) // only mutates
//    val entries = localState[ENTRIES_TYPE](ENTRIES) // only mutates
//    var msg: Message = new Message("whatever")
//
//    val cp = references.getClosestPrecedingNode(e.getID())
//    val succ = references.getSuccessor
//    val pre = references.getPredecessor
//    if (pre != null && !pre.equals(localNode) && e.getID().isInInterval(pre.getID, localNode.getID)) {
//
//      entries.add(e)
//      if (log) logger.info("Entry with [" + e.getKey() + "] is uploaded to the current node")
//
//    } else if (cp != null && !cp.equals(localNode)) {
//
//      if (log) logger.info("find key[" + e.getKey() + "]'s closest predecessor, pass the upload task to it")
//
//      msg = UploadEntry(e)
//      ds.send(this, msg, ds.get(cp.getURL))
//      if (log) sent(this, msg, ds.get(cp.getURL))
//
//    } else if (succ != null) {
//
//      msg = UploadEntry(e)
//      ds.send(this, msg, ds.get(succ.getURL))
//      if (log) sent(this, msg, ds.get(succ.getURL))
//
//      if (log) logger.info("pass the upload task to successor")
//
//    } else {
//
//      entries.add(e)
//      if (log) logger.info("Entry with [" + e.getKey() + "] is uploaded to the current node")
//
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     ThisRequest
//  //-------------------------------------------------------------------------------------------
//
//  /**
//   * Recursively find the successor that is responsible for request req
//   * and then return the requested node with a response message.
//   */
//  //  case class ThisRequest(req: Request) extends Message("ThisRequest", List(req.reqNode, req.reqID, req.reqKey))
//  //  case ThisRequest(req)=>{
//  val thisRequestAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    if (log) received(m.sender, m, this)
//    val references = localState[REFERENCES_TYPE](REFERENCES) // accessed
//    val entries = localState[ENTRIES_TYPE](ENTRIES) // accessed
//
//
//    val req = Request(m.payload[Node](0), m.payload[String](1), m.payload[ID](3))
//    /*
//        var reqID = makeRequestID()
//        val req = new Request(this.localNode, reqID, key)
//     */
//
//    val key = req.reqKey
//    val cp = references.getClosestPrecedingNode(key)
//    val succ = references.getSuccessor
//    val pre = references.getPredecessor
//
//    var msg = new Message("whatever")
//    if (pre != null && !pre.equals(localNode) && key.isInInterval(pre.getID, localNode.getID)) {
//
//      msg = Response(req.reqID, entries.getEntries(req.reqKey))
//      ds.send(this, msg, ds.get(req.reqNode.getURL))
//      if (log) sent(this, msg, ds.get(req.reqNode.getURL))
//
//    } else if (succ != null && key.isInInterval(localNode.getID, succ.getID)) {
//
//      msg = ThisRequest(req)
//      ds.send(this, msg, ds.get(succ.getURL))
//      if (log) sent(this, msg, ds.get(succ.getURL))
//
//    } else if (cp != null && !cp.equals(localNode)) {
//
//      msg = ThisRequest(req)
//      ds.send(this, msg, ds.get(cp.getURL))
//      if (log) sent(this, msg, ds.get(cp.getURL))
//
//    } else if (succ != null) {
//
//      msg = ThisRequest(req)
//      ds.send(this, msg, ds.get(succ.getURL))
//      if (log) sent(this, msg, ds.get(succ.getURL))
//
//    } else {
//      msg = Response(req.reqID, entries.getEntries(req.reqKey))
//      ds.send(this, msg, ds.get(req.reqNode.getURL))
//      if (log) sent(this, msg, ds.get(req.reqNode.getURL))
//    }
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                 TransmitBackupEntries
//  //-------------------------------------------------------------------------------------------
//
//  /** n normally precedes localNode, back up entries for n. */
//  //  case TransmitBackupEntries(n, e) => {
//  val transmitBackupEntriesAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    if (log) received(m.sender, m, this)
//
//    val mm = m.asInstanceOf[TransmitBackupEntries]
//    val n = mm.n
//    val e = mm.e
//    val backupEntries = localState[BACKUP_ENTRIES_TYPE](BACKUP_ENTRIES)
//
//    if (log) logger.info("back up entries for predecessor[" + n.getURL + "]")
//    backupEntries.setEntries(n, e)
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     CarryEntries
//  //-------------------------------------------------------------------------------------------
//
//  /** carry entries for node n, which most likely is predecessor of current node */
//  //  case CarryEntries(n, e) => {
//  val carryEntriesAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    if (log) received(m.sender, m, this)
//    val mm = m.asInstanceOf[CarryEntries]
//    val references = localState[REFERENCES_TYPE](REFERENCES) // accessed
//    val entries = localState[ENTRIES_TYPE](ENTRIES) // mutates
//    val n = mm.n
//    val e = mm.e
//
//    val succ = references.getSuccessor
//    if (succ != null && !this.localNode.equals(succ) && n.getID.isInInterval(this.localNode.getID, succ.getID)) {
//
//      val msg = CarryEntries(n, e)
//      ds.send(this, msg, ds.get(succ.getURL))
//      if (log) sent(this, msg, ds.get(succ.getURL))
//
//    } else {
//      if (log) logger.info("carry entries for dead predecessor")
//      e.foreach { x => entries.addAll(x) }
//    }
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     UploadEntries
//  //-------------------------------------------------------------------------------------------
//  //  case UploadEntries(e) => {
//  val uploadEntriesAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    if (log) received(m.sender, m, this)
//
//    val e = m.payload[Set[Entry]](0)
//    val entries = localState[ENTRIES_TYPE](ENTRIES) // mutates
//
//    entries.addAll(e)
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     Response
//  //-------------------------------------------------------------------------------------------
//  //  case Response(id, res)=> {
//  val responseAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//    val mm = m.asInstanceOf[Response]
//    val id = mm.code
//    val res = mm.e
//    val responses = localState[RESPONSES_TYPE](RESPONSES) // mutates
//
//
//    // Also how to map 'id' to the original 'k' again?!!! use bytes of k as id and append counter?
//    responses.receive(id, res)
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     Successor
//  //-------------------------------------------------------------------------------------------
//  ////  case Successor()=>
//  //  val successorAction: Action = new Action + Statement {(m:Message,_:Agent)=>
//  //    if(log) received(m.sender, m,this)
//  //    val references = localState[REFERENCES_TYPE](REFERENCES) // accessed
//  //    sender ! references.getSuccessor // ChordImpl (cuz: no message wrapping and no matching case for the sent message)
//  //    val msg =
//  //  }
//
//
//  //-------------------------------------------------------------------------------------------
//  //                                    RequestPredecessor
//  //-------------------------------------------------------------------------------------------
//  //  case RequestPredecessor() => {
//  //  val requestPredecessorAction: Action = new Action + Statement { (m: Message, _: Agent) =>
//  //    if (log) received(m.sender, m, this)
//  //    val references = localState[REFERENCES_TYPE](REFERENCES) // accessed
//  //
//  //    sender ! references.getPredecessor // sent to ChordImpl? (because no message wrapping and no matching case for the sent message)
//  //  }
//
//
//  /** Adding reactions follow */
//  //-------------------------------------------------------------------------------------------
//  //                                     Adding Reactions
//  //-------------------------------------------------------------------------------------------
//
//  defaultBehavior += new Message("UpdateFingerTable") -> updateFingerTableAction
//  defaultBehavior += new Message("SuccessorFound") -> successorFoundAction
//  defaultBehavior += new Message("RecSuccessorList") -> recSuccessorListAction
//  defaultBehavior += new Message("UpdatePredecessor") -> updatePredecessorAction
//  defaultBehavior += new Message("FindSuccessor") -> findSuccessorAction
//  defaultBehavior += new Message("UploadEntry") -> uploadEntryAction
//  defaultBehavior += new Message("ThisRequest") -> thisRequestAction
//  defaultBehavior += new Message("TransmitBackupEntries") -> transmitBackupEntriesAction
//  defaultBehavior += new Message("CarryEntries") -> carryEntriesAction
//  defaultBehavior += new Message("UploadEntries") -> uploadEntriesAction
//  defaultBehavior += new Message("Response") -> responseAction
//
//  reactions = defaultBehavior
//
//  specialReactions(Start()) + Statement { (m: Message, _: Agent) =>
//
//    /**
//     * Start action (added by Mo)
//     * - join the ring => how ??? by sending FindSuccessor? YES, the first branch of the if-else chain
//     * - [X] Once a node joins update preceding nodes => UpdateFingerTable(s,i) (does this include updated pre?)
//     */
//    /* in the harness, I put the name of each agent start sequence to have the name of the node next to it
//    and then this agent sends that neighbor a message to find successor.
//    * */
//
//    if (log) received(m.sender, m, this)
//
//    if (!name.endsWith("1")) { // not first node
//
//      val neighbor = ds.get(m.payload[String](0))
//      val msg = FindSuccessor(localNode, localNode.id)
//      ds.send(this, msg, neighbor)
//
//      if (log) sent(this, msg, neighbor)
//    }
//  }
//  //-------------------------------------------------------------------------------------------
//  //                                     Util. Methods
//  //-------------------------------------------------------------------------------------------
//
//  private def gmakeRequestID(): String = {
//    val reqID = System.currentTimeMillis().toString + "_" + localState[COUNTER_TYPE](COUNTER)
//    localState(COUNTER) = localState[COUNTER_TYPE](COUNTER) + 1
//    reqID
//  }
//
//}