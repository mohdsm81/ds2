//package benchmarks.openchord.service
//
//import java.util.logging.Logger
//
//import akka.util.Timeout
//import benchmarks.openchord.data._
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Action, Agent, LocalState, Message, Start => StartMSG}
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
//
//
//case class ThisRequest(req: Request) extends Message("ThisRequest", Seq(req))
//
//case class Response(code: String, e: Set[Entry]) extends Message("Response", Seq(code, e))
//
//case class FindSuccessor(sn: Node, ident: ID) extends Message("FindSuccessor", Seq(sn, ident))
//
//case class Successor() extends Message("Successor")
//
//case class RequestPredecessor() extends Message("RequestPredecessor")
//
//case class ClosestPrecedingNode(ident: ID) extends Message("ClosestPrecedingNode", Seq(ident))
//
//case class SuccessorFound(s: Node) extends Message("SuccessorFound", Seq(s))
//
//case class UpdatePredecessor(pre: Node) extends Message("UpdatePredecessor", Seq(pre))
//
//case class UpdateFingerTable(s: Node, i: Int) extends Message("UpdateFingerTable", Seq(s, i))
//
//case class Live() extends Message("Live")
//
//case class UploadEntry(e: Entry) extends Message("UploadEntry", Seq(e))
//
//case class UploadEntries(e: Set[Entry]) extends Message("UploadEntries", Seq(e))
//
//case class RecSuccessorList(sl: List[Node]) extends Message("RecSuccessorList", Seq(sl))
//
//case class RequestAll() extends Message("RequestAll")
//
//case class TransmitBackupEntries(n: Node, e: List[Set[Entry]]) extends Message("TransmitBackupEntries", Seq(n, e))
//
//case class CarryEntries(n: Node, e: List[Set[Entry]]) extends Message("CarryEntries", Seq(n, e))
//
//
///**
// * This class work represent the chord actor which acts as an remote endpoint to communicate with the remote chords
// * The class shares same objects with ChordImpl
// * @author <br>
// *         Mohammed S. Al-Mahfoudh <br/>
// *         mahfoudh@cs.utah.edu <br/>
// *         SoC - Gauss Group <br/>
// *
// *         based on the Akka implementation done by Zepeng Zhao.
// */
//class ChordDS2Agent(name: String,
//                    val node: Node,
//                    val ref: References,
//                    val e: Entries,
//                    val resp: Responses,
//                    val be: BackupEntries,
//                    val log: Boolean = false) extends Agent(name) {
//
//  //  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//  val LOGGER = s"logger${LocalState.DELIM}Logger"
//  localState(LOGGER) = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//  //  private val localNode = node
//  val LOCAL_NODE = s"localNode${LocalState.DELIM}Node"
//  localState(LOCAL_NODE) = node
//  //  private val references = ref
//  val REFERENCES = s"references${LocalState.DELIM}References"
//  localState(REFERENCES) = ref
//  //  private val entries = e
//  val ENTRIES = s"entries${LocalState.DELIM}Entries"
//  localState(ENTRIES) = e
//  //  private val backupEntries = be
//  val BACKUP_ENTRIES = s"backupEntries${LocalState.DELIM}BackupEntries"
//  localState(BACKUP_ENTRIES) = be
//  //  private val responses = resp
//  val RESPONSES = s"responses${LocalState.DELIM}Responses"
//  localState(RESPONSES) = resp
//  //  implicit val timeout = Timeout(5000)
//  val TIMEOUT = s"timeout${LocalState.DELIM}Timeout"
//  localState(TIMEOUT) = Timeout(5000)
//
//
//  val MSG_TO_SEND = s"msgToSend${LocalState.DELIM}Message"
//  val DST_AGENT = s"dstAgent${LocalState.DELIM}Agent"
//
//  ////////////////////////////////////////////////////////////////////////////
//  /////                   Default Behavior Definition
//  ////////////////////////////////////////////////////////////////////////////
//
//  //  override def receive: Receive = {
//
//  //---------------------------------------------------------------------------------------------------
//  //once a new node join in a network, it updates the nodes preceding it through this case class
//  //  case class UpdateFingerTable(s: Node, i: Int)
//  //  case UpdateFingerTable(s, i)
//  //  =>
//  val updateFingerTableAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  if (!(references.contains(s) && s.equals(this.localNode)) || i < 5) {
//  val ifStmt_updFingerTable: If = If((m: Message, a: Agent) => {
//    val mm = m.asInstanceOf[UpdateFingerTable]
//    !(a.localState[References](REFERENCES).contains(mm.s) &&
//      mm.s == a.localState[Node](LOCAL_NODE)) || mm.i < 5
//  })(
//    //    logger.info("receive new node[" + s.getURL + "]")
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("receive new node[" + m.payload[Node](0).getURL + "]") },
//    //    references.addReference(s)
//    Statement((m: Message, a: Agent) => a.localState[References](REFERENCES).addReference(m.payload[Node](0))),
//    //    if (i + 1 < 50 && references.getPredecessor != null &&
//    //      !references.getPredecessor.equals(localNode)) {
//    If((m: Message, a: Agent) => {
//      val mm = m.asInstanceOf[UpdateFingerTable]
//      mm.i < 50 && a.localState[References](REFERENCES).getPredecessor != null &&
//        a.localState[References](REFERENCES).getPredecessor != a.localState[Node](LOCAL_NODE)
//    })(
//      //      logger.info("inform predecessor about the new joining node[" + s.getURL + "]")
//      Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("inform predecessor about the new joining node[" + m.payload[Node](0).getURL + "]") },
//      //      context.actorSelection(references.getPredecessor.getURL) ! UpdateFingerTable(s, i + 1)
//      ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => UpdateFingerTable(m.payload[Node](0), m.payload[Int](1))),
//      ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[References](REFERENCES).getPredecessor.getURL))),
//    Send(MSG_TO_SEND, DST_AGENT)
//    //    } // if
//  ) // if
//  updateFingerTableAction + ifStmt_updFingerTable
//  //  else {
//  //    logger.info ("Node [" + s.getURL + "] already exists")
//  //  }
//  updateFingerTableAction + Else(
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("Node [" + m.payload[Node](0).getURL + "] already exists") }
//  )(ifStmt_updFingerTable)
//
//  defaultBehavior += new Message("UpdateFingerTable") -> updateFingerTableAction
//  //---------------------------------------------------------------------------------------------------
//  //request all the node references that a nodes stores, including itself
//  //  case class RequestAll() extends Message("RequestAll")
//  //  case RequestAll()
//  //  =>
//  val requestAllAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  val NL = s"nl${LocalState.DELIM}List[Node]"
//
//  requestAllAction + Statement { (m: Message, a: Agent) =>
//    if (log) a.localState[Logger](LOGGER).info("receive node list from [" + m.sender.toString() + "]")
//    var nl = a.localState[References](REFERENCES).getSuccessorList.getCopy
//    val pre = a.localState[References](REFERENCES).getPredecessor
//    if (pre != null)
//      nl = nl :+ pre
//    nl = nl :+ a.localState[Node](LOCAL_NODE)
//    a.localState(NL) = nl
//  }
//  //  sender ! nl
//  requestAllAction + ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => a.localState[List[Node]](NL))
//  requestAllAction + ModifyState(DST_AGENT, (m: Message, _: Agent) => m.sender)
//  requestAllAction + Send(MSG_TO_SEND, DST_AGENT)
//
//  defaultBehavior += new Message("RequestAll") -> requestAllAction
//  //---------------------------------------------------------------------------------------------------
//  //add successor to references table
//  //  case class SuccessorFound(s: Node) extends Message("SuccessorFound", Seq(s))
//  //  case SuccessorFound(s)
//  //  =>
//  val successorFoundAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  successorFoundAction + Statement { (m: Message, a: Agent) => a.localState[References](REFERENCES).addReference(m.payload[Node](0)) }
//
//  defaultBehavior += new Message("SuccessorFound") -> successorFoundAction
//  //---------------------------------------------------------------------------------------------------
//  //add successor list to references table one by one
//  //  case class RecSuccessorList(sl: List[Node]) extends Message("RecSuccessorList", Seq(sl))
//  //  case RecSuccessorList(sl)
//  //  =>
//  val recSuccessorListAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  recSuccessorListAction + Statement { (m: Message, a: Agent) =>
//    m.payload[List[Node]](0).foreach { x => a.localState[References](REFERENCES).addReference(x) }
//  }
//
//  defaultBehavior += new Message("RecSuccessorList") -> recSuccessorListAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class UpdatePredecessor(pre: Node) extends Message("UpdatePredecessor", Seq(pre))
//  //  case UpdatePredecessor(pre)
//  //  =>
//  val updatePredecessorAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  updatePredecessorAction + Statement { (m: Message, a: Agent) => a.localState[References](REFERENCES).updatePredecessor(m.payload[Node](0)) }
//
//  defaultBehavior += new Message("UpdatePredecessor") -> updatePredecessorAction
//  //---------------------------------------------------------------------------------------------------
//  /*
//   * find successor of id for remote node rn iteratively in the local finger table
//   * and recursively getting closer to the one that desired
//   * once reach the successor of id, return remote node rn with all the references that
//   * the successor has.
//   */
//  //  case class FindSuccessor(sn: Node, ident: ID) extends Message("FindSuccessor", Seq(sn, ident))
//  //  case FindSuccessor(rn, id)
//  //  =>
//  val findSuccessorAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //println("find_successor")
//
//  findSuccessorAction + Statement { (m: Message, a: Agent) =>
//    if (log) a.localState[Logger](LOGGER).info("find successors for [" + m.payload[Node](0).getURL + "]")
//  }
//
//  //  val ifStmt = _:Statement
//
//  //  if (rn != null && id != null) {
//  //    var c = references.getClosestPrecedingNode(id)
//  //    var s: Node = references.getSuccessor
//
//  val C = s"c${LocalState.DELIM}Node"
//  val S = s"s${LocalState.DELIM}Node"
//
//  val assignmentStmt1: ModifyState = ModifyState(C, (m: Message, a: Agent) => a.localState[References](REFERENCES).getClosestPrecedingNode(m.payload[ID](1)))
//  val assignmentStmt2: ModifyState = ModifyState(S, (_: Message, a: Agent) => a.localState[References](REFERENCES).getSuccessor)
//
//  //  if (s == null || s.equals(localNode)) {
//  //    if (!rn.equals(localNode)) {
//  //      logger.info("I am the only node in the network, send my info to node[" + rn.getURL + "]")
//  //      context.actorSelection(rn.getURL) ! SuccessorFound(localNode)
//  //      context.actorSelection(rn.getURL) ! UpdatePredecessor(localNode)
//  //      references.addReference(rn)
//  //    }
//  //  }
//  val ifStmt1_findSuccessorAction: If = If((_: Message, a: Agent) => {
//    a.localState[Node](S) == null ||
//      a.localState[Node](S) == a.localState[Node](LOCAL_NODE)
//  })(
//    If((m: Message, a: Agent) => m.payload[Node](0) != a.localState[Node](LOCAL_NODE))(
//      Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("I am the only node in the network, send my info to node[" + m.payload[Node](0).getURL + "]") },
//      ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => SuccessorFound(a.localState[Node](LOCAL_NODE))),
//      ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Node](0).getURL)),
//      Send(MSG_TO_SEND, DST_AGENT),
//      ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => UpdatePredecessor(a.localState[Node](LOCAL_NODE))),
//      ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Node](0).getURL)),
//      Send(MSG_TO_SEND, DST_AGENT),
//      Statement { (m: Message, a: Agent) => a.localState[References](REFERENCES).addReference(m.payload[Node](0)) }
//    ) // inner If
//  ) // outer If
//
//
//  //  else if (id.isInInterval(localNode.getID, s.getID)) {
//  //    logger.info("I am the predecessor for node[" + rn.getURL + "]")
//  //    context.actorSelection(rn.getURL) ! SuccessorFound(s)
//  //    context.actorSelection(rn.getURL) ! UpdatePredecessor(localNode)
//  //    references.addReference(rn)
//  //    var temp = references.getSuccessorList.getCopy
//  //    if (!temp.isEmpty)
//  //      context.actorSelection(rn.getURL) ! RecSuccessorList(temp)
//  //  }
//  val TEMP = s"temp${LocalState.DELIM}List[Node]"
//
//  val elseIfStmt1_findSuccessorAction: ElseIf = ElseIf((m: Message, a: Agent) => {
//    m.payload[ID](1).isInInterval(a.localState[Node](LOCAL_NODE).getID, a.localState[Node](S).getID)
//  })(
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("I am the predecessor for node[" + m.payload[Node](0).getURL + "]") },
//    ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => SuccessorFound(a.localState[Node](LOCAL_NODE))),
//    ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Node](0).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT),
//    ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => UpdatePredecessor(a.localState[Node](LOCAL_NODE))),
//    ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Node](0).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT),
//    Statement { (m: Message, a: Agent) => a.localState[References](REFERENCES).addReference(m.payload[Node](0)) },
//    ModifyState(TEMP, (_: Message, a: Agent) => a.localState[References](REFERENCES).getSuccessorList.getCopy),
//    If((_: Message, a: Agent) => a.localState[List[Node]](TEMP).nonEmpty)(
//      ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => RecSuccessorList(a.localState[List[Node]](TEMP))),
//      ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Node](0).getURL)),
//      Send(MSG_TO_SEND, DST_AGENT)
//    )
//  )(ifStmt1_findSuccessorAction)
//
//  //  else
//  //  {
//
//  //    if (c != null && !c.equals(localNode)) {
//  //      context.actorSelection(c.getURL) ! FindSuccessor(rn, id)
//  //      logger.info("find closest predecessor[" + c.getURL + "] for node[" + rn.getURL + "],passing task to it")
//  //    }
//  val ifStmt2_findSuccessorAction: If = If((_: Message, a: Agent) => {
//    a.localState[Node](C) != null &&
//      a.localState[Node](C) != a.localState[Node](LOCAL_NODE)
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => FindSuccessor(m.payload[Node](0), m.payload[ID](1))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](C).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT),
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("find closest predecessor[" + a.localState[Node](C).getURL + "] for node[" + m.payload[Node](0).getURL + "],passing task to it") }
//  )
//
//  //  else
//  //  {
//  //    logger.info("happens to be the successor of node[" + rn.getURL + "]")
//  //    context.actorSelection(rn.getURL) ! SuccessorFound(localNode)
//  //    context.actorSelection(rn.getURL) ! SuccessorFound(this.references.getPredecessor)
//  //    var temp = references.getSuccessorList.getCopy
//  //    if (!temp.isEmpty)
//  //      context.actorSelection(rn.getURL) ! RecSuccessorList(temp)
//  //  } // inner else
//  val elseStmt2_findSuccessorAction: Else = Else(
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("happens to be the successor of node[" + m.payload[Node](0).getURL + "]") },
//    ModifyState(MSG_TO_SEND, (_:Message, a:Agent) => SuccessorFound(a.localState[Node](LOCAL_NODE))),
//    ModifyState(DST_AGENT, (m:Message,a:Agent) => a.ds.get(m.payload[Node](0).getURL)),
//    Send(MSG_TO_SEND,DST_AGENT),
//    ModifyState(MSG_TO_SEND, (_:Message, a:Agent) => SuccessorFound(a.localState[References](REFERENCES).getPredecessor)),
//    ModifyState(DST_AGENT, (m:Message,a:Agent) => a.ds.get(m.payload[Node](0).getURL)),
//    Send(MSG_TO_SEND,DST_AGENT),
//    ModifyState(TEMP, (_:Message, a:Agent)=> a.localState[References](REFERENCES).getSuccessorList.getCopy),
//    If((_:Message, a:Agent) => a.localState[List[Node]](TEMP).nonEmpty)(
//      ModifyState(MSG_TO_SEND, (_:Message, a:Agent) => RecSuccessorList(a.localState[List[Node]](TEMP))),
//      ModifyState(DST_AGENT, (m:Message,a:Agent) => a.ds.get(m.payload[Node](0).getURL)),
//      Send(MSG_TO_SEND,DST_AGENT)
//    )
//  )(ifStmt2_findSuccessorAction)
//
//
//  //  } // outer else
//  //  } / major If
//
//
//  val majorIfStmt_findSuccessorAction: If = If((m: Message, _: Agent) => {
//    m.payload[Node](0) != null &&
//      m.payload[ID](1) != null
//  })(
//    assignmentStmt1,
//    assignmentStmt2,
//    ifStmt1_findSuccessorAction,
//    elseIfStmt1_findSuccessorAction,
//    Else(
//      ifStmt2_findSuccessorAction,
//      elseStmt2_findSuccessorAction
//    )(elseIfStmt1_findSuccessorAction)
//  )
//
//
//  findSuccessorAction + majorIfStmt_findSuccessorAction
//
//  defaultBehavior += new Message("FindSuccessor") -> findSuccessorAction
//  //---------------------------------------------------------------------------------------------------
//  /*
//   * Recursively find the successor that is responsible for entry e and upload the entry e to this
//   * corresponding successor
//   */
//  //  case class UploadEntry(e: Entry) extends Message("UploadEntry", Seq(e))
//  //  case UploadEntry(e: Entry)
//  //  =>
//  val uploadEntryAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  val CP_uploadEntryAction = s"cp${LocalState.DELIM}Node"
//  val SUCC_uploadEntryAction = s"succ${LocalState.DELIM}Node"
//  val PRE_uploadEntryAction = s"pre${LocalState.DELIM}Node"
//  //  var cp = references.getClosestPrecedingNode(e.getID())
//  //  var succ = references.getSuccessor
//  //  var pre = references.getPredecessor
//  uploadEntryAction + Statement { (m: Message, a: Agent) =>
//    val refs = a.localState[References](REFERENCES)
//    a.localState(CP_uploadEntryAction) = refs.getClosestPrecedingNode(m.payload[Entry](0).getID())
//    a.localState(SUCC_uploadEntryAction) = refs.getSuccessor
//    a.localState(PRE_uploadEntryAction) = refs.getPredecessor
//  }
//
//  //  if (pre != null && !pre.equals(localNode) && e.getID().isInInterval(pre.getID, localNode.getID)) {
//  //    this.entries.add(e)
//  //    logger.info("Entry with [" + e.getKey().toString() + "] is uploaded to the current node")
//  //  }
//  val ifStmt_uploadEntryAction: If = If((m: Message, a: Agent) => {
//    a.localState[Node](PRE_uploadEntryAction) != null &&
//      a.localState[Node](PRE_uploadEntryAction) != a.localState[Node](LOCAL_NODE) &&
//      m.payload[Entry](0).getID().isInInterval(a.localState[Node](PRE_uploadEntryAction).getID, a.localState[Node](LOCAL_NODE).getID)
//  })(
//    Statement { (m: Message, a: Agent) =>
//      a.localState[Entries](ENTRIES).add(m.payload[Entry](0))
//      if (log) a.localState[Logger](LOGGER).info("Entry with [" + m.payload[Entry](0).getKey() + "] is uploaded to the current node")
//    }
//  )
//
//  uploadEntryAction + ifStmt_uploadEntryAction
//
//  //  else if (cp != null && !cp.equals(localNode)) {
//  //    logger.info("find key[" + e.getKey() + "]'s closest predecessor, pass the upload task to it")
//  //    context.actorSelection(cp.getURL) ! UploadEntry(e)
//  //  }
//  val elseIfStmt_uploadEntryAction: ElseIf = ElseIf((_: Message, a: Agent) => {
//    a.localState[Node](CP_uploadEntryAction) != null &&
//      a.localState[Node](CP_uploadEntryAction) != a.localState[Node](LOCAL_NODE)
//  })(
//    Statement { (m: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("find key[" + m.payload[Entry](0).getKey() + "]'s closest predecessor, pass the upload task to it") },
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => UploadEntry(m.payload[Entry](0))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](CP_uploadEntryAction).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  )(ifStmt_uploadEntryAction)
//
//  uploadEntryAction + elseIfStmt_uploadEntryAction
//
//  //  else if (succ != null) {
//  //    context.actorSelection(succ.getURL) ! UploadEntry(e)
//  //    logger.info("pass the upload task to successor")
//  //  }
//  val elseIfStmt2_uploadEntryAction: ElseIf = ElseIf((_: Message, a: Agent) => {
//    a.localState[Node](SUCC_uploadEntryAction) != null
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => UploadEntry(m.payload[Entry](0))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](SUCC_uploadEntryAction).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT),
//    Statement { (_: Message, a: Agent) => if (log) a.localState[Logger](LOGGER).info("pass the upload task to successor") }
//  )(elseIfStmt_uploadEntryAction)
//
//  uploadEntryAction + elseIfStmt2_uploadEntryAction
//
//  //  else {
//  //    this.entries.add(e)
//  //    logger.info("Entry with [" + e.getKey().toString() + "] is uploaded to the current node")
//  //  }
//  val elseStmt_uploadEntryAction: Else = Else(
//    Statement { (m: Message, a: Agent) =>
//      a.localState[Entries](ENTRIES).add(m.payload[Entry](0))
//      if (log) a.localState[Logger](LOGGER).info("Entry with [" + m.payload[Entry](0).getKey() + "] is uploaded to the current node")
//    }
//  )(elseIfStmt2_uploadEntryAction)
//
//  uploadEntryAction + elseStmt_uploadEntryAction
//  defaultBehavior += new Message("UploadEntry") -> uploadEntryAction
//  //---------------------------------------------------------------------------------------------------
//  /*
//   * Recursively find the successor that is responsible for request req
//   * and then return the requested node with a response message.
//   */
//  //  case class ThisRequest(req: Request) extends Message("Request", Seq(req))
//  //  case ThisRequest(req)
//  //  =>
//  val requestAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  var key = req.reqKey
//  //  var cp = references.getClosestPrecedingNode(key)
//  //  var succ = references.getSuccessor
//  //  var pre = references.getPredecessor
//  val KEY = s"key${LocalState.DELIM}ID"
//  val CP = s"cp${LocalState.DELIM}Node"
//  val SUCC_requestAction = s"succ${LocalState.DELIM}Node"
//  val PRE = s"pre${LocalState.DELIM}Node"
//
//  requestAction + Statement { (m: Message, a: Agent) =>
//    val mm = m.asInstanceOf[ThisRequest]
//    val refs = a.localState[References](REFERENCES)
//    a.localState(KEY) = mm.req.reqKey
//    a.localState(CP) = refs.getClosestPrecedingNode(a.localState[ID](KEY))
//    a.localState(SUCC) = refs.getSuccessor
//    a.localState(PRE) = refs.getPredecessor
//  }
//
//  //  if (pre != null && !pre.equals(localNode) && key.isInInterval(pre.getID, localNode.getID))
//  //    context.actorSelection(req.reqNode.getURL) ! Response(req.reqID, entries.getEntries(req.reqKey))
//  val ifStmt3_requestAction: If = If((_: Message, a: Agent) => {
//    val pre = a.localState[Node](PRE)
//    val localNode = a.localState[Node](LOCAL_NODE)
//    pre == null && pre != localNode && a.localState[ID](KEY).isInInterval(pre.getID, localNode.getID)
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, a: Agent) =>
//      Response(m.payload[Request](0).reqID,
//        a.localState[Entries](ENTRIES).getEntries(m.payload[Request](0).reqKey))),
//    ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Request](0).reqNode.getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  ) // If
//
//
//  requestAction + ifStmt3_requestAction
//  //  else if (succ != null && key.isInInterval(localNode.getID, succ.getID))
//  //    context.actorSelection(succ.getURL) ! ThisRequest(req)
//  val elseIfStmt_requestAction: ElseIf = ElseIf((_: Message, a: Agent) => {
//    a.localState[Node](SUCC_requestAction) != null &&
//      a.localState[ID](KEY).isInInterval(a.localState[Node](LOCAL_NODE).getID, a.localState[Node](SUCC_requestAction).getID)
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => ThisRequest(m.payload[Request](0))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](SUCC_requestAction).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  )(ifStmt3_requestAction)
//
//
//  requestAction + elseIfStmt_requestAction
//  //  else if (cp != null && !cp.equals(localNode))
//  //    context.actorSelection(cp.getURL) ! ThisRequest(req)
//  val elseIfStmt2_requestAction: ElseIf = ElseIf((_: Message, a: Agent) => {
//    a.localState[Node](CP) != null &&
//      a.localState[Node](CP) != a.localState[Node](LOCAL_NODE)
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => ThisRequest(m.payload[Request](0))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](CP).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  )(elseIfStmt_requestAction)
//
//
//  requestAction + elseIfStmt2_requestAction
//  //  else if (succ != null)
//  //    context.actorSelection(succ.getURL) ! ThisRequest(req)
//  val elseIfStmt3_requestAction: ElseIf = ElseIf((_: Message, a: Agent) => {
//    a.localState[Node](SUCC_requestAction) != null
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => ThisRequest(m.payload[Request](0))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](SUCC_requestAction).getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  )(elseIfStmt2_requestAction)
//
//
//  requestAction + elseIfStmt3_requestAction
//  //  else
//  //    context.actorSelection(req.reqNode.getURL) ! Response(req.reqID, entries.getEntries(req.reqKey))
//  val elseStmt_requestAction: Else = Else(
//    ModifyState(MSG_TO_SEND, (m: Message, a: Agent) => Response(
//      m.payload[Request](0).reqID,
//      a.localState[Entries](ENTRIES).getEntries(m.payload[Request](0).reqKey))),
//    ModifyState(DST_AGENT, (m: Message, a: Agent) => a.ds.get(m.payload[Request](0).reqNode.getURL)),
//    Send(MSG_TO_SEND, DST_AGENT)
//  )(elseIfStmt3_requestAction)
//
//  requestAction + elseStmt_requestAction
//
//  defaultBehavior += new Message("ThisRequest") -> requestAction
//  //---------------------------------------------------------------------------------------------------
//  //n normally precedes localNode, back up entries for n.
//  //  case class TransmitBackupEntries(n: Node, e: List[Set[Entry]]) extends Message("TransmitBackupEntries", Seq(n, e))
//  //  case TransmitBackupEntries(n, e)
//  //  =>
//  val transmitBackupEntriesAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  transmitBackupEntriesAction + Statement { (m: Message, a: Agent) =>
//    val mm = m.asInstanceOf[TransmitBackupEntries]
//    if (log) a.localState[Logger](LOGGER).info("back up entries for predecessor[" + mm.n.getURL + "]")
//    a.localState[BackupEntries](BACKUP_ENTRIES).setEntries(mm.n, mm.e)
//  }
//
//
//  defaultBehavior += new Message("TransmitBackupEntries") -> transmitBackupEntriesAction
//  //---------------------------------------------------------------------------------------------------
//  //carry entries for node n, which most likely is predecessor of current node
//  //  case class CarryEntries(n: Node, e: List[Set[Entry]]) extends Message("CarryEntries", Seq(n, e))
//  //  case CarryEntries(n, e)
//  //  =>
//  val carryEntriesAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  var succ = this.references.getSuccessor
//  val SUCC = s"succ${LocalState.DELIM}Node"
//  carryEntriesAction + Statement { (_, a: Agent) => a.localState(SUCC) = a.localState[References](REFERENCES).getSuccessor }
//  //  if (succ != null && !this.localNode.equals(succ) && n.getID.isInInterval(this.localNode.getID, succ.getID))
//  //    context.actorSelection(succ.getURL) ! CarryEntries(n, e)
//  val ifStmt2_carryEntriesAction: If = If((m: Message, a: Agent) => {
//    val mm = m.asInstanceOf[CarryEntries]
//    a.localState[Node](SUCC) != null &&
//      a.localState[Node](LOCAL_NODE) != a.localState[Node](SUCC) &&
//      mm.n.getID.isInInterval(a.localState[Node](LOCAL_NODE).getID, a.localState[Node](SUCC).getID)
//  })(
//    ModifyState(MSG_TO_SEND, (m: Message, _: Agent) => CarryEntries(m.payload[Node](0), m.payload[List[Set[Entry]]](1))),
//    ModifyState(DST_AGENT, (_: Message, a: Agent) => a.ds.get(a.localState[Node](SUCC).getURL))
//  ) // If
//  carryEntriesAction + ifStmt2_carryEntriesAction
//  //  else {
//  //    logger.info("carry entries for dead predecessor")
//  //    e.foreach {
//  //      x => this.entries.addAll(x)
//  //    }
//  //  }
//  carryEntriesAction + Else(
//    Statement { (m: Message, a: Agent) =>
//      val mm = m.asInstanceOf[CarryEntries]
//      if (log) a.localState[Logger](LOGGER).info("carry entries for dead predecessor")
//      mm.e.foreach { x => a.localState[Entries](ENTRIES).addAll(x) }
//    }
//  )(ifStmt2_carryEntriesAction)
//
//  defaultBehavior += new Message("CarryEntries") -> carryEntriesAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class UploadEntries(e: Set[Entry]) extends Message("UploadEntries", Seq(e))
//  //  case UploadEntries(e)
//  //  =>
//  val uploadEntriesAction = new Action
//  //---------------------------------------------------------------------------------------------------
//  uploadEntriesAction + Statement { (m: Message, a: Agent) =>
//    //    entries.addAll(e)
//    a.localState[Entries](ENTRIES).addAll(m.payload[Set[Entry]](0))
//  }
//
//  defaultBehavior += new Message("UploadEntries") -> uploadEntriesAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class Response(code: String, e: Set[Entry]) extends Message("Response", Seq(code, e))
//  //  case Response(id, res)
//  //  =>
//  val responseAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  this.responses.receive(id, res)
//  responseAction + Statement { (m: Message, a: Agent) =>
//    val mm = m.asInstanceOf[Response]
//    a.localState[Responses](RESPONSES).receive(mm.code, mm.e)
//  }
//
//
//  defaultBehavior += new Message("Response") -> responseAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class Successor() extends Message("Successor")
//  //  case Successor()
//  //  =>
//  val successorAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  sender ! references.getSuccessor
//  successorAction + ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => a.localState[References](REFERENCES).getSuccessor)
//  successorAction + ModifyState(DST_AGENT, (m: Message, _: Agent) => m.sender)
//  successorAction + Send(MSG_TO_SEND, DST_AGENT)
//
//  defaultBehavior += new Message("Successor") -> successorAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class RequestPredecessor() extends Message("RequestPredecessor")
//  //  case RequestPredecessor()
//  //  =>
//  val requestPredecessorAction = new Action
//  //---------------------------------------------------------------------------------------------------
//  //  sender ! references.getPredecessor
//
//  requestPredecessorAction + ModifyState(MSG_TO_SEND, (_: Message, a: Agent) => a.localState[References](REFERENCES).getPredecessor)
//  requestPredecessorAction + ModifyState(DST_AGENT, (m: Message, _: Agent) => m.sender)
//  requestPredecessorAction + Send(MSG_TO_SEND, DST_AGENT)
//
//  defaultBehavior += new Message("RequestPredecessor") -> requestPredecessorAction
//  //---------------------------------------------------------------------------------------------------
//  //  case class Live() extends Message("Live")
//  //  case Live()
//  //  =>
//  val liveAction = new Action
//  //---------------------------------------------------------------------------------------------------
//
//  //  sender ! "yes"
//  liveAction + ModifyState(MSG_TO_SEND, (_: Message, _: Agent) => "yes")
//  liveAction + ModifyState(DST_AGENT, (m: Message, _: Agent) => m.sender)
//  liveAction + Send(MSG_TO_SEND, DST_AGENT)
//
//  defaultBehavior += new Message("Live") -> liveAction
//
//  //  }
//
//  specialReactions(new StartMSG) + Statement{(m: Message, a:Agent) =>
//    /*
//    - Send "Join" to next agent in round robin, and send update predecessor (check my-notes.org)
//    - will this also update the predecessor? --> check the src code
//    -
//     */
//  } // Statement
//
//}