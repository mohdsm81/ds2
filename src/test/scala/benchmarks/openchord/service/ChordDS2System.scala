package benchmarks.openchord.service

import benchmarks.openchord.data._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem, LocalState}
import edu.utah.cs.gauss.serialization.IO._

import java.io.File


/**
 *
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *
 *         based on the Akka implementation done by Zepeng Zhao.
 */

object ChordDS2System {

  def getInstance(numOfAgents: Int = 3, harnessFilePath: String, log: Boolean = false): (DistributedSystem, File) = {
    val systemName = "OpenChord"
    val ds = new DistributedSystem(systemName)
    (1 to numOfAgents) foreach { id =>
      ds + createAgent(id.toString, log)
    }

    ds.refresh
    (ds, createHarnessFile(harnessFilePath,ds))
  } // getInstance

  private def createAgent(agentName: String = "oc", log: Boolean = false): Agent = {


    //========================================================================
    //     How to generate a URL, ID, and other supporting datastructures
    //            --------- Initializing all fields ------------
    //========================================================================
    val entries: Entries = new Entries() // this is the initial value for the SPEC in the WGL[2] alg. Map.empty[Any,Any]
    val backupEntries: BackupEntries = new BackupEntries()
    val responses: Responses = new Responses()
    val localURL: String = agentName
    val nodeID: ID = HashFunction.createID(localURL.getBytes)
    val localNode: Node = new Node(nodeID, localURL)
    val successorList: SuccessorList = new SuccessorList(localNode)
    val fingerTable: FingerTable = new FingerTable(nodeID, localNode, successorList)
    val references: References = new References(localNode, fingerTable, successorList, localNode)


    //------------------------------------------------------------------------
    // Creating the agent
    //------------------------------------------------------------------------
    val agent = new ChordDS2Agent3(
      localURL,
      localNode,
      references,
      entries,
      responses,
      backupEntries,
      log)

    agent
  } // createAgent

  private def createHarnessFile(filePath: String, ds: DistributedSystem): File = {
    /*
    NOTES: messages of interest (internal messaging with invocations
    UploadEntry => key index = 0 // write request
    UploadEntryResponse => key index = ??? // write request response

    Request => key index = 0 // read request
    Response => key index = 0 // Request Response

    Client Responses aren't needed here (acks to clients), just to complete the history.
     */

    if (new File(filePath).exists()) deleteFile(filePath)

    val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"

    val writeMsgName = "UploadEntry"
    val writeAckMsgName = "WriteResponse"
    val readMsgName = "Request"
    val readAckMsgName = "ReadResponse"
    val ired = new Agent("IRed")

    appendToFile(filePath,
      SPEC, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(filePath, "") // newline
    appendToFile(filePath, """\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(filePath, "") // newline
    appendSeqToFile(filePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x =>
      if(x.name == "1") x + ", "
      else x + s", ${x.name.toInt - 1}"
    }.toSeq)
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      s"write, $writeMsgName, k1, 10", // add operations here (harness)
      s"read, $readMsgName, k1",
      s"read, $readMsgName, k1",
      s"write, $writeMsgName, k2, 20",
      s"read, $readMsgName, k2",
      s"read, $readMsgName, k2")
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      "read" + ", " + readAckMsgName, // add response messages regexes
      "write" + ", " + writeAckMsgName)
    appendToFile(filePath, "") // newline
     /*
     This part is special to LiViola, it focuses on shuffling what is mentioned here and leaves others to happen in
     whatever order they happen in, so be VERY CAREFUL what you put/leave-out of this. Other schedules simply ignore
     this part.
      */
    appendToFile(filePath,
      // write-transaction-related messaging
     "UploadEntry, 0, w",
      "UploadEntryResponse, 0, w",
      // read-transaction-related messaging
      "Request, 0, r",
      "Response, 0, r"
    ) // newline

    new File(filePath)
  }

} // object


/* =========================================================================================
 *            Utility Functions: to manipulate agents by TimedActions
 *    HINT: this contains hints on how to make a harness!!! (along with console commands)
 * ========================================================================================
 */
//    /**
//     * Joining a network by calling this function.
//     *
//     * @param node join a network from the provide node
//     */
//    def join(node: Node) {
//      try {
//        if (node != null && !node.equals(localNode)) {
//          logger.info("Start trying to join a network from:[" + node.getURL + "]")
//
//          if (this.ping(node)) {
//            val t = system.actorSelection(node.getURL)
//            t ! FindSuccessor(localNode, localNode.getID)
//            //inform other nodes to reconfigure their fingerTable or successorList accordingly
//            new Thread(new Runnable {
//              def run() {
//                update_others()
//              }
//            }).start()
//          }
//          else
//            logger.info("The node you try to join from is not alive")
//        }
//        else
//          logger.info("you may not try to join your own network")
//
//      }
//      catch {
//        case e: Exception => logger.log(Level.SEVERE, "Exception thrown while trying to join", e)
//      }
//    }
//
//
//    /**
//     * to initialize finger table, not used in practice
//     */
//    def init_finger_table(a: ActorSelection) {
//      for (i <- 0 until nodeID.getLength()) {
//        Thread.sleep(200)
//        try {
//          val finger = nodeID.addPowerOfTwo(i)
//          a ! FindSuccessor(localNode, finger)
//        }
//        catch {
//          case e: Exception => println(e.getMessage)
//        }
//      }
//    }
//
//
//    /**
//     * leave the network, not used
//     */
//    def leaveNetwork() {
//
//      this.passEntriesToSuccessor()
//
//      Thread.sleep(20)
//
//    }
//
//    /**
//     * not used
//     */
//    def passEntriesToSuccessor() {
//      val temp = system.actorSelection(references.getSuccessor.getURL)
//
//      println("Passing entries to:" + references.getSuccessor.getURL)
//
//      val values = entries.getValues
//
//      values.foreach { x => temp ! UploadEntries(x) }
//    }
//
//
//    /**
//     * waiting until the predecessor is retrieved, pass the node to inform the others
//     */
//    def update_others() {
//      logger.info("Joining a network, waiting for predecessor.")
//      while (references.getPredecessor == null || references.getPredecessor.equals(localNode)) {
//        Thread.sleep(10)
//      }
//      logger.info("Starting informing predecessors.")
//      system.actorSelection(references.getPredecessor.getURL) ! UpdateFingerTable(localNode, 1)
//    }
//
//    /**
//     * Periodically check and update the predecessor and successor by calling this function
//     * if the predecessor is dead, remove it and inherit its entries
//     * if successor is alive, and entries is renewed, back up entries to successor.
//     */
//    def checkPredecessor() {
//      val pre = references.getPredecessor
//      //check predecessor, if there exists one
//      if (pre != null && !pre.equals(localNode)) {
//        logger.info("Checking the live-ness of predecessor.")
//        //if predecessor is no more alive, and there exists backup entries for it, carry the backup entries for it.
//        if (!ping(pre)) {
//          logger.info("Node[" + pre.getURL + "] is dead removed it from references")
//          this.references.removeReference(pre)
//          logger.info("remove backupEntries for dead Node[" + pre.getURL + "] and transmit it to its successor")
//          val backup = this.backupEntries.remove(pre)
//          if (backup != null)
//            master ! CarryEntries(pre, backup)
//        }
//      }
//
//    }
//
//    /**
//     * if successor is alive and there exists new entries, ask successor to back up entries
//     */
//    def checkSuccessor() {
//      val suc = references.getSuccessor
//      val suc_live = ping(suc)
//      if (suc != null && suc_live && entries.getStatus) {
//        val backup = this.entries.getValues
//        entries.resetStatus()
//        if (backup != null) {
//          system.actorSelection(suc.getURL) ! TransmitBackupEntries(this.localNode, backup)
//          logger.info("transmit backup to successor")
//        }
//      } else if (suc != null && !suc_live) {
//        logger.info("successor is dead, remove it from references table.")
//        references.removeReference(suc)
//        if (!entries.getStatus)
//          entries.resetStatus()
//      }
//    }
//
//    /**
//     * check a node's live-ness by sending a message and waiting for a future
//     * if the future times out, consider the node as dead node, return false
//     * otherwise, return true.
//     */
//    def ping(toPing: Node): Boolean = {
//      var alive = true
//      try {
//        logger.info("ping node[" + toPing.getURL + "]")
//        val f = system.actorSelection(toPing.getURL) ? Live()
//        //        val re =
//        Await.result(f, Duration.create(5, "seconds"))
//      }
//      catch {
//        case e: Exception =>
//          if (toPing != null) {
//            logger.log(Level.SEVERE, "Node [" + toPing.getURL + "] fail", e)
//            alive = false
//          }
//      }
//      alive
//    }
//
//    /**
//     * Periodically examining the live-ness of nodes from reference.
//     * and rip off dead nodes if any is found
//     */
//    def examineReferences() {
//      val suc = references.getSuccessor
//      var temp1 = successorList.getCopy.toSet
//
//      temp1 ++= this.fingerTable.getFingers
//
//      temp1.foreach { x =>
//        if (x != null && !x.equals(suc)) new Thread(new Runnable {
//          def run() {
//            TestAndRemoveNode(x)
//          }
//        }).start()
//      }
//
//    }
//
//    /**
//     * provided a target node from references, test the live-ness of the target
//     * if target is dead, rip it off the reference table
//     */
//    private def TestAndRemoveNode(target: Node) {
//      if (!ping(target)) {
//        logger.info("Node[" + target.getURL + "] is dead removed it from references")
//        this.references.removeReference(target)
//        logger.info("remove backupEntries for dead Node[" + target.getURL + "] and transmit it to its successor")
//        val backup = this.backupEntries.remove(target)
//        val succ = this.successorList.getImmediateSuccessor(target.getID)
//        val lastS = this.successorList.getLast()
//        if (backup != null && succ != null)
//          system.actorSelection(succ.getURL) ! CarryEntries(target, backup)
//        else if (backup != null && lastS != null) {
//          system.actorSelection(lastS.getURL) ! CarryEntries(target, backup)
//        } else if (backup != null)
//          master ! CarryEntries(target, backup)
//      }
//    }
//
//
//    /**
//     * periodically stabilize the reference table. mainly make sure that the local node id is between
//     * that of the successor's and the predecessor's.
//     */
//    def stabilize() {
//      val s = references.getSuccessor
//      if (s != null && !s.equals(localNode)) {
//        try {
//          val f = system.actorSelection(s.getURL) ? RequestAll()
//          Await.result(f, Duration.create(5, "seconds")) match {
//            case re: List[Node] =>
//              logger.info("receive list of nodes from successor[" + s.getURL + "]")
//              re.foreach { x =>
//                new Thread(new Runnable {
//                  def run() {
//                    TestAndAddNode(x)
//                  }
//                }).start()
//              }
//              var nl = List(localNode)
//              Thread.sleep(1000)
//              logger.info("sending predecessor and localNode to successor")
//              if (references.getPredecessor != null)
//                nl = nl :+ references.getPredecessor
//              system.actorSelection(s.getURL) ! RecSuccessorList(nl)
//          }
//        }
//        catch {
//          case e: Exception =>
//            logger.log(Level.SEVERE, "successor [" + s.getURL + "] fail,remove successor", e)
//            references.removeReference(s)
//        }
//      } else {
//        logger.info("Doesn't need to be stabilized, successor doesn't exist")
//      }
//    }
//
//    /**
//     * test the live-ness of a target node, if target is alive add it to reference
//     */
//    private def TestAndAddNode(target: Node) {
//      if (ping(target)) {
//        logger.info("node [" + target.getURL + "] pass ping, add it to references")
//        references.addReference(target)
//      } else
//        logger.info("node[" + target.getURL + "] fail ping, drop it")
//
//    }
//
//
//    /**
//     * periodically call this function to update the finger table
//     */
//    def fix_fingers() {
//      val r = new scala.util.Random(System.currentTimeMillis())
//      val num = r.nextInt(nodeID.getLength())
//      try {
//        val start = nodeID.addPowerOfTwo(num)
//        if (start != null)
//          master ! FindSuccessor(localNode, start)
//      }
//      catch {
//        case e: Exception => logger.log(Level.SEVERE, "", e)
//      }
//    }
//
//    /**
//     * Periodically balance the loads of the entries table in case that new nodes come in
//     * the new node can carry some of the payloads
//     */
//    def loadBalance() {
//      val keys = entries.getKeys
//      val succ = this.successorList.getSuccessor()
//      keys.foreach { x =>
//        if (succ != null && succ != localNode && x.isInInterval(localNode.getID, succ.getID)) {
//          val toT = entries.removeID(x)
//          toT.foreach { x1 => system.actorSelection(succ.getURL) ! UploadEntry(x1) }
//        }
//      }
//    }
//
//    /**
//     * providing a key and lookup the corresponded entry set
//     */
//    def lookup(key: ID): Set[Entry] = {
//      val reqID = this.makeRequestID()
//      val req = new Request(this.localNode, reqID, key)
//      master ! ThisRequest(req)
//      var t = 0
//      while (!responses.containsKey(reqID)) {
//        if (t > 500)
//          return Set()
//        t += 1
//        Thread.sleep(2)
//      }
//      responses.remove(reqID)
//    }
//
//
//    def makeRequestID(): String = System.currentTimeMillis().toString + "_" + counter
//
//
//    /**
//     * find a node that is responsible for an entry and upload the entry to this node
//     */
//    def upload(toUpload: Entry) {
//      val entryID = toUpload.getID()
//      val cp = references.getClosestPrecedingNode(entryID)
//      val pre = references.getPredecessor
//      if (pre != null && entryID.isInInterval(pre.getID, this.localNode.getID)) {
//        logger.info("Entry with [" + toUpload.getKey() + "] is uploaded to the current node")
//        entries.add(toUpload)
//      }
//      else if (cp != null && !cp.equals(localNode)) {
//        logger.info("find key[" + toUpload.getKey() + "]'s closest predecessor, pass the upload task to it")
//        system.actorSelection(cp.getURL) ! UploadEntry(toUpload)
//      }
//      else if (references.getSuccessor != null) {
//        logger.info("pass the upload task to successor")
//        system.actorSelection(references.getSuccessor.getURL) ! UploadEntry(toUpload)
//      }
//      else {
//        entries.add(toUpload)
//        logger.info("Entry with [" + toUpload.getKey() + "] is uploaded to the current node")
//      }
//    }


/** ********************bench of functions for user testing from console*********************** */
/** This is really not needed here in DS2 model, I just left it for reference/comments it provides */
//    def printF() {
//      fingerTable.print()
//    }
//
//    def printS() {
//      if (references.getSuccessor != null)
//        println(references.getSuccessor.getURL)
//    }
//
//    def print_pre() {
//      println(references.getPredecessor.getURL)
//    }
//
//    def print_entries() {
//      val temp = entries.getValues
//      temp.foreach { x => x.foreach { y => println("key:" + y.getKey() + ", value:" + y.getValue()) } }
//    }
//
//    def printSL() {
//      this.successorList.getCopy.foreach { x => println(x.getURL) }
//    }


//================================================================================
//           Add these timed actions to the first Agent's Start action
//      Ryan: These are not necessary. Mo: then I won't add them.
//================================================================================
//val initialDelay = Duration(50, TimeUnit.SECONDS)
//    val interval: FiniteDuration = Duration(100, TimeUnit.SECONDS)
//    val interval: Int = 0
//    val interval1: FiniteDuration = Duration(15, TimeUnit.SECONDS)
//    val interval1: Int = 0

//    implicit val executor: ExecutionContextExecutor = system.dispatcher


//    system.scheduler.schedule(Duration(55, TimeUnit.SECONDS), interval, new Runnable {
//      def run() {
//        loadBalance()
//      }
//    })
//    system.scheduler.schedule(Duration(50, TimeUnit.SECONDS), interval, new Runnable {
//      def run() {
//        stabilize()
//      }
//    })
//    system.scheduler.schedule(Duration(120, TimeUnit.SECONDS), interval, new Runnable {
//      def run() {
//        fix_fingers()
//      }
//    })
//    system.scheduler.schedule(Duration(40, TimeUnit.SECONDS), interval1, new Runnable {
//      def run() {
//        checkPredecessor()
//      }
//    })
//    system.scheduler.schedule(Duration(40, TimeUnit.SECONDS), interval1, new Runnable {
//      def run() {
//        checkSuccessor()
//      }
//    })
//    system.scheduler.schedule(Duration(80, TimeUnit.SECONDS), interval, new Runnable {
//      def run() {
//        examineReferences()
//      }
//    })
