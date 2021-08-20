//package benchmarks.openchord.service
//
//import java.util.logging.Logger
//
//import akka.actor.Actor
//import akka.util.Timeout
//import benchmarks.openchord.data._
//
//
//case class request(req:Request)
//case class response(code:String, e:Set[Entry])
//case class find_successor(sn:Node,id:ID)
//case class successor()
//case class request_predecessor()
//case class closestPrecedingNode(id:ID)
//case class successor_found(s:Node)
//case class update_predecessor(pre:Node)
//case class update_finger_table(s:Node,i:Int)
//case class live()
//case class uploadEntry(e:Entry)
//case class uploadEntries(e:Set[Entry])
//case class recSuccessorList(sl:List[Node])
//case class requestAll()
//case class transmitBackupEntries(n:Node, e:List[Set[Entry]])
//case class carryEntries(n:Node,e:List[Set[Entry]])
//
///**
// * This class work represent the chord actor which acts as an remote endpoint to communicate with the remote chords
// * The class shares same objects with ChordImpl
// *
// * @author zepeng zhao
// */
//class Chord_Actor(val node:Node,val ref:References,val e:Entries,val resp:Responses,val be:BackupEntries) extends Actor{
//
//  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//  private val localNode = node
//  private val references = ref
//  private val entries = e
//  private val backupEntries = be
//  private val responses = resp
//  implicit val timeout = Timeout(5000)
//
//  override def receive: Receive = {
//
//    //once a new node join in a network, it updates the nodes preceding it through this case class
//    case update_finger_table(s,i)=>{
//      if(!(references.contains(s) && s.equals(this.localNode))|| i < 5){
//        logger.info("receive new node["+s.getURL+"]")
//        references.addReference(s)
//        if(i+1 < 50 && references.getPredecessor != null &&
//            !references.getPredecessor.equals(localNode)){
//          logger.info("inform predecessor about the new joining node["+s.getURL+"]")
//          context.actorSelection(references.getPredecessor.getURL) ! update_finger_table(s,i+1)
//        }
//      }else{
//        logger.info("Node ["+s.getURL+"] already exists")
//      }
//    }
//
//    //request all the node references that a nodes stores, including itself
//    case requestAll() =>{
//       logger.info("receive node list from ["+sender.path.toString()+"]")
//       var nl = references.getSuccessorList.getCopy
//       var pre = references.getPredecessor
//       if(pre != null)
//         nl = nl:+pre
//       nl = nl:+localNode
//       sender ! nl
//    }
//
//    //add successor to references table
//    case successor_found(s) => {
//      references.addReference(s)
//    }
//
//    //add successor list to references table one by one
//    case recSuccessorList(sl)=>{
//      sl.foreach { x => references.addReference(x) }
//    }
//
//
//    case update_predecessor(pre) =>{
//     references.updatePredecessor(pre)
//    }
//
//
//    /*
//     * find successor of id for remote node rn iteratively in the local finger table
//     * and recursively getting closer to the one that desired
//     * once reach the successor of id, return remote node rn with all the references that
//     * the successor has.
//     */
//    case find_successor(rn, id)=>{
//      //println("find_sucessor")
//      logger.info("find successors for ["+rn.getURL+"]")
//      if(rn !=null && id != null){
//        var c = references.getClosestPrecedingNode(id)
//        var s:Node = references.getSuccessor
//        if(s == null || s.equals(localNode)){
//          if(!rn.equals(localNode)){
//            logger.info("I am the only node in the network, send myself's info to node["+rn.getURL+"]")
//            context.actorSelection(rn.getURL) ! successor_found(localNode)
//            context.actorSelection(rn.getURL) ! update_predecessor(localNode)
//            references.addReference(rn)
//           }
//        }
//        else if(id.isInInterval(localNode.getID, s.getID)){
//            logger.info("I am the predecesor for node["+rn.getURL+"]")
//            context.actorSelection(rn.getURL) ! successor_found(s)
//            context.actorSelection(rn.getURL) ! update_predecessor(localNode)
//            references.addReference(rn)
//            var temp = references.getSuccessorList.getCopy
//            if(!temp.isEmpty)
//              context.actorSelection(rn.getURL) ! recSuccessorList(temp)
//        }
//        else{
//           if(c != null && !c.equals(localNode)){
//              context.actorSelection(c.getURL) ! find_successor(rn,id)
//              logger.info("find closest predecessor["+c.getURL+"] for node["+rn.getURL+"],passing task to it")
//           }
//           else{
//              logger.info("happens to be the successor of node["+rn.getURL+"]")
//              context.actorSelection(rn.getURL) ! successor_found(localNode)
//              context.actorSelection(rn.getURL) ! successor_found(this.references.getPredecessor)
//              var temp = references.getSuccessorList.getCopy
//              if(!temp.isEmpty)
//                context.actorSelection(rn.getURL) ! recSuccessorList(temp)
//          }
//        }
//      }
//    }
//
//    /*
//     * Recursively find the successor that is responsible for entry e and upload the entry e to this
//     * corresponding successor
//     */
//    case uploadEntry(e:Entry) =>{
//      var cp = references.getClosestPrecedingNode(e.getID())
//      var succ = references.getSuccessor
//      var pre = references.getPredecessor
//      if(pre != null && !pre.equals(localNode) && e.getID().isInInterval(pre.getID, localNode.getID)){
//        this.entries.add(e)
//        logger.info("Entry with ["+e.getKey().toString()+"] is uploaded to the current node")
//      }
//      else if(cp != null && !cp.equals(localNode)){
//        logger.info("find key["+e.getKey()+"]'s closest predecessor, pass the upload task to it")
//        context.actorSelection(cp.getURL) ! uploadEntry(e)
//      }
//      else if(succ != null){
//        context.actorSelection(succ.getURL) ! uploadEntry(e)
//        logger.info("pass the upload task to successor")
//      }
//      else{
//        this.entries.add(e)
//        logger.info("Entry with ["+e.getKey().toString()+"] is uploaded to the current node")
//      }
//    }
//
//    /**
//     * Recursively find the successor that is responsible for request req
//     * and then return the requested node with a response message.
//     */
//    case request(req) =>{
//      var key = req.reqKey
//      var cp = references.getClosestPrecedingNode(key)
//      var succ = references.getSuccessor
//      var pre = references.getPredecessor
//      if(pre != null && !pre.equals(localNode) && key.isInInterval(pre.getID, localNode.getID) )
//        context.actorSelection(req.reqNode.getURL) ! response(req.reqID, entries.getEntries(req.reqKey))
//      else if(succ != null && key.isInInterval(localNode.getID, succ.getID))
//        context.actorSelection(succ.getURL) ! request(req)
//      else if(cp != null && !cp.equals(localNode))
//        context.actorSelection(cp.getURL) ! request(req)
//      else if(succ != null)
//        context.actorSelection(succ.getURL) ! request(req)
//      else
//        context.actorSelection(req.reqNode.getURL) ! response(req.reqID, entries.getEntries(req.reqKey))
//    }
//
//    //n normally precedes localNode, back up entries for n.
//    case transmitBackupEntries(n, e) => {
//       logger.info("back up entries for predecessor["+n.getURL+"]")
//       backupEntries.setEntries(n, e)
//    }
//
//    //carry entries for node n, which most likely is predecessor of current node
//    case carryEntries(n,e) =>{
//      var succ = this.references.getSuccessor
//      if(succ!=null && !this.localNode.equals(succ) && n.getID.isInInterval(this.localNode.getID, succ.getID))
//        context.actorSelection(succ.getURL) ! carryEntries(n,e)
//      else{
//        logger.info("carry entries for dead predecessor")
//        e.foreach { x => this.entries.addAll(x) }
//      }
//
//    }
//
//    case uploadEntries(e) => entries.addAll(e)
//
//    case response(id,res) => this.responses.receive(id, res)
//
//    case successor() => sender ! references.getSuccessor
//
//    case request_predecessor() => sender ! references.getPredecessor
//
//    case live() => sender ! "yes"
//
//  }
//
//
//}