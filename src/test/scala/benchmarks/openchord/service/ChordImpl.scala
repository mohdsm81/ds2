//package benchmarks.openchord.service
//
//import java.net.NetworkInterface
//import java.util.concurrent.TimeUnit
//import java.util.logging.{Level, Logger}
//
//import akka.actor.ActorSelection.toScala
//import akka.actor.{ActorSelection, ActorSystem, Props, actorRef2Scala}
//import akka.pattern.ask
//import akka.util.Timeout
//import benchmarks.openchord.data._
//import com.typesafe.config.ConfigFactory
//
//import scala.concurrent.Await
//import scala.concurrent.duration.Duration
//
//
//
//
//
///**
// * This class works together with the Chord_Actor class to implement the chord functionality.
// * When a user create an ChordImpl instance, an actor system is built which will spawn a remote chord actor, which
// * is associated with the current node's ip address and a specified port number that a user wrote to the chord.config
// * file. user may need to specify the ip address in the chord.config if they want to connect nodes via internet.
// * Users can define the name of the actor system and the name of the chord actor from the constructor, default system
// * name is 'RemoteSystem', default actor name is 'Master', the default port number is 2015.
// *
// * @author zepeng zhao
// */
//class ChordImpl(val system_name:String="RemoteSystem",val actor_name:String="Master",val port:Int = 2015) {
//
//  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
////  private val configFile = getClass.getClassLoader.getResource("chord.config").getFile
////  private val config = ConfigFactory.parseFile(new File(configFile))
//  private val entries = new Entries()
//  private val backupEntries = new BackupEntries()
//  private val responses = new Responses()
//  private var counter:Int = 0
//
//  //retreive the internet ip address
//  private var net_ip:String = null
//  val interfaces = NetworkInterface.getNetworkInterfaces
//  while (interfaces.hasMoreElements) {
//    val element = interfaces.nextElement
//     //println(element.getDisplayName)
//    if (element.getDisplayName.equalsIgnoreCase("en0")||element.getDisplayName.equalsIgnoreCase("wlan0")){
//      //println(element.getDisplayName +":"+element.getInterfaceAddresses.get(1).toString())
//      net_ip = ((element.getInterfaceAddresses.get(1).toString()).split("/"))(1)
//      //println(net_ip)
//    }
//  }
//  if(net_ip == null){
//    net_ip = "127.0.0.1"
//    logger.info("Didn't connect to the internet, using local ip address:127.0.0.1")
//  }
// // private val config =
//  private val system = ActorSystem(system_name , ConfigFactory.load(ConfigLoader.load(net_ip,port)))
//  //e.g: "akka.tcp://RemoteSystem@192.168.1.14:2015/user/server"
//  private val localURL = "akka.tcp://"+system_name+"@"+net_ip+":"+port+"/user/"+actor_name
//  println(localURL)
//  private val nodeID = HashFunction.createID(localURL.getBytes)
//  private val localNode = new Node(nodeID,localURL)
//  private val successorList = new SuccessorList(localNode)
//  private val fingerTable = new FingerTable(nodeID, localNode,successorList)
//  private val references = new References(localNode,fingerTable,successorList,localNode)
//  implicit val timeout = Timeout(50000)
//
//
//  private val master =system.actorOf(Props(classOf[Chord_Actor],localNode,references,entries,responses,backupEntries),name = actor_name)
//
//
//  //val initialDelay = Duration(50, TimeUnit.SECONDS)
//  val interval = Duration(100, TimeUnit.SECONDS)
//  val interval1 = Duration(15, TimeUnit.SECONDS)
//  implicit val executor = system.dispatcher
//  system.scheduler.schedule(Duration(55, TimeUnit.SECONDS), interval, new Runnable{def run(){loadBalance()}})
//  system.scheduler.schedule(Duration(50, TimeUnit.SECONDS), interval, new Runnable{def run(){stabilize()}})
//  system.scheduler.schedule(Duration(120, TimeUnit.SECONDS), Duration(100, TimeUnit.SECONDS), new Runnable{def run(){fix_fingers()}})
//  system.scheduler.schedule(Duration(40, TimeUnit.SECONDS), interval1, new Runnable{def run(){checkPredecessor()}})
//  system.scheduler.schedule(Duration(40, TimeUnit.SECONDS), interval1, new Runnable{def run(){checkSuccessor()}})
//  system.scheduler.schedule(Duration(80, TimeUnit.SECONDS), interval, new Runnable{def run(){examineReferences()}})
//
//
//  /**
//   * Joining a network by calling this function.
//   * @arg: node:Node, join a network from the provide node
//   */
//  def join(node:Node){
//    try{
//      if(node!=null && !node.equals(localNode)){
//        logger.info("Start trying to join a network from:["+node.getURL+"]")
//
//        if(this.ping(node)){
//          var t = system.actorSelection(node.getURL)
//          t ! find_successor(localNode,localNode.getID)
//          //inform other nodes to reconfigure their fingerTable or successorList accordingly
//           new Thread(new Runnable{def run{update_others()}}).start()
//        }
//        else
//          logger.info("The node you try to join from is not alive")
//      }
//      else
//        logger.info("you may not try to join your own network")
//
//    }
//    catch{
//      case e:Exception => logger.log(Level.SEVERE, "Exception thrown while trying to join", e)
//    }
//  }
//
//
// /**
//  * to initialize finger table, not used in practice
//  */
//  def init_finger_table(a:ActorSelection){
//    for(i <- 0 until nodeID.getLength()){
//        Thread.sleep(200)
//        try{
//          var finger = nodeID.addPowerofTwo(i)
//          a ! find_successor(localNode,finger)
//        }
//        catch{
//          case e:Exception => println(e.getMessage)
//        }
//    }
//  }
//
//
//  /**
//   * leave the network, not used
//   */
//  def leaveNetwork(){
//
//    this.passEntriesToSuccessor()
//
//    Thread.sleep(20)
//
//  }
//
//  /**
//   * not used
//   */
//  def passEntriesToSuccessor(){
//   var temp = system.actorSelection(references.getSuccessor.getURL)
//
//   println("Passing entries to:"+references.getSuccessor.getURL)
//
//   var values = entries.getValues
//
//   values.foreach { x => temp ! uploadEntries(x)  }
//  }
//
//
//  /**
//   * waiting until the predecessor is retrieved, pass the node to inform the others
//   */
//  def update_others(){
//    logger.info("Joining a network, waiting for predecessor.")
//    while(references.getPredecessor == null || references.getPredecessor.equals(localNode)){
//      Thread.sleep(10)
//    }
//    logger.info("Starting informing predecessors.")
//    system.actorSelection(references.getPredecessor.getURL) ! update_finger_table(localNode,1)
//  }
//
//  /**
//   * Periodically check and update the predecessor and successor by calling this function
//   * if the predecessor is dead, remove it and inherit its entries
//   * if successor is alive, and entries is renewed, back up entries to successor.
//   */
//  def checkPredecessor(){
//    var pre = references.getPredecessor
//    var suc = references.getSuccessor
//    //check predecessor, if there exists one
//    if(pre != null && !pre.equals(localNode)){
//       logger.info("Checking the liveness of predecessor.")
//       //if predecessor is no more alive, and there exists backup entries for it, carry the backup entries for it.
//      if(!ping(pre)){
//          logger.info("Node["+pre.getURL+"] is dead removed it from references")
//          this.references.removeReference(pre)
//          logger.info("remove backupEntries for dead Node["+pre.getURL+"] and transmit it to its successor")
//          var backup = this.backupEntries.remove(pre)
//           if(backup != null)
//              master ! carryEntries(pre,backup)
//       }
//     }
//
//  }
//
//  /**
//   * if successor is alive and there exists new entries, ask successor to back up entries
//   */
//  def checkSuccessor(){
//    var suc = references.getSuccessor
//    var suc_live = ping(suc)
//    if(suc!=null && suc_live && entries.getStatus){
//      var backup = this.entries.getValues
//      entries.resetStatus()
//      if(backup != null){
//        system.actorSelection(suc.getURL) ! transmitBackupEntries(this.localNode, backup)
//        logger.info("transmit backup to successor")
//      }
//    }else if(suc!=null && !suc_live){
//      logger.info("successor is dead, remove it from references table.")
//      references.removeReference(suc)
//      if(!entries.getStatus)
//        entries.resetStatus()
//    }
//  }
//
//  /**
//   * check a node's liveness by sending a message and waiting for a future
//   * if the future times out, consider the node as dead node, return false
//   * otherwise, return true.
//   */
//  def ping(toPing:Node):Boolean={
//      var alive = true
//      try{
//        logger.info("ping node["+toPing.getURL+"]")
//        val f = system.actorSelection(toPing.getURL) ? live()
//        val re =  Await.result(f,Duration.create(5, "seconds"))
//      }
//      catch{
//        case e:Exception => {
//          if(toPing != null){
//            logger.log(Level.SEVERE, "Node ["+toPing.getURL+"] fail", e)
//            alive = false
//          }
//        }
//      }
//      alive
//  }
//
//  /**
//   * Periodically examing the liveness of nodes from reference.
//   * and rip off dead nodes if any is found
//   */
//  def examineReferences(){
//    var suc = references.getSuccessor
//    var temp1 = successorList.getCopy.toSet
//
//    temp1 ++= this.fingerTable.getFingers
//
//    temp1.foreach { x => if(x!=null && !x.equals(suc)) new Thread(new Runnable{def run{TestAndRemoveNode(x)}}).start }
//
//  }
//
//  /**
//   * provided a target node from references, test the liveness of the target
//   * if target is dead, rip it off the reference table
//   */
//  private def TestAndRemoveNode(target:Node){
//    if(!ping(target)){
//      logger.info("Node["+target.getURL+"] is dead removed it from references")
//      this.references.removeReference(target)
//      logger.info("remove backupEntries for dead Node["+target.getURL+"] and transmit it to its successor")
//      var backup = this.backupEntries.remove(target)
//      var succ = this.successorList.getImmediateSuccessor(target.getID)
//      var lastS = this.successorList.getLast()
//      if(backup != null && succ != null )
//        system.actorSelection(succ.getURL) ! carryEntries(target,backup)
//      else if(backup != null && lastS !=null){
//         system.actorSelection(lastS.getURL) ! carryEntries(target,backup)
//      }else if(backup != null)
//        master ! carryEntries(target,backup)
//    }
//  }
//
//
//  /**
//   * periodically stabilize the reference table. mainly make sure that the local node id is between
//   * that of the successor's and the predecessor's.
//   */
//  def stabilize(){
//      var s = references.getSuccessor
//      if(s!=null && !s.equals(localNode)){
//        try{
//          var f = system.actorSelection(s.getURL) ? requestAll()
//           Await.result(f, Duration.create(5, "seconds")) match{
//            case re:List[Node]=>{
//               logger.info("receive list of nodes from successor["+s.getURL+"]")
//               re.foreach { x =>
//                 new Thread(new Runnable{def run{TestAndAddNode(x)}}).start()
//              }
//               var nl = List(localNode)
//               Thread.sleep(1000)
//               logger.info("sending predecessor and localNode to successor")
//               if(references.getPredecessor !=null)
//                 nl=nl:+references.getPredecessor
//               system.actorSelection(s.getURL) ! recSuccessorList(nl)
//            }
//          }
//        }
//        catch{
//          case e:Exception => {
//            logger.log(Level.SEVERE, "successor ["+s.getURL+"] fail,remove successor",e)
//            references.removeReference(s)
//          }
//        }
//      }else{
//        logger.info("Doesn't need to be stablized, successor doesn't exist")
//      }
//  }
//
//  /**
//   * test the liveness of a target node, if target is alive add it to reference
//   */
//  private def TestAndAddNode(target:Node){
//    if(ping(target)){
//      logger.info("node ["+target.getURL+"] pass ping, add it to references")
//      references.addReference(target)
//    }else
//      logger.info("node["+target.getURL+"] fail ping, drop it")
//
//  }
//
//
//  /**
//   * periodically call this function to update the finger table
//   */
//  def fix_fingers(){
//    val r = new scala.util.Random(System.currentTimeMillis())
//    val num = r.nextInt(nodeID.getLength())
//    try{
//      val start = nodeID.addPowerofTwo(num)
//      if(start !=null )
//        master ! find_successor(localNode,start)
//    }
//    catch{
//      case e:Exception => logger.log(Level.SEVERE, "", e)
//    }
//  }
//
//  /**
//   * Periodically balance the loads of the entries table in case that new nodes come in
//   * the new node can carry some of the payloads
//   */
//  def loadBalance(){
//    var keys = entries.getKeys
//    var succ = this.successorList.getSuccessor()
//    keys.foreach { x =>
//        if(succ != null && !succ.equals(localNode) && x.isInInterval(localNode.getID, succ.getID)){
//          var toT = entries.removeID(x)
//          toT.foreach { x1 =>system.actorSelection(succ.getURL) ! uploadEntry(x1) }
//      }
//    }
//  }
//
//  /**
//   * providing a key and lookup the corresponded entry set
//   */
//  def lookup(key:ID):Set[Entry]={
//    var reqID = this.makeRequestID()
//    val req = new Request(this.localNode, reqID, key)
//    master ! request(req)
//    var t = 0
//    while(!responses.containsKey(reqID)){
//      if(t > 500)
//        return Set()
//      t+=1
//      Thread.sleep(2)
//    }
//    responses.remove(reqID)
//  }
//
//
//  private def makeRequestID():String = System.currentTimeMillis().toString() +"_"+counter
//
//
//
//  /**
//   * find a node that is responsible for an entry and upload the entry to this node
//   */
//  def upload(toUpload:Entry){
//    var entryID = toUpload.getID()
//    var cp = references.getClosestPrecedingNode(entryID)
//    var pre  = references.getPredecessor
//    if(pre != null && entryID.isInInterval(pre.getID, this.localNode.getID)){
//      logger.info("Entry with ["+toUpload.getKey().toString()+"] is uploaded to the current node")
//      entries.add(toUpload)
//    }
//    else if(cp != null && !cp.equals(localNode)){
//      logger.info("find key["+toUpload.getKey()+"]'s closest predecessor, pass the upload task to it")
//      system.actorSelection(cp.getURL) ! uploadEntry(toUpload)
//    }
//    else if(references.getSuccessor != null){
//      logger.info("pass the upload task to successor")
//      system.actorSelection(references.getSuccessor.getURL) ! uploadEntry(toUpload)
//    }
//    else{
//      entries.add(toUpload)
//      logger.info("Entry with ["+toUpload.getKey().toString()+"] is uploaded to the current node")
//    }
//  }
//
//
//  /**********************serials of functions for user testing from console************************/
//  def printF(){
//    fingerTable.print()
//  }
//
//  def printS(){
//    if(references.getSuccessor != null)
//      println(references.getSuccessor.getURL)
//  }
//
//  def print_pre(){
//    println(references.getPredecessor.getURL)
//  }
//
//  def print_entries(){
//    var temp = entries.getValues
//    temp.foreach { x => x.foreach { y => println("key:"+y.getKey()+", value:"+y.getValue()) }}
//  }
//
//  def printSL(){
//    this.successorList.getCopy.foreach { x => println(x.getURL) }
//  }
//
//
//
//
//}