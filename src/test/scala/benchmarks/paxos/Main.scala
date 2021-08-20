//package benchmarks.paxos
//
//import java.util.logging.{FileHandler, Logger, SimpleFormatter}
//
//import akka.actor.{ActorSystem, Props, actorRef2Scala}
//
//object Main extends App {
//  override def main(args: Array[String]) {
//    val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//    logger.setUseParentHandlers(false);
//    val fileTxt = new FileHandler("log_4.txt");
//    val formatterTxt = new SimpleFormatter();
//    fileTxt.setFormatter(formatterTxt);
//    logger.addHandler(fileTxt);
//
//    val system = ActorSystem("RemoteSystem")
//    var id = -1
//
//    try {
//      val m = Util.loadPaxos()
//      while (id < 0) {
//        try {
//          print("Select node id:")
//          id = readLine().toInt
//          if (!(id > 0 && id <= m.size)) {
//            println("id should be a number between 1 and " + m.size)
//            id = -1
//          }
//        }
//        catch {
//          case e: Exception => println("nid should be a number between 1 and " + m.size); id = -1
//        }
//      }
//      val master = system.actorOf(Props(classOf[PaxosActor], m, id), "Paxos")
//      while (true) {
//        print("->")
//        val command = readLine()
//        if (command.equals("propose")) {
//          print("value:")
//          val v = readLine().asInstanceOf[java.io.Serializable]
//          print("node:")
//          try {
//            val nid = readLine().toInt
//            if (nid > 0 && nid <= m.size)
//              master ! Propose(v, nid)
//            else
//              println("nid should be a number between 1 and " + m.size)
//          } catch {
//            case e: Exception => println("nid should be a number between 1 and " + m.size)
//          }
//        }
//        else if (command.equals("PL")) {
//          master ! PrintLogs()
//        }
//
//      }
//    }
//    catch {
//      case e: Exception => println(e.getMessage)
//    }
//  }
//
//}