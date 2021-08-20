//package benchmarks.openchord.service
//
////import akka.remote.testconductor.Server
//
//import java.io.Serializable
//import java.util.logging.{FileHandler, Logger, SimpleFormatter}
//
//import benchmarks.openchord.data._
//
///**
// * This is a simple tester class for Akka Actor implementation of the DHT system. New DHT learners can get closer to
// * how the DHT system works by run this Tester. when first run this class, decide if you want to test if with multiple
// * computers via Internet or just use the local host.
// * To have better performance, I suggest that users use different computers, for me, I use 3 computers, and run 2 chords
// * in each one. One thing should be noted that when multi-chords are run in a single computers, please make sure that
// * each one use a different port number(set in the chord.config file). And when a ChordImpl object is created, specify
// * a different system name and actor name and the selected port number. e.g: val chord = new ChordImpl(sys, actor, port)
// * After a new chord is created, you can join in a existed system by typing 'join' from the console and then enter the
// * actor system name, host name, port number, actor name of the remote chord's url from console.
// * After joining a network system, the current chord maybe assigned some payload by its predecessor, this can be checked
// * out by looking up the entries table by typing 'entries' from console. Other functionality of the chords can be tested
// * by applying the command names from the console, e.g 'upload' is to upload key-value pairs into the system.
// *
// * @author zepeng zhao
// */
//
//
//object Main extends App {
//  override def main(args: Array[String]) {
//    var logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//    logger.setUseParentHandlers(false);
//    var fileTxt = new FileHandler("log.txt");
//    var formatterTxt = new SimpleFormatter();
//    fileTxt.setFormatter(formatterTxt);
//    logger.addHandler(fileTxt);
//    print("Actor System Name:")
//    var asn = readLine()
//    print("Actor Name:")
//    var an = readLine()
//    var p: Int = 0
//    while (p == 0) {
//      print("Port Number:")
//      try {
//        p = readLine().toInt
//      }
//      catch {
//        case e: Exception => println("You should input a number as port number."); p = 0
//      }
//    }
//
//    val chord = new ChordImpl(asn, an, p)
//    while (true) {
//      try {
//        print(">")
//        var ln = readLine()
//        //eg. akka.tcp://RemoteSystem@192.168.1.14:2015/user/server
//        if (ln.startsWith("join")) {
//          var url = "akka.tcp://"
//          print("Actor System Name:")
//          url += readLine().trim()
//          print("Host Name:")
//          url += "@" + readLine().trim()
//          print("Port Number:")
//          url += ":" + readLine().trim()
//          url += "/user/"
//          print("Actor Name:")
//          url += readLine().trim()
//          val id = HashFunction.createID(url.getBytes)
//          val node = new Node(id, url)
//          chord.join(node)
//        }
//        else if (ln.trim().equals("FT"))
//          chord.printF()
//        else if (ln.trim().equals("SL"))
//          chord.printSL()
//        else if (ln.trim().equals("PRE"))
//          chord.print_pre()
//        else if (ln.trim().equals("SUC"))
//          chord.printS()
//        else if (ln.trim().equals("entries"))
//          chord.print_entries()
//        else if (ln.trim().equals("upload")) {
//          print("Key:")
//          var k = readLine()
//          print("value:")
//          val v: Serializable = readLine().asInstanceOf[java.io.Serializable]
//          var id = HashFunction.createID(k.getBytes)
//          var e = new Entry(id, k, v)
//          chord.upload(e)
//        }
//        else if (ln.trim().equals("lookup")) {
//          print("Key:")
//          var key = readLine()
//          var id = HashFunction.createID(key.getBytes)
//          var res = chord.lookup(id)
//          if (res.isEmpty)
//            println("Value not found")
//          else
//            res.foreach { x => println(x.getValue()) }
//        }
//      }
//      catch {
//        case e: Exception => println(e.getMessage)
//      }
//    }
//  }
//}