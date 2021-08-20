package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers.{Parser=> MyParser}
import edu.utah.cs.gauss.serialization.IO
import java.io.File
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
class ParserSpec extends MyTestSpecs {
  test("Cleaning + Parsing KV Log string"){
    val in =Seq(
      "Stupid line to mislead it :D",
      "INFO  jepsen.util - 6	:info	:cas	:timed-out",
      "INFO  jepsen.util - 3	:fail	:cas	[2 2]",
      "INFO  jepsen.util - 14	:invoke	:write	4",
      "INFO  jepsen.util - 0	:invoke	:cas	[4 3]",
      "INFO  jepsen.util - 0	:fail	:cas	[4 3]",
      "INFO  jepsen.util - 2	:invoke	:write	0",
      "INFO  jepsen.util - 11	:invoke	:read	nil",
      "INFO  jepsen.util - 11	:ok	:read	2",
      "INFO  jepsen.util - 2	:ok	:write	0",
      "INFO  jepsen.util - 3	:invoke	:write	2",
      "INFO  jepsen.util - 3	:ok	:write	2",
      "INFO  jepsen.util - 14	:info	:write	:timed-out",
      "INFO  jepsen.util - 0	:invoke	:write	1")

    val out =Seq(
      "6	:info	:cas	:timed-out",
      "3	:fail	:cas	[2 2]",
      "14	:invoke	:write	4",
      "0	:invoke	:cas	[4 3]",
      "0	:fail	:cas	[4 3]",
      "2	:invoke	:write	0",
      "11	:invoke	:read	nil",
      "11	:ok	:read	2",
      "2	:ok	:write	0",
      "3	:invoke	:write	2",
      "3	:ok	:write	2",
      "14	:info	:write	:timed-out",
      "0	:invoke	:write	1").map(_.trim)

    val testIn = MyParser.parseKVLog(in)
    val testOut = MyParser.parseKVLog(out)

    (testIn zip testOut).foreach{
      x =>
      assert(x._1.getPID == x._2.getPID &&
               x._1.getKey == x._2.getKey &&
               x._1.getOperation == x._2.getOperation &&
               x._1.getArgs == x._2.getArgs
      )
    }

    assert(testIn.size == testOut.size)
  }

  test("Cleaning + Parsing KV dirty Log string") {
    // val input = new File(System.getenv("HOME")+"/workspace/ds2/data/etcd_000.log")
    // val lines = Source.fromFile(input).getLines.toSeq


    val lines = Seq(
      "      ", // yes garbage to trip it
      "lein test jepsen.system.etcd-test",
      "INFO  jepsen.os.debian - :n3 setting up debian",
      "INFO  jepsen.os.debian - :n4 setting up debian",
      "INFO  jepsen.os.debian - :n5 setting up debian",
      "INFO  jepsen.os.debian - :n1 setting up debian",
      "INFO  jepsen.os.debian - :n2 setting up debian",
      "INFO  jepsen.os.debian - :n5 debian set up",
      "INFO  jepsen.os.debian - :n3 debian set up",
      "INFO  jepsen.os.debian - :n1 debian set up",
      "INFO  jepsen.os.debian - :n2 debian set up",
      "INFO  jepsen.os.debian - :n4 debian set up",
      "INFO  jepsen.system.etcd - :n4 etcd nuked",
      "INFO  jepsen.system.etcd - :n1 etcd nuked",
      "INFO  jepsen.system.etcd - :n5 etcd nuked",
      "INFO  jepsen.system.etcd - :n2 etcd nuked",
      "INFO  jepsen.system.etcd - :n3 etcd nuked",
      "INFO  jepsen.system.etcd - Running nodes: {:n1 false, :n2 false, :n3 false, :n4 false, :n5 false}",
      "INFO  jepsen.system.etcd - :n2 etcd nuked",
      "INFO  jepsen.system.etcd - :n3 etcd nuked",
      "INFO  jepsen.system.etcd - :n4 etcd nuked",
      "INFO  jepsen.system.etcd - :n5 etcd nuked",
      "INFO  jepsen.system.etcd - :n1 etcd nuked",
      "INFO  jepsen.system.etcd - :n1 starting etcd",
      "   INFO  jepsen.system.etcd - :n2 starting etcd",
      "INFO  jepsen.system.etcd - :n3 starting etcd",
      "INFO  jepsen.system.etcd - :n4 starting etcd",
      "INFO  jepsen.system.etcd - :n5 starting etcd",
      "INFO  jepsen.system.etcd - Running nodes: {:n1 true, :n2 true, :n3 true, :n4 true, :n5 true}",
      "INFO  jepsen.system.etcd - :n5 etcd ready",
      "INFO  jepsen.system.etcd - :n1 etcd ready",
      "INFO  jepsen.system.etcd - :n2 etcd ready",
      "INFO  jepsen.system.etcd - :n3 etcd ready",
      "INFO  jepsen.system.etcd - :n4 etcd ready",
      "INFO  jepsen.core - Worker 4 starting",
      "INFO  jepsen.core - Worker 3 starting",
      "INFO  jepsen.core - Worker 0 starting",
      "INFO  jepsen.core - Worker 1 starting",
      "INFO  jepsen.core - Worker 2 starting",
      "INFO  jepsen.util - 0	:invoke	:read	nil",
      "INFO  jepsen.util - 3	:invoke	:read	nil",
      "INFO  jepsen.util - 2	:invoke	:write	4",
      "INFO  jepsen.util - 1	:invoke	:write	2",
      "INFO  jepsen.util - 4	:invoke	:write	3",
      "INFO  jepsen.util - 3	:ok	:read	nil",
      "INFO  jepsen.util - 0	:ok	:read	nil",
      "INFO  jepsen.util - 4	:ok	:write	3",
      "INFO  jepsen.util - 1	:ok	:write	2",
      "INFO  jepsen.util - 2	:ok	:write	4",
      "INFO  jepsen.util - 3	:invoke	:write	3",
      "INFO  jepsen.util - 0	:invoke	:read	nil",
      "INFO  jepsen.util - 0	:ok	:read	3",
      "INFO  jepsen.util - 3	:ok	:write	3",
      "INFO  jepsen.util - 4	:invoke	:read	nil",
      "INFO  jepsen.util - 4	:ok	:read	3",
      "INFO  jepsen.util - 1	:invoke	:read	nil",
      "INFO  jepsen.util - 1	:ok	:read	3",
      "INFO  jepsen.util - 2	:invoke	:cas	[3 0]"
    )

    val expected = Seq(
      "0	:invoke	:read	nil",
      "3	:invoke	:read	nil",
      "2	:invoke	:write	4",
      "1	:invoke	:write	2",
      "4	:invoke	:write	3",
      "3	:ok	:read	nil",
      "0	:ok	:read	nil",
      "4	:ok	:write	3",
      "1	:ok	:write	2",
      "2	:ok	:write	4",
      "3	:invoke	:write	3",
      "0	:invoke	:read	nil",
      "0	:ok	:read	3",
      "3	:ok	:write	3",
      "4	:invoke	:read	nil",
      "4	:ok	:read	3",
      "1	:invoke	:read	nil",
      "1	:ok	:read	3",
      "2	:invoke	:cas	[3 0]"
    )

    val parsed = MyParser.parseKVLog(lines)
    // if(!input.exists()) throw new Error("The file doesn't exist")

    //DEBUG
    // println(MyParser.parseKVLog(lines).mkString("\n"))

    (expected zip (0 to expected.size-1)) foreach{
      line =>
      val parts = line._1.split(" ")
      parts(0) == parsed(line._2).getPID &&
      parts(2) == parsed(line._2).getOperation
    }
  }

  test("Cleaning + Parsing KV Log File") {
    val file = new File("data/etcd_000.PASTED.log")
    // val file = new File(System.getenv("HOME")+"/workspace/ds2/data/etcd_000.PASTED.log")
    val content = IO.readLinesFromFile(file)
    val idx = content.indexWhere(s => s.contains("INFO  knossos.core") || s.startsWith("------"), 0)
    val parsed = MyParser.parseKVLog(content.slice(0,idx))
    assert(parsed.size < idx && parsed.size == 170) // I counted them one by one ...

    // println("idx = " + idx)

//    val content =  Source.fromFile(file,enc="UTF8").getLines.toList
    // -------------- DEBUG ------------------
    // println(content.mkString("\n"))
    // println( MyParser.parseKVLog(content.slice(0,idx)).mkString("\n"))
    // println( MyParser.parseKVLog(content.slice(0,idx)).size)
  }

  test("Cleaning + Parsing Dirty Set Log String") {
    val in = Seq(
      " Some stupid sentences to try and trip the parser here",
      "0 invokes add 7",
      "2 invokes contains 3",
      "1 invokes contains 21",
      "more junk ",
      "  3 invokes contains 1", // yes, note the initial spaces
      "1 returns false",
      "2 returns false"
    )

    // DEBUG
    // println(MyParser.parseSetLog(in).mkString("\n"))

    assert(MyParser.parseSetLog(in).size == 6)
  }


  // it should work the same for a file as the KVLog anyways.
  // test("Cleaning + Parsing Set Dirty Log File") {
  //   fail("implement me")
  // }

}
