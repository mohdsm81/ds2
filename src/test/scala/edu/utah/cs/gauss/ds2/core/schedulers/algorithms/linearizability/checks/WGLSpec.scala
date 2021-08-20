package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks

import edu.utah.cs.gauss.ds2.core.MyTestSpecs // specs
import edu.utah.cs.gauss.ds2.core.MyTestSpecs._ // tags
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.History
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers.PorcupineData._
import java.io.File
import edu.utah.cs.gauss.serialization.IO
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import org.scalatest.Ignore

import org.scalatest._


class WGLSpec extends MyTestSpecs {

  // -----------------------------------------------------------------
  // Micro Developmental tests (non porcupine)
  // -----------------------------------------------------------------

  test("Paper test", FastTest, PCompAlgTest) {
    //    val fileLines = Seq(
    //      """{:process 0, :type :invoke, :f :get, :key "k1", :value nil}""",
    //      """{:process 0, :type :invoke, :f :put, :key "k1", :value "1"}""",
    //      """{:process 1, :type :invoke, :f :get, :key "k1", :value nil}""",
    //      """{:process 0, :type :ok,     :f :get, :key "k1", :value ""}""",
    //      """{:process 0, :type :ok,     :f :put, :key "k1", :value "1"}""",
    //      """{:process 1, :type :ok,     :f :get, :key "k1", :value "1"}"""
    //      )

    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :put, :key "k1", :value "1"}""",
      """{:process 0, :type :ok,     :f :put, :key "k1", :value "1"}""",
      """{:process 0, :type :invoke, :f :get, :key "k1", :value nil}""",
      """{:process 1, :type :invoke, :f :get, :key "k1", :value nil}""",
      """{:process 1, :type :ok,     :f :get, :key "k1", :value "1"}""",
      """{:process 0, :type :ok,     :f :get, :key "k1", :value "1"}"""
    )



    val content = parseKV(fileLines)

//    println(content.size)
//    println(Event.makeHistory(content).size)

    //    Event.makeHistory(content).toPorcupineLog should be equals(fileLines.mkString("\n"))

    val alg = new WGL(Event.makeHistory(content))

    assert(alg.check == true) // is linearizable    
  }

  test("Paper test 2", FastTest, PCompAlgTest) {

    val fileLines = Seq(
      """{:process 3, :type :invoke, :f :put, :key "k1", :value "1"}""",
      """{:process 3, :type :ok,     :f :put, :key "k1", :value "1"}""",
      """{:process 1, :type :invoke,     :f :get, :key "k1", :value nil}""",
      """{:process 2, :type :invoke,     :f :get, :key "k1", :value nil}""",
      """{:process 1, :type :ok,     :f :get, :key "k1", :value "1"}""",
      """{:process 2, :type :ok,     :f :get, :key "k1", :value "1"}""",
      """{:process 3, :type :invoke,     :f :put, :key "k1", :value "2"}""",
      """{:process 3, :type :ok,     :f :put, :key "k1", :value "2"}""",
      """{:process 1, :type :invoke,     :f :get, :key "k1", :value nil}""",
      """{:process 1, :type :ok,     :f :get, :key "k1", :value "2"}""",
      """{:process 2, :type :invoke,     :f :get, :key "k1", :value nil}""",
      """{:process 2, :type :ok,     :f :get, :key "k1", :value "1"}"""
    )

    val content = parseKV(fileLines)

    //    Event.makeHistory(content).toPorcupineLog should be equals(fileLines.mkString("\n"))

    val alg = new WGL(Event.makeHistory(content))

    assert(alg.check == false) // is linearizable
  }


  test("Linearizable micro test", FastTest, PCompAlgTest, MicroTest1) {

    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)
    val alg = new WGL(Event.makeHistory(content))

    assert(alg.check == true) // is linearizable
  }

  test("None-Linearizable micro test", FastTest, PCompAlgTest, MicroTest2) {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""")

    val content = parseKV(fileLines)
    val alg = new WGL(Event.makeHistory(content))

    assert(alg.check == false) // is linearizable
  }

  test("GET operation for some keys", FastTest, PCompAlgTest, GetTest) {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :get, :key "5", :value nil}""",
      """{:process 0, :type :ok, :f :get, :key "5", :value ""}""",
      """{:process 0, :type :invoke, :f :get, :key "0", :value nil}""",
      """{:process 0, :type :ok, :f :get, :key "0", :value "x 0 0 y"}""", // no one added this before! non linearizable
      """{:process 0, :type :invoke, :f :append, :key "7", :value "x 0 4 y"}""",
      """{:process 0, :type :ok, :f :append, :key "7", :value "x 0 4 y"}""")

    val content = parseKV(fileLines)
    val alg = new WGL(Event.makeHistory(content), debug = false)

    assert(alg.check == false) // is linearizable

  }

  test("Snippet of c01-OK.txt test", PCompAlgTest, FastTest, C01K, Snippet) {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 3 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 3 y"}""")

    val content = parseKV(fileLines)
    val alg = new WGL(Event.makeHistory(content))

    assert(alg.check == true) // is linearizable

  }

  // -----------------------------------------------------------------
  // Porcupine exact tests (partitioned and non partitioned) follow
  // -----------------------------------------------------------------

  // -------------
  // C01 ok
  // -------------
  test("c01-OK.txt", FastTest, PCompAlgTest, C01K, S) {
    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-ok.txt")))

    assert(content.size == 116, "Wrong content size!!!")

    val alg = WGL(Event.makeHistory(content))

    val ans = alg.check
    assert(ans == true) // is linearizable

  }

  test("c01-OK.txt partitioned", PCompAlgTest, FastTest, C01K, P) {
    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-ok.txt")))

    val partitions = content.groupBy(_.getKey).values

    partitions.foreach { partition =>
      val history = Event.makeHistory(partition)

      val alg = WGL(history)
      assert(alg.check == true) // is linearizable
    }

  }

  test("selectable c01-OK.txt partitioned", PCompAlgTest, FastTest, C01K, P) {
    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-ok.txt")))

    val partitions = content.groupBy(_.getKey).values

    partitions.foreach { partition =>
      if (partition.head.getKey == Some("0")) {
        val alg = WGL(Event.makeHistory(partition))
        assert(alg.check == true) // is linearizable
      }
    }

  }
  // -------------
  // C01 bad
  // -------------

  test("c01-bad.txt", PCompAlgTest, FastTest, C01B, S) {
    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-bad.txt")))

    val alg = WGL(Event.makeHistory(content))

    assert(alg.check == false) // is not linearizable
  }

  test("c01-bad.txt partitioned", PCompAlgTest, FastTest, C01B, P) {
    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-bad.txt")))

    val partitions = content.groupBy(_.getKey).values

    assert(partitions.exists { partition =>
      val alg = WGL(Event.makeHistory(content))
      !alg.check
    })
  }

  // -------------
  // C10 bad
  // -------------
  test("checking partitioned c10-bad.txt should find a linearizability violation", SlowTest, PCompAlgTest, C10B, P) {

    /**
      * This one also, though parallelized/partitioned, and is only 810 entries,
      * still takes long under heavy CPU usage...
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-bad.txt")))

    val partitions = Event.partitionedHistories(content)

    val ans = partitions.values.par.exists {
      p =>
        val alg = WGL(p)
        alg.check == false // is not linearizable
    }

    assert(ans)
  }

  test("DEBUG specific partition of c10-bad.txt", SlowTest, PCompAlgTest, C10B, P) {

    /**
      * This one also, though parallelized/partitioned, and is only 810 entries,
      * still takes long under heavy CPU usage...
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-bad.txt")))

    // DEBUG
    //    val grouped = content.groupBy(x => x.getKey)

    val partitions = Event.partitionedHistories(content)

    val selectedKey = Some("0") // checks false:  0 , 1 , 2 , 3, 5, 6, 7, 9
    // checks true :  4 , 8

    val alg = WGL(partitions(selectedKey), false)

    val ans = alg.check

    assert(!ans)
  }


  ignore("checking c10-bad.txt should find a linearizability violation", SlowTest, PCompAlgTest, C10B, S, Infinity) {
    /**
      * This one also, though parallelized/partitioned, and is only 810 entries,
      * still takes long under heavy CPU usage...
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-bad.txt")))

    val alg = WGL(Event.makeHistory(content), true)// ,graphFilePathToEmit = "data/c10-bad.dot") //, false, "data/c50.bad.dot")
//    alg.startIteration = 750
//    alg.endIteration = 800

    val ans = alg.check // is not linearizable

    assert(!ans)
  }

  // -------------
  // C10 ok
  // -------------

  test("DEBUGGING checking specific partition of c10-ok.txt", PCompAlgTest, SlowTest, C10K, D, P) {
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-ok.txt")))

    val partitions = Event.partitionedHistories(content)

    println("Keys: " + partitions.keySet.mkString(","))

    val ans = {
      val selectedKey = Some("9") // 0 or 7, 9
      val alg = WGL(partitions(selectedKey))
      alg.check
    }
    assert(ans)
  }

  test("checking partitioned c10-ok.txt", PCompAlgTest, SlowTest, C10K, P) {
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-ok.txt")))

    //    println(content.mkString("\n"))

    val partitions = Event.partitionedHistories(content)

    //    println("Keys: " + partitions.keySet.mkString(","))

    val ans = partitions.values.forall {
      p =>
        val alg = WGL(p) //, false, "data/c10.ok.partitioned.fail.dot")
        alg.check
    }

    assert(ans)

  }

  ignore("checking c10-ok.txt", PCompAlgTest, SlowTest, Infinity, C10K, S) {
    val content = parseKV2(IO.readLinesFromFile(new File("data/c10-ok.txt")))

    val alg = WGL(Event.makeHistory(content))

    assert(alg.check)

  }

  // -------------
  // C50 bad
  // -------------
  ignore("checking c50-bad.txt should find a linearizability violation", SlowTest, PCompAlgTest, Infinity, C50B, S) {
    /**
      * This test takes too long (over 5 mins, it
      * has 4048 entries/lines) so I will use the partitioning
      * technique to parallelize and get the speed up.
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-bad.txt")))

    val alg = WGL(Event.makeHistory(content)) // , true, "data/c50.bad.cached.dot")

    assert(alg.check == false) // is not linearizable
  }

  ignore("DEBUGGING: checking ONE partition of c50-bad.txt should find a linearizability violation", Infinity, SlowTest, PCompAlgTest, C50B, D, P) {
    /**
      * Takes up to 1 minute and 52 seconds to finish!!! the nonCaching version
      * is WAY faster.
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-bad.txt")))

    val partitions = Event.partitionedHistories(content)

    val selectedKey = Some("8") // 1 (now passes!), 2 (passes), 3(passes), 4(passes),6(passes),, (0,5,7,8,9) take forever and don't terminate

    val alg = WGL(partitions(selectedKey)) //, true)
    // alg.fileToOutput = "../scratch/ds2.log"
    // alg.startIteration = 0
    // alg.endIteration = 1096107 // 10K increments of start

    assert(alg.check == false)
  }

  ignore("checking partitioned c50-bad.txt should find a linearizability violation", SlowTest, Infinity, PCompAlgTest, C50B, P, This) {
    /**
      * This test takes too long (over 5 mins, it
      * has 4048 entries/lines) so I will use the partitioning
      * technique to parallelize and get the speed up.
      */

    // parse the data from the file
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-bad.txt")))

    val partitions = Event.partitionedHistories(content)

    val ans = partitions.values.par.exists {
      p =>
        val alg = WGL(p, false) //,s"c50-bad.$p.dot")
        !alg.check
    }

    assert(ans)
  }

  // -------------
  // C50 ok
  // -------------

  ignore("DEBUGGING: Selectable c50-ok.txt partition", PCompAlgTest, SlowTest, Infinity, C50K, P) {
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-ok.txt")))

    val partitions = Event.partitionedHistories(content)

    val selectedKey = Some("0")

    val alg = WGL(partitions(selectedKey))

    assert(alg.check)

  }

  ignore("checking partitioned c50-ok.txt", PCompAlgTest, SlowTest, Infinity, C50K, P) {
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-ok.txt")))

    val partitions = Event.partitionedHistories(content)

    val ans = partitions.values.forall {
      p =>
        val alg = WGL(p)
        alg.check
    }

    assert(ans)

  }

  ignore("checking c50-ok.txt", SlowTest, Infinity, PCompAlgTest, C50K, S) {
    val content = parseKV(IO.readLinesFromFile(new File("data/c50-ok.txt")))

    val alg = WGL(Event.makeHistory(content))

    alg.fileToOutput = "data/testing.infinity.log"
    alg.startIteration = 0
    alg.endIteration = Int.MaxValue

    assert(alg.check)

  }

}
