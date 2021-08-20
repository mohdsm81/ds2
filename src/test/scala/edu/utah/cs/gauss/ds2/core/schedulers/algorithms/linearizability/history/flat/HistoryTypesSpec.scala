package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat

import java.io.File

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.MyTestSpecs._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers.PorcupineData._
import edu.utah.cs.gauss.ds2.core.time.versionvectors._
import edu.utah.cs.gauss.serialization.IO

object HistoryFixtures {
  /* Some restrictions on causal chanins:
   - One causal chain can have more than one process
   - One causal chain can NOT have more than one partition (to respect
     the pcompositionality principle)
   - Under no partitioning assumption, could there be a causal chain
     between different keys and potentially different processes?
     + answer: In general yes a distributed system may exhibit that,
       but in the stores we are targetting (i.e. a model of a map) that
       is not the case. so no, in our case.
   */

  def historyWithDVV1: Seq[Event] = {
    // well formed (every inv is matched with a resp)
    val fileLines = Seq(
      // k4
      """{:process 0, :type :invoke, :f :put, :key "4", :value "x 0 0 y"}""", // 0 (cc1)
      """{:process 0, :type :ok, :f :put, :key "4", :value "x 0 0 y"}""", // 1 (cc1)
      """{:process 0, :type :invoke, :f :get, :key "4", :value nil}""", // 2 (cc1)
      """{:process 0, :type :ok, :f :get, :key "4", :value "x 0 0 y"}""", // 3 (cc1)
      // k0
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 1 y"}""", // 4 (cc2)
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 1 y"}""", // 5 (cc2)
      // k4
      """{:process 0, :type :invoke, :f :get, :key "4", :value "x 0 0 yx 0 1 y"}""", // 6 (cc1)
      """{:process 0, :type :ok, :f :get, :key "4", :value "x 0 0 yx 0 1 y"}""", // 7 (cc1)
      // k0
      """{:process 1, :type :invoke, :f :put, :key "0", :value "x 1 0 y"}""", // 8 (cc2)
      """{:process 1, :type :ok, :f :put, :key "0", :value "x 1 0 y"}""", // 9 (cc2)
      // k4
      """{:process 1, :type :invoke, :f :append, :key "4", :value "x 1 1 y"}""", // 10 (cc1)
      """{:process 1, :type :ok, :f :append, :key "4", :value "x 1 1 y"}""" // 11 (cc1)
    )

    val content = parseKV(fileLines)

    // hand processing of DVVs follow: (two causal chains and no left out events)
    val k0 = ":Key 0" // each key has its own DVV
    val k4 = ":Key 4" // because it is considered a single "sequential store/register"

    val dvv4 = DottedVersionVector(k4) // we assume one store and multiple causal chains
    val dvv0 = DottedVersionVector(k0)

    val p0 = ":process 0"
    val p1 = ":process 1"

    // 0
    content(0).setDVV(dvv4.copy)
    // 1
    content(1).setDVV(dvv4.copy) // response
    dvv4 advance p0
    // dvv4 ++ p0 // increments the version
    // dvv4 ++ k4
    // dvv4 incrementCounter p0 // increments the history counter
    //                          // so far, key-4-store/process and p0 are causally related and no gap
    //                          // between version and history (i.e. no concurrency chance there)
    // NOTE" advance the clock only upon completion of the call

    // 2
    content(2).setDVV(dvv4.copy) // no increments since this is NOT an update, just a get
    // 3
    content(3).setDVV(dvv4.copy) // same as 2, only get response
    // 4
    content(4).setDVV(dvv0.copy) // append inv
    // 5
    content(5).setDVV(dvv0.copy) // append response, now completed so increment/advance time
    dvv0 advance p0
    // dvv0 ++ p0
    // dvv0 ++ k0
    // dvv0 incrementCounter p0

    // 6 (another get inv), k4
    content(6).setDVV(dvv4.copy)
    // 7
    content(7).setDVV(dvv4.copy)
    // 8 (put inv), k0
    content(8).setDVV(dvv0.copy)
    // 9 (put resp), k0
    content(9).setDVV(dvv0.copy)
    dvv0.follows(p1, p0)

    // dvv0 ++ p1
    // dvv0 ++ k0
    // dvv0 incrementCounter p1
    // 10 (inv append), k4
    content(10).setDVV(dvv4.copy)
    // 11 (resp append), k4
    content(11).setDVV(dvv4.copy)
    dvv4 advance p1
    // dvv4 ++ p1
    // dvv0 ++ k4
    // dvv0 incrementCounter p1

    // val history = makeHistory(content)
    // history
    content
  }


  def historyWithDVV2: Seq[Event] = {
    // ill-formed (not every response has invocation, and not every inv has resp)

    val fileLines = Seq(
      // 0 - p0,k0: inv append (lonley)
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""", // 0 (no response, lonely)
      // 1 - p1,k4: inv append
      """{:process 1, :type :invoke, :f :append, :key "4", :value "x 1 0 y"}""", // 1 (cc1)
      // 2 - p1,k4: resp append
      """{:process 1, :type :ok, :f :append, :key "4", :value "x 1 0 y"}""", // 2 (cc1)
      // 3 - p0,k0: inv get 
      """{:process 0, :type :invoke, :f :get, :key "0", :value nil}""", // 3 (cc2)
      // 4 - p0,k0: resp get
      """{:process 0, :type :ok, :f :get, :key "0", :value ""}""", // 4 (cc2)
      // 5 - p0,k4: inv append
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""", // 5 (cc1)
      // 6 - p0,k4: resp append
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""", // 6 (cc1)
      // 7 - p1,k0: lonly resp
      """{:process 1, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""", // 7 (no inv, lonely)
      // 8 - p1,k0: resp append (lonely)
      """{:process 1, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""", // 8 (no inv, lonely)
      // 9 - p0, k4: lonely resp
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""" // 9 (no invocation, lonely)
    )
    val content = parseKV(fileLines)
    // hand processing of DVVs follow:
    val p0 = ":process 0"
    val p1 = ":process 1"
    val k0 = ":key 0"
    val k4 = ":key 4"
    val dvv0 = DottedVersionVector(k0)
    val dvv4 = DottedVersionVector(k4)

    // 0
    content(0).setDVV(dvv0.copy)
    // 1
    content(1).setDVV(dvv4.copy)
    // 2
    content(2).setDVV(dvv4.copy) // modification took effect, so increment
    dvv4.advance(p1)
    // dvv4 ++ p1
    // dvv4 incrementCounter p1
    // dvv4 ++ k4

    // 3, get inv
    content(3).setDVV(dvv0.copy)
    // 4, get resp
    content(4).setDVV(dvv0.copy) // no modification so no increments
    // 5 and 6
    content(5).setDVV(dvv0.copy)
    content(6).setDVV(dvv0.copy) // mod => increment
    dvv0.advance(p0)
    // dvv0 ++ k0
    // dvv0 ++ p0
    // dvv0 incrementCounter p0

    // 7
    content(7).setDVV(dvv0.copy) // did it or not take effect? since no inv so not, same for the rest
    content(8).setDVV(dvv0.copy)
    content(9).setDVV(dvv4.copy)

    content
  }
}


class HistoryTypesSpec extends MyTestSpecs {

  def sampleLinearizableHistory: Event = {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)

    Event.makeHistory(content)
  }


  def sampleIncompleteHistory: Event = {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""")
    //      ,
    //      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)

    Event.makeHistory(content)
  }

  def sampleIncompleteHistory2: Event = {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      //      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""")
    //      ,
    //      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)

    Event.makeHistory(content)
  }

  def sampleIncompleteHistory3: Event = {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""")
    //      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""")
    //      ,
    //      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)

    Event.makeHistory(content)
  }

  def sampleNoneLinearizableHistory: Event = {

    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :get, :key "5", :value nil}""",
      """{:process 0, :type :ok, :f :get, :key "5", :value ""}""",
      """{:process 0, :type :invoke, :f :get, :key "0", :value nil}""",
      """{:process 0, :type :ok, :f :get, :key "0", :value "x 0 0 y"}""", // no one added this before! non linearizable
      """{:process 0, :type :invoke, :f :append, :key "7", :value "x 0 4 y"}""",
      """{:process 0, :type :ok, :f :append, :key "7", :value "x 0 4 y"}""")

    val content = parseKV(fileLines)

    Event.makeHistory(content)
  }

  def sampleNoneLinearizableCausalHistory: Event = {
    val fileLines = Seq(
      """{:process 0, :type :ok, :f :get, :key "5", :value ""}""",
      """{:process 0, :type :invoke, :f :get, :key "0", :value nil}""",
      """{:process 0, :type :ok, :f :get, :key "0", :value "x 0 0 y"}""", // no one added this before! non linearizable
      """{:process 0, :type :invoke, :f :append, :key "7", :value "x 0 4 y"}""",
      """{:process 0, :type :ok, :f :append, :key "7", :value "x 0 4 y"}""")

    val content = parseKV(fileLines)

    CausalChain(Event.makeHistory(content))
  }

  test("Event.doComplete 1") {

    val hist = sampleIncompleteHistory
    hist.size should equal(3)
    val completed = Event.doComplete(hist)
    completed.size should equal(4)
  }

  test("Event.doComplete 2") {

    val hist = sampleIncompleteHistory2
    hist.size should equal(2)
    val completed = Event.doComplete(hist)
    completed.size should equal(4)
  }

  test("Event.doComplete 3") {

    val hist = sampleIncompleteHistory3

    print("\n---------------------------\n"+hist.mkString())

    hist.size should equal(3)
    val completed = Event.doComplete(hist)

    print("\n---------------------------\n"+completed.mkString())
    completed.size should equal(4)
  }

  test("history.size") {

    // has both Invocations and Responses, and every invocation there is a response for it.
    val content = parseKV(IO.readLinesFromFile(new File("data/c01-ok.txt")))
    val history = Event.makeHistory(content)

    var count = 0;
    val none: Option[Event] = None
    var event = history
    while (event != null) {
      event = event.getNext
      count += 1
    }
    assert(history.size == count)
  }

  test("history.mkString") {

    val size = sampleLinearizableHistory.mkString().split("\n").size
    //debug
    //    println(sampleLinearizableHistory.mkString())
    size should equal(4)
  }

  test("lifting an entry - linearizable") {

    var history = sampleLinearizableHistory
    insertBefore(history, getPadding)
    history = history.head

    var prevEntry1 = history.getNext

    history.getNext.size should equal(4)

    lift(prevEntry1)

    history.getNext.size should equal(2)

    // DEBUG
    //    println(history.mkString())

    var prevEntry2 = history.getNext

    lift(prevEntry2)

    // DEBUG
    //    println(history.mkString())

    assert(history.getNext == null)

    // I flipped the order in purpose to see if it restores order
    unlift(prevEntry2)
    unlift(prevEntry1)

    // DEBUG
    //    println(history.mkString())

    history.getNext.size should be(4) // again

  }

  test("lifting an entry - None Linearizable") {

    var history = sampleNoneLinearizableHistory
    insertBefore(history, getPadding)
    history = history.head

    var prevEntry1 = history.getNext

    history.getNext.size should equal(6)
    lift(prevEntry1)
    history.getNext.size should equal(4)

    var prevEntry2 = history.getNext

    lift(prevEntry2) // yes lifting a non-linearizable inv/response pair
    history.getNext.size should equal(2)

    var prevEntry3 = history.getNext
    lift(prevEntry3)
    history.getNext should be(null)

    unlift(prevEntry2)
    unlift(prevEntry3)
    unlift(prevEntry1)

    prevEntry2.head.mkString() should equal(history.mkString())

  }

  test("Event.isEmpty") {
    sampleLinearizableHistory.isEmpty should be(false)
  }

  test("Event.first and Event.last") {
    val sequence = Event.historyToSeq(sampleLinearizableHistory)
    val history = sampleLinearizableHistory

    history.first should equal(sequence.head)
    history.last should equal(sequence.last)
  }

  test("Event.partitionByKey") {
    partitionByKey(sampleLinearizableHistory).size should equal(2)
  }

  test("partitioning a big history - c50-bad.txt") {

    val content = parseKV(IO.readLinesFromFile(new File("data/c50-bad.txt")))
    val filteredContent = content.filter(_.getKey == Some[Any]("3"))
    val historyContent = partitionByKey(makeHistory(content))(Some[Any]("3"))

    historyContent.size should equal(filteredContent.size)

    // note that the following historyToSeq() call resets next and prev of each event to null as that of the seq.
    val sequentializedHistoryContent = Event.historyToSeq(historyContent)
    val ans = (filteredContent zip sequentializedHistoryContent).par forall {
      x =>
        x._1.getDVV == x._2.getDVV &&
          x._1.getArgs == x._2.getArgs &&
          x._1.getID == x._2.getID &&
          x._1.getKey == x._2.getKey &&
          x._1.getMatch == x._2.getMatch &&
          x._1.getNext == x._2.getNext &&
          x._1.getOperation == x._2.getOperation &&
          x._1.getPID == x._2.getPID &&
          x._1.getPrev == x._2.getPrev
    }

    assert(ans)
  }

  test("specific matchOf(entry) test") {
    val content = parseKV(IO.readLinesFromFile(new File("data/c10-ok.txt")))
    val history = Event.makeHistory(content)

    val entry = history.getNext
    val theMatch = matchOf(entry)

    assert(
      entry.isInvocation &&
        theMatch.isResponse &&
        //        entry.getPID == theMatch.getPID &&
        entry.getKey == theMatch.getKey &&
        entry.getOperation == theMatch.getOperation)
  }

  // test("new reNumber method") {
  //   val content = parseKV(IO.readLinesFromFile(new File("data/c10-ok.txt")))
  //   val history = Event.makeHistory(content)
  //
  //   val entry = history.getNext
  //   entry.reNumber
  //   // val map = entry.reNumber
  //
  //   // def matches(event: Entry): Entry = map(event)
  //
  //   var event = entry
  //   while (event != null) {
  //     // val theMatch = matches(event)
  //     val theMatch = matchOf(event)
  //     if (event.isInvocation) {
  //       // println(s"I = ${event.getID}, R = ${theMatch.getID}")
  //       event.getID should equal(theMatch.getID)
  //     }
  //     else
  //       theMatch should equal (null)
  //     event = event.getNext
  //   }
  // }


  test("make causal 1", This) {
    // val history = makeCausal(HistoryFixtures.historyWithDVV1)
    val history = makeCausal(HistoryFixtures.historyWithDVV1)
    history.size should equal(2)

    history.head match {
      case x: CausalChain => assert(x.chain.size == 8)
      case _ => assert(false)
    }

    history.last match {
      case x: CausalChain => assert(x.chain.size == 4)
      case _ => assert(false)
    }
  }

  test("make causal 2", This) {
    // println(HistoryFixtures.historyWithDVV2.mkString("\n"))
    /*
     After the close investigation of the printed history vs causal
     chains, althogh the causal chains are weird, the makeCausal()
     works. The history, though, is not wellformed so no wonder it
     will make stupid causal inferences as I implanted couple of
     non-sense Responses anyways (i.e. I lied to the method). The good
     news? the scheduler (running the actual distributed system should
     never produce an ill-formed history like this one so we are not
     worried about reasoning about well-formedness at this
     stage, i.e. CALC stage)
     */
    val history = makeCausal(HistoryFixtures.historyWithDVV2)
    history.size should be(3)

    //    print(history.toPorcupineLog)

    var accumulator = 0
    historyToSeq(history).map {
      case CausalChain(chain) => accumulator += chain.size
      case _ => accumulator += 1
    }

    assert(accumulator == 10) // 10 events in the original non-causal history
  }


  test("insertAfter case 1") {

    val target = Invocation(None, "0", ":append", Seq("x 0 0 y"))
    val toInsert = Response(None, "0", ":append", Seq("x 0 0 y"))

    HistoryTypes.Event.insertAfter(target, toInsert)

    assert(target.getNext == toInsert)
    assert(target.getPrev == null)
    assert(toInsert.getNext == null)
    assert(toInsert.getPrev == target)

  }

  test("insertAfter case 2") {
    val target = Invocation(None, "0", ":append", Seq("x 0 0 y"))
    val toInsert = Response(None, "0", ":append", Seq("x 0 0 y"))
    val toInsert2 = Invocation(None, "1", ":append", Seq("x 0 0 y"))

    HistoryTypes.Event.insertAfter(target, toInsert)

    assert(target.getNext == toInsert)
    assert(target.getPrev == null)
    assert(toInsert.getNext == null)
    assert(toInsert.getPrev == target)

    HistoryTypes.Event.insertAfter(target, toInsert2)

    assert(target.getNext == toInsert2)
    assert(target.getPrev == null)
    assert(toInsert2.getNext == toInsert)
    assert(toInsert2.getPrev == target)

    assert(toInsert.getNext == null)
    assert(toInsert.getPrev == toInsert2)
    assert(toInsert2.getNext == toInsert)
    assert(toInsert2.getPrev == target)

  }

  test("History.remove(entry)") {
    val history = sampleLinearizableHistory
    val size1 = history.head.size
    var rest = history.getNext
    Event.remove(history.head)
    rest = rest.head
    assert(size1 == rest.size + 1)
    assert(rest.head.isResponse)
  }

  test("History.causalReconstruct") {
    var history = sampleNoneLinearizableCausalHistory

    assert(history.chainFirst.isResponse)
    assert(history.isActualCausalChain)

    history.causalReconstruct

    assert(history.head.isResponse)
    assert(history.head.getNext.isActualCausalChain)
    assert(history.head.getNext.chainFirst.isInvocation)
  }

  test("stripOffPadding"){
    val hist = sampleNoneLinearizableHistory // has 6 real entries
    Event.insertBefore(hist,Event.getPadding)
    Event.insertBefore(hist,Event.getPadding)
    Event.insertBefore(hist,Event.getPadding)

    hist.first.size should be equals 9

    val trimmed = Event.stripOffPadding(hist.first).head

    trimmed.size should be equals 6


    Event.stripOffPadding(trimmed).first.size should be equals 6

  }

  //  test("make unCausal 1") {
  //    val history = makeCausal(HistoryFixtures.historyWithDVV1)
  //    println(makeUnCausal(history))
  //    assert(makeUnCausal(history).size == 12)
  //  }
  //  test("make unCausal 2") {???}

}
