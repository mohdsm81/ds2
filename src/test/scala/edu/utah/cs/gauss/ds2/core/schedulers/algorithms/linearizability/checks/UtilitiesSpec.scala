package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.MyTestSpecs._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers.PorcupineData.parseKV

class CheckUtilitiesTests extends MyTestSpecs {

  test("check cache 1", FastTest, PCompAlgTest, MicroTest1) {

    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :invoke, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""")

    val content = parseKV(fileLines)
    val history = Event.makeHistory(content)
    val alg = new WGL(history)

    val map = alg.computeMatchMap

    var event = history // don't call head or we will get a dummy-head node prepended by the alg
    while (event != null) {
      Event.matchOf(event) should equal(alg.matches(event))
      event = event.getNext
    }
  }

  test("check cache 2", FastTest, PCompAlgTest, MicroTest2) {
    val fileLines = Seq(
      """{:process 0, :type :invoke, :f :append, :key "0", :value "x 0 0 y"}""",
      """{:process 0, :type :ok, :f :append, :key "4", :value "x 0 1 y"}""",
      """{:process 0, :type :ok, :f :append, :key "0", :value "x 0 0 y"}""")

    val content = parseKV(fileLines)
    val history = Event.makeHistory(content)
    val alg = new WGL(history)

    var event = history // don't call head or we will get a dummy-head node prepended by the alg
    while (event != null) {
      Event.matchOf(event) should equal(alg.matches(event))
      event = event.getNext
    }
  }
}
