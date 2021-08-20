package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.structured

import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.tracing._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.History
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.LinearizabilityInvocation
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.LinearizabilityResponse
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.HistoriesManager
import scala.math.BigInt.int2bigInt

class LinearizabilityToFromJson extends MyTestSpecs {

  import Fixtures._

  info("=======================================")
  info("Linearizability to/from json")
  info("=======================================")



  test("Linearizability Invocation") {
    val li = LinearizabilityInvocation(ExecuteTimedAction(timedActionInstance)(() => 10))

    li.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    li.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)

    val js = li.toJson

    val li2 = LinearizabilityInvocation.fromJson(js)

    li2.id should equal(li.id)


  }

  test("Linearizability Response") {
    val lr = LinearizabilityResponse(new ExecuteTimedAction(timedActionInstance)(() => 10))

    lr.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    lr.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)

    val js = lr.toJson

    val te2 = LinearizabilityInvocation.fromJson(js) // yes even linearizabilityresponse is actualy linearizabilityresponse

    te2.id should equal(lr.id)
  }

  test("History") {
    val h = historyInstance

    val js = h.toJson

    val h2 = History.fromJson(js)

    h2.entries.size should equal(h.entries.size)
  }

  test("Histories Manager") {
    val hm = new HistoriesManager
    hm.traces = scala.collection.mutable.Seq(historyInstance, historyInstance)

    val js = hm.toJson

    val hm2 = HistoriesManager.fromJson(js)

    // DEBUG
    //    println(pretty(render(js)))

    hm2.traces.size should equal(hm.traces.size)

  }
}
