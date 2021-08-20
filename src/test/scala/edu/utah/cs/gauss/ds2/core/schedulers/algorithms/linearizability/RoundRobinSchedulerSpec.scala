package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.RoundRobinScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks.WGL
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event
import edu.utah.cs.gauss.ds2.core.state.SchedulerState

import java.io.File

class RoundRobinSchedulerSpec extends MyTestSpecs {

  test("First test") {

    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegister("./harness.round.robin.1.txt", 5)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore

    sch.benchmarkStatistics.addSchedule(sch.scheduleRecorded)
    sch.benchmarkStatistics.generateHistories()

    var hist = Event.stripOffPadding(sch.benchmarkStatistics.histories.head.head)
    if (Event.isComplete(hist)) assert(WGL(hist).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }

  test("STRICT test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT("./harness.round.robin.2.txt", 2)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore

    sch.benchmarkStatistics.addSchedule(sch.scheduleRecorded)
    sch.benchmarkStatistics.generateHistories()

    var hist = Event.stripOffPadding(sch.benchmarkStatistics.histories.head.head)

    if (Event.isComplete(hist)) assert(WGL(hist).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }

  ignore("STRICT-retries test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_W_Retries("./harness.round.robin.3.txt", 2)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore

    sch.benchmarkStatistics.addSchedule(sch.scheduleRecorded)
    sch.benchmarkStatistics.generateHistories()

    var hist = Event.stripOffPadding(sch.benchmarkStatistics.histories.head.head)

    if (Event.isComplete(hist)) assert(WGL(hist).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }

  ignore("STRICT-retries NO CACHE test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_W_R_Retries_no_cache("./harness.round.robin.4.txt", 2)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore

    sch.benchmarkStatistics.addSchedule(sch.scheduleRecorded)
    sch.benchmarkStatistics.generateHistories()

    var hist = Event.stripOffPadding(sch.benchmarkStatistics.histories.head.head)

    if (Event.isComplete(hist)) assert(WGL(hist).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }

  test("STRICT RW retries and CACHE test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_W_RW_Retries_AND_cache("./harness.round.robin.5.txt", 2)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore

    sch.benchmarkStatistics.addSchedule(sch.scheduleRecorded)
    sch.benchmarkStatistics.generateHistories()

    var hist = Event.stripOffPadding(sch.benchmarkStatistics.histories.head.head)

    if (Event.isComplete(hist)) assert(WGL(hist).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }
  ignore("LOGGED STRICT RW retries + CACHE test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_W_RW_Retries_AND_cache_TAINTED("./harness.round.robin.6.txt", 3)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)

    // first sequence the writes (instead of coding paxos again)
    val writer = sch.ds.agents.filterNot{x => x.name.startsWith("IRed")}.head
    sch.schedule
      .filter { case (msg, _) => sch.isModificationCall(msg) }
      .map { case (msg, _) =>
        sch.ds.send(msg.sender, msg, writer)
        sch.benchmarkStatistics.histories.last.append(sch.makeInvocation(msg,writer))
      }

    // then round robin the reads

    sch.schedule = sch.schedule.filterNot { case (msg, _) => sch.isModificationCall(msg) }

    sch.explore
    var toRemove = sch.benchmarkStatistics.histories.head.head
    var hist = sch.benchmarkStatistics.histories.head.head.getNext
    Event.remove(toRemove)

    if (Event.isComplete(hist)) {
      println(hist.mkString("\n"))
      assert(WGL(hist.head).check)
    }
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }

  ignore("Majority-retries test") {
    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegisterMajority_W_Retries("./harness.round.robin.7.txt", 2)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)
    sch.explore
    var hist = sch.benchmarkStatistics.histories.head.head

    if (Event.isComplete(hist)) assert(WGL(hist.head).check)
    else {
      println("INCOMPLETE HISTORY")
      println(hist.head.mkString())
    }
  }


  test("Testing and debugging the snapshot resume feature") {

    val sample: (DistributedSystem, File) = SampleDistributedSystems.sampleLinearizableNAgentsRegister("./harness.round.robin.7.txt", 5)
    val sch = new RoundRobinScheduler(sample._2.getPath, sample._1)

    sch.prepare

    val hist: Event = Event.getPadding

    sch.step(sch.schedule.head, hist, sch.insertAfter)

    val schState: SchedulerState = sch.snapshot

    sch.schedule = sch.schedule.tail

    var r = sch.schedule.head

    sch.schedule = sch.schedule.tail

    schState.restore

    sch.targetAgents = sch.filterTargetAgents(sch.ds)

  }
}
