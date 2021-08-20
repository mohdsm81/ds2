package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem}
import edu.utah.cs.gauss.ds2.core.order.RoundRobinQueue
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
import edu.utah.cs.gauss.ds2.core.time.StopWatch

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class RoundRobinScheduler(val harnFilePath: String,
                          val distSys: DistributedSystem,
                          iterationsLimit: Int = Int.MaxValue,
                          histSizeLimit: Int = 4048,
                         benchmark: Boolean = false,
                         numSchedulesLimit: Int = 1) extends
  Scheduler with Helpers with RoundRobinQueue[Agent] {

  harnessFilePath = harnFilePath
  ds = distSys
  attach(ds)
  ds.attach(this)
  var benchmarkStatistics: BenchmarkResults = BenchmarkResults(this)

  var schedule: Schedule = Seq()

  override def s: RoundRobinScheduler = this

  override val maxHistoryLength: Int = histSizeLimit
  var iter = 0 // increment with each loop iteration

//  var history: Event = Event.getPadding

  def prepare(): Unit = {

    filePrompt() // parses the file and harness

    setItems(filterTargetAgents(ds).toSeq) // only ADT agents are issued invocations

    val iterator = Iterator.continually(items).flatten
    
    setItems(items)

    schedule = Seq.empty[Receive]
    harness foreach {
      x =>
        x.sender = createNewClient // each invocation is sent by a different client
        val nextItem = iterator.next()
        schedule = schedule :+ makeReceiveFor(nextItem, x)
    }

    startAllAgentsUsing(this)

    spec = getInitializedSpecFromAgentDataStructure(targetAgents)

    setItems(ds.agents.toSeq) // now we set the RR queue to all agents (ADT and Clients)
  }


  prepare() // parse and stuff
  // capture everything
  var scheduleRecorded: Schedule = Seq[Receive]()
  val timeElapsed: StopWatch = StopWatch()
  var enabledSet: Set[Receive] = schedule.toSet
  def cond: Boolean = ds.agents.exists(_.hasWork) || enabledSet.nonEmpty

  override def explore: Unit = {
//    def flipACoin: Boolean = Math.random < 0.20

    // start timing if benchmark
    if(benchmark) {
      timeElapsed.start
      benchmarkStatistics.harness = enabledSet.toList
      benchmarkStatistics.dsName = distSys.name
      benchmarkStatistics.numOfAgents = targetAgents.size
      benchmarkStatistics.schedulerName = getClass.getSimpleName
    }

    val distSysState = distSys.snapshot
    val thisScheduleWatch = StopWatch()

    (1 to numSchedulesLimit) foreach { _ =>
      while (
          cond &&
//            lessThanHistorySizeLimit(history.head) &&
            iter < iterationsLimit) {

        if(benchmark && !thisScheduleWatch.isStarted) thisScheduleWatch.start

        getNext // initialize the current agent to next agent in the queue

        if (enabledSet.nonEmpty) {
          val r = enabledSet.head
          enabledSet = enabledSet - r
          ds.send(r._1.sender, r._1, r._2)
        }

        if (!isClient(current) && current.hasWork) {
          val m = current.q.head
          scheduleRecorded = scheduleRecorded :+ (m -> current)
//          if(isInvocation(m)) insertAfter(history.last, makeInvocation(m, current))
          schedule(current)
          consumeATask(current)
          executeAll()
        }

        if (isClient(current) && current.hasWork) {
          current.q = current.q.filter(isResponse) // anything other than responses in client's queues is BS
          current.q.foreach { resp =>
            scheduleRecorded = scheduleRecorded :+ (resp -> current)
//            insertAfter(history.last, makeResponse(resp, current))
          }
          current.q = Seq() // empty the queue
        }
        iter += 1
      } // while
      if (benchmark) {

        // record everything
        thisScheduleWatch.stop
        benchmarkStatistics.addSchedule(scheduleRecorded)
        benchmarkStatistics.addTime(thisScheduleWatch.copy)
//        benchmarkStatistics.addHistory(history.copyAll)

        // reset everything
        thisScheduleWatch.reset
        scheduleRecorded = Seq.empty
        enabledSet = schedule.toSet
//        history = Event.getPadding
        ds.restore(distSysState)
        iter = 0
      }
    } // map (repeating execution)
    if(benchmark) {
      timeElapsed.stop
      benchmarkStatistics.elapsedTime = timeElapsed
    }
  } // explore
}
