package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem }
import edu.utah.cs.gauss.ds2.core.state.{ DistributedSystemState, SchedulerState }
import edu.utah.cs.gauss.ds2.core.time.StopWatch

import scala.collection.mutable.{ Stack => MStack }

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */

class ExhaustiveDFSScheduler(
                              override val harnFilePath: String,
                              override val distSys: DistributedSystem,
                              benchmark: Boolean = false,
                              log: Boolean = false,
                              iterationsLimit: Int = Int.MaxValue,
                              val histSizeLimit: Int = 4048,
                              numOfSchedulesLimit: Int = Int.MaxValue
                            ) extends RoundRobinScheduler(
  harnFilePath,
  distSys,
  iterationsLimit,
  histSizeLimit
) with Backtracking {
  backtrack = MStack(new DFSState(this))

  currentState.setEnabledSet(schedule.toSet) // yup, even invocations are explored to their maximum re-ordering

  benchmarkStatistics.dsName = ds.name
  benchmarkStatistics.numOfAgents = targetAgents.size
  benchmarkStatistics.schedulerName = getClass.getSimpleName

  // init state
  setItems(items.filterNot(isClient)) // only keep the ADT agents to cycle through

  def loop(codeBlock: => Unit): Unit = {
    benchmarkStatistics.harness = currentState.getEnabledSet.toList
    while (backtrack.nonEmpty && benchmarkStatistics.schedules.size < numOfSchedulesLimit) {
      codeBlock
    } // while
  } // loop

  def updateScheduleWithResponses(): Unit = {
    if (clients.exists(_.hasWork)) {
      clients.filter(_.hasWork).map { client =>
        client.q = client.q.filter(isResponse) // anything other than responses in client's queues is BS
        client.q.foreach { resp =>
          currentState.schedule = currentState.schedule :+ (resp, client)
        }
        client.q = Seq() // empty the queue
      } // map
    } //if
  } // update method



  def agentHasReceive(agent: Agent): Boolean = currentState.toExploreSet.exists {
    case (_, dst) if null != agent => agent.name == dst.name
    case _ => false
  }

  def captureState(): Unit = {
    currentState.dsState = DistributedSystemState(ds)
    currentState.schState = SchedulerState(s)
  }

  def body(): Unit = {

    if (benchmark) {
      benchmarkStatistics.elapsedTime.start
      currentState.stopWatch.start
    }
    
    // just in case there is remaining receives from the start action
    currentState.liftReceivesOfADTAgents
  
    loop { // the main loop
      getNext // updates 'current'
      // ---------------------------
      // Exploration Branch
      // ---------------------------
      if (currentState.toExploreSet.nonEmpty &&
        agentHasReceive(current)) {

        // before doing anything... we need to capture/snapshot the state of the system
        captureState()
  
        val r = currentState
          .nextReceiveFor(current)
          .getOrElse(throw new Error("Holly Crab! No receives..."))
  
  
        currentState.markExplored(r) // mark it explored in the current state
  
        if (benchmark) currentState.stopWatch.stop // have to stop before pushing a new state else the timer grows too much
  
  
        push(currentState.nextState) // make the next state the current resetting some parameters (i.e. make it the top of the stack)
        currentState.iter += 1

        if (benchmark) { // then reset the timer that was copied from prev. state and start it to time things.
          currentState.stopWatch.reset
          currentState.stopWatch.start
        }

        currentState.removeFromEnabledSet(r) // remove that has been explored
        // explored set is auto-reset by the nextState() method
        currentState.schedule = currentState.schedule :+ r
        currentState.performReceive(r) // the effects will reflect in current state
        currentState.liftReceivesOfADTAgents // a bench of receives have been enabled, add them to next state
        updateScheduleWithResponses()

        /*  Capture the distributed system and scheduler state, why?
            because, the materialised effects in the local state of agents (due to performing the receive in a PREVIOUS
            iteration) need be captured.
            Initially, it will capture the original state, no harm done. But eventually, it will capture previous
            iteration state before pushing a new state on top of the stack.
        */
//        captureState()
        if (currentState.toExploreSet.isEmpty) currentState.endOfScheduleReached = true
      } // if->explore

      // ---------------------------
      // Backtracking branch
      // ---------------------------
      if (currentState.toExploreSet.isEmpty ||
        currentState.iter >= iterationsLimit ||
        currentState.schedule.count { x => isResponse(x._1) || isInvocation(x._1) } >= maxHistoryLength
      ) { // full history and schedule generated and should be added

        if (benchmark && currentState.endOfScheduleReached) {
          currentState.stopWatch.stop
          val sw = StopWatch()
          sw.accumulator = backtrack.map { x => x.stopWatch.getAccumulatedTime }.sum
          benchmarkStatistics.addTime(sw)
        }

        if (currentState.endOfScheduleReached) {
          // so that we don't add a schedule and a history when we backtrack in the middle of them
          benchmarkStatistics.addSchedule(currentState.schedule)
        }

        if (log) println(s"========== BACKTRACKING ========== at iter = ${currentState.iter} AND DEPTH = ${backtrack.size}")
        // try another schedule starting from previous state
        pop

        if (backtrack.nonEmpty) {
          currentState.dsState.restore
          currentState.schState.restore
        }
      } // if->backtrack
    } // loop
    if (benchmark) benchmarkStatistics.elapsedTime.stop
  } // body

  override def explore: Unit = body()
}
