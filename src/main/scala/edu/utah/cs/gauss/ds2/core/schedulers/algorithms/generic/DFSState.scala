package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.Helpers.DecoratorMethods._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.state.{DistributedSystemState, SchedulerState}
import edu.utah.cs.gauss.ds2.core.time.StopWatch


/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DFSState(sch: Scheduler with Helpers with Backtracking) {
  protected var enabledSet: Set[Receive] = Set()
  protected var exploredSet: Set[Receive] = Set()
  var dsState: DistributedSystemState = _ //sch.ds.snapshot
  var schState: SchedulerState = _ //sch.snapshot
  var schedule: Seq[Receive] = Seq()
  var stopWatch: StopWatch = StopWatch()
  var dc = 0
  var receive: Receive = _
  var iter = 0
  var endOfScheduleReached = false

  def areCoEnabledInState(x: DFSState, r1: Receive, r2: Receive): Boolean = (r1 in x.enabledSet) && (r2 in x.enabledSet)
  def areCoEnabled(x: DFSState, r: Receive): Boolean = areCoEnabledInState(x, x.receive, r)

  def hasDelays: Boolean = dc > 0

  def useDelays(delays: Int): Boolean = {
    if (delays > dc) false
    else {
      dc = dc - delays
      true
    } // else
  } // method

  def setEnabledSet(s: Set[Receive]): Unit = enabledSet = s

  def addToEnabledSet(receives: Set[Receive]): Unit = setEnabledSet(enabledSet ++ receives)

  def addToEnabledSet(r: Receive): Unit = setEnabledSet(getEnabledSet + r)

  def removeFromEnabledSet(r: Receive): Unit = setEnabledSet(getEnabledSet - r)

  def getEnabledSet: Set[Receive] = enabledSet

  def setExploredSet(s: Set[Receive]): Unit = exploredSet = s

  def getExploredSet: Set[Receive] = exploredSet

  def toExploreSet: Set[Receive] = enabledSet -- exploredSet

  def liftReceivesOfADTAgents: DFSState = {
    val agents = sch.targetAgents
    agents.filter(_.hasWork).foreach { a =>
      a.q.foreach{m =>
        addToEnabledSet(m->a)
      }
      a.q = Seq()
    }
    this
  }

  /**
   * Gets a receive for the agent 'a' from the to-explore set.
   *
   * @param a the agent whose receive will be returned
   */
  def nextReceiveFor(a: Agent): Option[Receive] = toExploreSet.find {
    case (_, dst) => dst.name == a.name
  }

  def markExplored(r: Receive): Unit = {
    setExploredSet(getExploredSet + r)
    receive = r
  }

  protected def perform(r: Receive): DFSState = {
    val m = r._1
    val dst = r._2

    // put the message back at the dst queue
    dst.q = dst.q :+ m

    sch.schedule(dst)
    sch.consumeATask(dst)
    sch.executeAll(0)
    this // this is just for chaining commands
  }

  def performReceive(r: Receive): DFSState = perform(r)

  def assignAttributesTo(state: DFSState): Unit = {
    state.enabledSet = enabledSet
    // each time we explore a new state we simply reset its exploredSet
//    state.exploredSet = exploredSet
    state.schedule = schedule
    state.stopWatch = stopWatch.copy
    state.iter = iter
    state.dc = dc
    state.receive = _ : Receive // each time the alg performs a receive it sets this field
    state.endOfScheduleReached = endOfScheduleReached
  }

  def nextState: DFSState = { // the beauty of immutability
    val newOne = new DFSState(sch)
    // snapshot is done at the creation time of DFSState()
    // newOne.dsState = dsState
    // newOne.schState = schState
    assignAttributesTo(newOne)
    newOne
  }
}
