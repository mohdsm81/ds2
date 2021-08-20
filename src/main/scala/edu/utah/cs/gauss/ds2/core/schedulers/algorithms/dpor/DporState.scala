package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor

import edu.utah.cs.gauss.Helpers.DecoratorMethods.SetContains
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.{Backtracking, DFSState}

import scala.collection.mutable.{Stack => MStack}
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DporState(sch: Scheduler with Helpers with Backtracking)
  extends DFSState(sch) {

  protected var backtrackingSet: Set[Receive] = Set()

  def getBacktrackingSet: Set[Receive] = backtrackingSet

  def setBacktrackingSet(s: Set[Receive]): Unit = backtrackingSet = s

  private def stack: MStack[DporState] = sch.backtrack.asInstanceOf[MStack[DporState]]

  def areDependent(r1: Receive, r2: Receive): Boolean =  r1._2 == r2._2 // same receivers

  def areDependent(x: DporState, r: Receive): Boolean = areDependent(x.receive, r)

  def didEnable(enabled: Receive, state: DporState): Set[Receive] = {
    val tail = stack.tail
//    tail.tail.splitAt(tail.tail.indexWhere(_ == state))._1.map{x => x.receive}.toSet
    tail.tail.splitAt(tail.tail.indexWhere(_ == state))._1.map{x => x.receive}.toSet.filter(_ in state.enabled)
  }

  def assignAttributesTo(state: DporState): Unit = {
    super.assignAttributesTo(state)

    // the backtracking set and the explored set go hand in hand at each state, so they are reset on creation
    //    state.backtrackingSet = backtrackingSet
  }

  override def nextState: DporState = {
    val newOne = new DporState(sch)
    assignAttributesTo(newOne)
    newOne
  }

  implicit def latestState: Ordering[DporState] = new Ordering[DporState] {
    override def compare(x: DporState, y: DporState): Int = {
      if (stack.indexOf(x) < stack.indexOf(y)) 1
      else if (stack.indexOf(x) > stack.indexOf(y)) -1
      else 0
    }
  }

  implicit def latestReceive: Ordering[Receive] = new Ordering[Receive] {
    override def compare(x: Receive, y: Receive): Int = {
      if (stack.indexWhere { s => s.receive == x } < stack.indexWhere { s => s.receive == y }) 1
      else if (stack.indexWhere { s => s.receive == x } > stack.indexWhere { s => s.receive == y }) -1
      else 0
    }
  }

  def enabled: Set[Receive] = enabledSet -- {
    backtrackingSet ++ exploredSet
  }

  def backtracking: Set[Receive] = backtrackingSet -- exploredSet

  override def toExploreSet: Set[Receive] = backtracking

  override def performReceive(r: Receive): DporState = {
    perform(r) // new dependencies discovered
    liftReceivesOfADTAgents
    updateBacktracking(r)
//    // remove this after debugging TransDPOR not to mess up DPOR
//    //===========================
//    // the following if (including the 'else' but not the 'if' after it) is to force a certain interleaving in TransDPOR
//    //===========================
//    if(stack.size == 4 &&
//      sch.asInstanceOf[TransDporScheduler].benchmarkStatistics.schedules.size == 2){
//      val r2 = enabled.filter{ x => x._1.name == "register" && x._1.sender.name == "worker2"}
//      setBacktrackingSet(getBacktrackingSet + r2.head)
//    }
//    else
    if (enabled.nonEmpty && backtracking.isEmpty)
      setBacktrackingSet(getBacktrackingSet + enabled.head)
    this
  }

  // ---------------------------------------------------------------------------------------------------------
  // This set of methods are to be overridden by TransDPOR: computeE, doWhenEnonEmpty, doWhenEisEmpty, and filterPreviousStates
  // ---------------------------------------------------------------------------------------------------------
  def computeE(r: Receive, state: DporState): Set[Receive] = {

    val result = state.enabledSet.filter{x => x == r} match{
      case x: Set[Receive] if x.nonEmpty =>
        x // co-enabled case
      case _ =>   // enabling receive case
        didEnable(r,state)
    }

    result // for debugging, it should be removed later
  }

  def doWhenConditionEApplies(E: Set[Receive], state: DporState): Unit = state.setBacktrackingSet(state.getBacktrackingSet + E.head)

  def doWhenConditionEDoesNotApply(state: DporState): Unit = state.setBacktrackingSet(state.getBacktrackingSet ++ state.enabledSet)
//  def doWhenConditionEDoesNotApply(state: DporState): Unit = state.setBacktrackingSet(state.backtracking ++ state.enabled)

  def filterPreviousStates(r: Receive): Set[DporState] = stack.tail.tail.toSet.filter { x => areDependent(x, r) }

  def conditionE(E: Set[Receive], state: DporState): Boolean = E.nonEmpty

  // ---------------------------------------------------------------------------------------------------------

  def updateBacktracking(r: Receive): Unit = {
    if (stack.tail.tail.exists { x => areDependent(x.receive, r)}) { // if such a state exists (or more than one)
      val latestDependentState = filterPreviousStates(r).max // pre(S,i)
      val E = computeE(r, latestDependentState) // and r is definitely in the top of the stack pending receives
      if (conditionE(E, latestDependentState)) {
        // adds a single receive from E to backtracking of pre(S,i) in case of DPOR and TransDPOR if not frozen
        doWhenConditionEApplies(E, latestDependentState)
      }
      else {
        /*In contrast, if E is empty, the algorithm was not able to
        identify a previous transition whose execution is necessary for next(s, m)
        to become enabled from pre(S, i)*/
        // add all enabled receives to backtracking of pre(S,i) in case of DPOR, and only one of pending in case of TransDPOR
        doWhenConditionEDoesNotApply(latestDependentState)
      }
    }
  }
}