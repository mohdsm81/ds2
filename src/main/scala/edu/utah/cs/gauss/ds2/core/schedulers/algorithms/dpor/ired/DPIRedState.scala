package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired

import edu.utah.cs.gauss.Helpers.DecoratorMethods.SetContains
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.DporState
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.Backtracking

import scala.collection.mutable.{Stack => MStack}

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DPIRedState(sch: Scheduler with Helpers with Backtracking) extends DporState(sch) {

  def stack: MStack[DPIRedState] = sch.backtrack.asInstanceOf[MStack[DPIRedState]]

  override def didEnable(enabled: Receive, state: DporState): Set[Receive] = {
    if(stack.size >= 2) {
      val ofInterest: Seq[Receive] = stack.tail.tail.splitAt(stack.tail.tail.indexOf(state))._1.map(_.receive)
      var curr = enabled
      for(potentialEnabler <- ofInterest ) {
        if(curr._1.sender == potentialEnabler._2) curr = potentialEnabler
      }
      if((curr in state.getEnabledSet) && curr != enabled) Set(curr)
      else Set.empty //super.didEnable(enabled,state)
    } else Set.empty
  }

  override def nextState: DPIRedState = {
    val newOne = new DPIRedState(sch)
    super.assignAttributesTo(newOne)
//    newOne.freeze = false
    newOne
  }
}