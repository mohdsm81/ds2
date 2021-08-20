package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.transdpor

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.DporState
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.Backtracking
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class TransDporState(sch: Scheduler with Helpers with Backtracking) extends DporState(sch) {

  var freeze: Boolean = false

  override def doWhenConditionEDoesNotApply(state: DporState): Unit = {}

  override def doWhenConditionEApplies(E: Set[Receive], state: DporState): Unit = {
    if (!state.asInstanceOf[TransDporState].freeze) {
      super.doWhenConditionEApplies(E, state)
      state.asInstanceOf[TransDporState].freeze = true
    }
  }

  override def conditionE(E: Set[Receive], state: DporState): Boolean = (E -- state.getBacktrackingSet).nonEmpty

  override def computeE(r: Receive, state: DporState): Set[Receive] = {
    val E = super.computeE(r,state)
    if(E nonEmpty) Set(E.min)
    else E
  }


  override def nextState: DporState = {
    val newOne = new TransDporState(sch)
    assignAttributesTo(newOne)
    newOne.freeze = false
    newOne
  }
}