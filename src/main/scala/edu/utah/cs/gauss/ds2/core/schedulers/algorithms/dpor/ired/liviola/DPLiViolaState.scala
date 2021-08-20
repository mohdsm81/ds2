package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.DPIRedState
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.Backtracking

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DPLiViolaState(sch: Scheduler with Helpers with Backtracking) extends DPIRedState(sch) {

  private def keyOf(r1: Receive): Any = r1._1.payload[Any](sch.keyIndicesMap(r1._1.name))

  override def areDependent(r1: Receive, r2: Receive): Boolean = {
    val sameReceiver = super.areDependent(r1,r2)
    val sameKey = keyOf(r1) == keyOf(r2)
//    val atLeastOneWrite = sch.isModificationCall(r1._1) || sch.isModificationCall(r2._1)

    sameReceiver && sameKey //&& atLeastOneWrite
  }

  override def nextState: DPLiViolaState = {
    val newOne = new DPLiViolaState(sch)
    super.assignAttributesTo(newOne)
//    newOne.freeze = false
    newOne
  }
}