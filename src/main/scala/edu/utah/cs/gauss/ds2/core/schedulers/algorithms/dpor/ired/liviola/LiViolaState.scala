package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes.Receive
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.IRedState
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.Backtracking

import java.util.UUID
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class LiViolaState(sch: Scheduler with Helpers with Backtracking) extends IRedState(sch) {

  private def keyOf(r1: Receive): Any =
    if(sch.keyIndicesMap.contains(r1._1.name))
      r1._1.payload[Any](sch.keyIndicesMap(r1._1.name))
    else UUID.randomUUID()

  override def areDependent(r1: Receive, r2: Receive): Boolean = {
    val sameReceiver = super.areDependent(r1,r2)
    val sameKey = keyOf(r1) == keyOf(r2)
    val oneIsAWildCard = keyOf(r1) == -1 || keyOf(r2) == -1
    val atLeastOneWrite = sch.isModificationCall(r1._1) || sch.isModificationCall(r2._1)

    sameReceiver && (sameKey || oneIsAWildCard) //&& atLeastOneWrite
  }

  override def nextState: LiViolaState = {
    val newOne = new LiViolaState(sch)
    super.assignAttributesTo(newOne)
    newOne.freeze = false
    newOne
  }
}