package edu.utah.cs.gauss.ds2.core.schedulers.composable.task.interleaving

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem }
import edu.utah.cs.gauss.ds2.core.schedulers.{Scheduler, TaskQ}



/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  *
  * This is the top level trait users need to extend (or extend one of its sub traits/classes) to
  * model their scheduler/algorithm-specific tasks interleavings that are allowed by their
  * scheduler/process-model.
  *
  *
  */
trait TaskInterleavings extends Scheduler {
  import TaskInterleaving.Policy
  import Policy._

  type Interleaving = (TaskQ,Seq[Agent])

  var cachedInterleavings: Set[Interleaving]

  def computeInterleavings(ds: DistributedSystem)(policy: Policy = PO)(): Set[Interleaving]

  def interleavingsIterator: Iterator[Interleaving] = cachedInterleavings.iterator

  def applyInterleavingTo(ds: DistributedSystem)(interleaving: Interleaving): Unit

}

object TaskInterleaving {
  object Policy extends Enumeration {
    type Policy = Value
    val PO, NPO = Value
  }
}
