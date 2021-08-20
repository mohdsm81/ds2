package edu.utah.cs.gauss.ds2.core.schedulers.composable.message.reordering

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem, Message }
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 *
 * This top-level trait models the algorithm of the the possible re-orderings of messages 
 * tuned to catch certain type of bugs (i.e. targeted re-ordering using a feedback 
 * function from the scheduler)
 *
 * Users need to inherit from this one (or one of its traits) to model their 
 * scheduler/algorithm-specific message reordering.
 */
trait Reordering extends Scheduler{

  type MessageOrdering = Map[Agent,Seq[Message]]

  protected var cachedReorderings: Set[MessageOrdering]
  
  def computeReorderingsFor(ds: DistributedSystem)(targetAgents: Set[Agent] = ds.agents): Set[MessageOrdering]

  // scheduelrs can iterate to do their own thing
  def reorderings: Iterator[MessageOrdering] = cachedReorderings.iterator

  def applyReorderingTo(ds: DistributedSystem)(reordering: MessageOrdering): Unit
}

object Reordering {
  object Policy extends Enumeration {
    type Policy = Value
    val P2P, NP2P = Value
  }
}
