package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.schedulers.TimedActionsTracker

import scala.collection.parallel.ParSet


/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class TimedActionsTrackerState(tat: TimedActionsTracker) extends State[TimedActionsTracker,TimedActionsTrackerState]{
  override var instanceToRestore: TimedActionsTracker = tat

  val timedActions: ParSet[TimedActionState] = instanceToRestore.timedActions.map(TimedActionState(_))

  override def restore: Unit = {
    instanceToRestore.timedActions = timedActions.map{x => x.restore; x.instanceToRestore}
  }
}
