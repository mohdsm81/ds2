package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.TimedAction

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class TimedActionState(ta: TimedAction) extends State[TimedAction,TimedActionState]{
  override var instanceToRestore: TimedAction = ta

  val action = ActionState(instanceToRestore.action)
  val startLimit = instanceToRestore.startLimit
  val endLimit = instanceToRestore.endLimit
  val howManyTimes = instanceToRestore.howManyTimes
  val submittedClock = instanceToRestore.submittedClock
  val countDown = instanceToRestore.countDown
  val runtimeHowManyTimes = instanceToRestore.runtimeHowManyTimes


  override def restore: Unit = {
    instanceToRestore.action = {action.restore; action.instanceToRestore}
    instanceToRestore.startLimit = startLimit
    instanceToRestore.endLimit = endLimit
    instanceToRestore.howManyTimes = howManyTimes
    instanceToRestore.submittedClock = submittedClock
    instanceToRestore.countDown = countDown
    instanceToRestore.runtimeHowManyTimes = runtimeHowManyTimes
  }
}
