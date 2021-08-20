package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.SuspendableTask

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class SuspendableTaskState(st: SuspendableTask) extends State[SuspendableTask,SuspendableTaskState]{
  override var instanceToRestore: SuspendableTask = st

  val isTimed = instanceToRestore.isTimed
  val suspended = instanceToRestore.suspended
  val action = ActionState(instanceToRestore.action)


  override def restore: Unit = {
    instanceToRestore.isTimed = isTimed
    instanceToRestore.suspended = suspended
    instanceToRestore.action = {action.restore; action.instanceToRestore}
  }
}
