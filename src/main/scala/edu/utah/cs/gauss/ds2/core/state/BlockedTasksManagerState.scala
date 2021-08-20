package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.schedulers.BlockedTasksManager

case class BlockedTasksManagerState(btm: BlockedTasksManager) extends State[BlockedTasksManager,BlockedTasksManagerState] {
  override var instanceToRestore: BlockedTasksManager = btm

  // ds and scheduler is non of this blocked tasks mgr business
  val blockedTasks = instanceToRestore.blockedTasks.map{x => SuspendableTaskState(x)}

  override def restore: Unit = {
    instanceToRestore.blockedTasks = blockedTasks.map{x => x.restore; x.instanceToRestore}
  }
}
