package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.ActivationFrame

case class ActivationFrameState(afr: ActivationFrame) extends State[ActivationFrame,ActivationFrameState]{
  override var instanceToRestore: ActivationFrame = afr

  // the stack is mutable, however the agent is responsible for
  // snapshotting the activation frames inside of it, and assigning the stack(s) to it/them
  val stack = instanceToRestore.stack
  val localState = LocalStateState(instanceToRestore.localState)
  val parameterList: Either[Seq[Any], Seq[String]] = instanceToRestore.parameterList
  val parameterAccessPattern = instanceToRestore.parameterAccessPattern
  val pushed = instanceToRestore.pushed
  val popped = instanceToRestore.popped

  override def restore: Unit = {
    instanceToRestore.stack = stack
    instanceToRestore.localState = {localState.restore; localState.instanceToRestore}
    instanceToRestore.parameterList = parameterList
    instanceToRestore.parameterAccessPattern = parameterAccessPattern
    instanceToRestore.pushed = pushed
    instanceToRestore.popped = popped
  }
}
