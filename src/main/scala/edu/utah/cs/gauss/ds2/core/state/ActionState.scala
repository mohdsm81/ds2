package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.Action

case class ActionState(act: Action) extends State[Action,ActionState]{
  override var instanceToRestore: Action = act

  val executed = instanceToRestore.executed
  val toExecute = instanceToRestore.toExecute
  val message = instanceToRestore.m
  val agent = instanceToRestore.a

  override def restore: Unit = {
    instanceToRestore.executed = executed
    instanceToRestore.toExecute = toExecute
    instanceToRestore.m = message
    instanceToRestore.a = agent
  }
}
