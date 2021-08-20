package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent

import scala.collection.parallel.ParSet


/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class AgentState(agent: Agent) extends State[Agent,AgentState]{
  override var instanceToRestore: Agent = agent

  // val name = instanceToRestore.name
  val q = instanceToRestore.q
  // val defaultBehavior = instanceToRestore.defaultBehavior
  val stash = instanceToRestore.stash
   val reactions = instanceToRestore.reactions // changes with become/unbecome
  // val behaviors = instanceToRestore.behaviors // stay the same throught the actor life
  // val specialReactions = instanceToRestore.specialReactions //
  val oldBehaviors = instanceToRestore.oldBehaviors.clone()
  // scheduler is not part of its internal state
  val locked = instanceToRestore.locked
  val blocked = instanceToRestore.blocked
  val blockedOn = instanceToRestore.blockedOn match{
    case None => None // already frozen
    case Some(x) => DummyFutureState(x) // freeze its state
  }
  val consuming = instanceToRestore.consuming
  val partitionNo = instanceToRestore.partitionNo

  val futuresPromised = instanceToRestore.futuresPromised map{
    case (k,v) => (k,DummyFutureState(v)) // freeze their state
  }
  val futuresWaitingFor = instanceToRestore.futuresWaitingFor map{
    case (k,v) => (k,DummyFutureState(v)) // freeze their state
  }

  val stack = instanceToRestore.stack map {
    case x => ActivationFrameState(x) // freeze their state
  }

  override def restore: Unit = {

    instanceToRestore.q = q
    instanceToRestore.stash = stash
    instanceToRestore.reactions = reactions
    instanceToRestore.oldBehaviors = oldBehaviors
    instanceToRestore.blocked = blocked
    instanceToRestore.consuming = consuming
    instanceToRestore.partitionNo = partitionNo
    // futuresPromised && futuresWaitingFor should be restored BEFORE the blockedOn future.
    // then the blockedOn field can be restored.
    instanceToRestore.futuresPromised = futuresPromised map{case (k,v) => v.restore; (k,v.instanceToRestore)}
    instanceToRestore.futuresWaitingFor = futuresWaitingFor map{case (k,v) => v.restore; (k,v.instanceToRestore)}
    instanceToRestore.blockedOn = blockedOn match{
      case None => None
      case x:DummyFutureState => x.restore; Some(x.instanceToRestore)
    }

    // for the stack, restore the activation frame, return its instance, and create a new stack.
    // then, another iteration on the stack, assign the newly creates stack to all frames 'stack' field.
    instanceToRestore.stack = stack map{x => x.restore; x.instanceToRestore}
    instanceToRestore.stack map{x => x.stack = instanceToRestore.stack}
  }

  override def toString: String = {
    stack.top.localState.toString
  }

  def valOfVar[T](str: String): T = stack.top.localState.valOfVar(str)
}
