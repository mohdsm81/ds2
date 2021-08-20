package edu.utah.cs.gauss.ds2.core.copy

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Action, ActivationFrame, Agent, Behavior, DistributedSystem, DummyFuture, LocalState, Message}
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler

object DistributedSystemCopy {

  def attachSchedulerCopy(ds: DistributedSystem, sch: Scheduler): Unit = ds.attach(sch)

  def apply(ds: DistributedSystem): DistributedSystem = {
    val copy = new DistributedSystem(ds.name)
    copy.tracingEnabled = ds.tracingEnabled
    copy.temporaries = ds.temporaries
    ds.agents map { x => copy + apply(x, copy) }
    copy
  }

  def apply(agent: Agent, newDS: DistributedSystem): Agent = {
    val copy = new Agent(agent.name)
    copy.ds = newDS // important since all statements will rely on this one for their execution

    copy.q = agent.q map { x => apply(x, agent) }
    copy.futuresWaitingFor = agent.futuresWaitingFor map { case (k, v) => k -> apply(v, agent) }
    copy.futuresPromised = agent.futuresPromised map { case (k, v) => k -> apply(v, agent) }
    copy.blockedOn = agent.blockedOn match {
      case None => None
      case Some(f) => Some(apply(f, agent))
    }
    copy.stack = agent.stack map { x => apply(x, agent) }
    copy.stack map { x => x.stack = copy.stack }

    copy.blocked = agent.blocked
    copy.locked = agent.locked
    copy.consuming = agent.consuming
    copy.reactions = apply(agent.reactions, agent)
    copy.defaultBehavior = apply(agent.defaultBehavior, agent)
    copy.specialReactions = apply(agent.specialReactions, agent)
    copy.behaviors = agent.behaviors map { case (k, v) => k -> apply(v, agent) }
    copy.oldBehaviors = agent.oldBehaviors.clone()
    copy.partitionNo = agent.partitionNo
    copy.stash = agent.stash map { x => apply(x, agent) }

    copy
  }

  def apply(behavior: Behavior, agent: Agent): Behavior = {
    val copy = new Behavior(behavior.name, behavior.dropping)
    copy.setAgent(agent)
    copy.reactions = behavior.reactions map { case (k, v) => k -> apply(v, agent) }
    copy
  }

  def apply(action: Action, agent: Agent): Action = {
    val copy = new Action
    action.stmts map { x => copy + apply(x, agent, copy) }
    copy.executed = copy.stmts.take(action.executed.size)
    copy.toExecute = copy.stmts.takeRight(action.toExecute.size)
    copy.setAgent(agent)
    copy
  }

  def apply(stmt: Statement, agent: Agent, action: Action): Statement = {
    val copy: Statement = stmt match {
      case x: Ask if !x.isDynamic =>
        val s = Ask(apply(x.msgOut, agent), agent.ds.get(x.dstAgent.name))
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s.future = apply(x.future, agent)
        s
      case x: Ask if x.isDynamic =>
        val s = Ask(x.msgOutVar,
          x.dstAgentVar,
          x.futureVar,
          x.agentStateRead1,
          x.agentStateRead2,
          x.agentStateWrite)
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s.future = apply(x.future, agent)
        s
      case x: Send if !x.isDynamic =>
        val s = Send(apply(x.msgOut, agent),
          agent.ds.get(x.dstAgent.name))
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setMessage(apply(x.m, agent))
        s.setAgent(agent)
        s
      case x: Send if x.isDynamic =>
        val s = Send(x.msgOutVar,
          x.dstAgentVar,
          x.agentStateRead1,
          x.agentStateRead2)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setMessage(apply(x.m, agent))
        s.setAgent(agent)
        s
      case x: Become if !x.isDynamic =>
        val s = Become.apply(x.behaviorName, x.remember)
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s
      case x: Become if x.isDynamic =>
        val s = Become.apply(x.behaviorNameVar,
          x.rememberVar,
          x.agentStateRead1,
          x.agentStateRead2)
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s
      case x: UnBecome if !x.isDynamic =>
        val s = UnBecome.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: UnBecome if x.isDynamic =>
        val s = UnBecome.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: BootStrap if !x.isDynamic =>
        val s = BootStrap.apply(x.bootStrapped)
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setAction(action)
        s.setMessage(apply(x.m, agent))
        s
      case x: BootStrap if x.isDynamic =>
        val s = BootStrap(x.bootStrappedVar)
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setAction(action)
        s.setMessage(apply(x.m, agent))
        s
      case x: Create if !x.isDynamic =>
        val s = Create(x.childAgentName)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: Create if x.isDynamic =>
        val s = Create(x.childAgentNameVar, x.agentStateRead1)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: While if !x.isDynamic =>
        val body = x.body.dropRight(1) // drop the self reference not to cause infinite loop
        val s = While(x.condition)(body map { stmt => apply(stmt, agent, action: Action) }: _*) // splice
        s.body = s.body :+ s
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s
      case x: While if x.isDynamic =>
        val s = While(x.conditionVar, x.agentStateRead1)(x.body map { stmt => apply(stmt, agent, action: Action) }: _*) // splice
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s.setAction(action)
        s
      case x: Else if !x.isDynamic =>
        val s = Else.apply(x.body.map { stmt => apply(stmt, agent, action) }: _*)(apply(x.associatedWith, agent, action))
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: Else if x.isDynamic =>
        val s = Else.apply(x.conditionVar, x.agentStateRead1)(x.body.map { stmt => apply(stmt, agent, action) }: _*)(apply(x.associatedWith, agent, action))
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: ElseIf if !x.isDynamic =>
        val s = ElseIf.apply(x.condition)(x.body.map { stmt => apply(stmt, agent, action) }: _*)(apply(x.associatedWith, agent, action))
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: ElseIf if x.isDynamic =>
        val s = ElseIf.apply(x.conditionVar, x.agentStateRead1)(x.body.map { stmt => apply(stmt, agent, action) }: _*)(apply(x.associatedWith, agent, action))
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: If if !x.isDynamic =>
        val s = If.apply(x.condition)(x.body.map { stmt => apply(stmt, agent, action) }: _*)
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: If if x.isDynamic =>
        val s = If.apply(x.conditionVar, x.agentStateRead1)(x.body.map { stmt => apply(stmt, agent, action) }: _*)
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match {
          case None => s // self
          case Some(st) => st
        }
        s.associatedWithForSkipping = action.search(x.associatedWithForSkipping) match {
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: FunctionRecurse => // it is always dynamic in rerms of hte function housing this recursion
        val s = FunctionRecurse.apply(x.functionName)(
          x.frame.parameterList match {
            case x@Right(a) => x
            case x@Left(a) => Left(a.map { x => copySpecific(x, agent) })
          })
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: Function =>
        val s = Function.apply(x.functionName)(x.frame.parameterList)(x.body.map { x => apply(x, agent, action) }: _*)
        x.assignAttributesTo(s)
        s.frame = x.frame match{
          case null => null
          case x: ActivationFrame => apply(x,agent)
        }
        x.setAction(action)
        x.setAgent(agent)
        x.setMessage(apply(x.m, agent))
        s
      case x: TimedGet if !x.isDynamic =>
        val s = TimedGet(x.future,x.dstVariableName,x.timeout)
        x.assignAttributesTo(s) // re assigns te future, this is why we had to copy it later on
        s.future = apply(x.future, agent)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: TimedGet if x.isDynamic =>
        val s = TimedGet(x.futureVar, x.dstVariableName, x.timeout,x.timeoutVar,x.agentStateRead1,x.agentStateWrite)
        x.assignAttributesTo(s) // re assigns te future, this is why we had to copy it later on
        // s.future = apply(x.future, agent) // no need to copy, it is in the local-state
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: Get if !x.isDynamic =>
        val s = Get(x.future,x.dstVariableName)
        x.assignAttributesTo(s)
        s.future = apply(x.future,agent)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Get if x.isDynamic =>
        val s = Get(x.futureVar,x.dstVariableName,x.agentStateRead1,x.agentStateWrite)
        x.assignAttributesTo(s)
        // s.future = apply(x.future,agent) // no need to copy since it is in the local state
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Kill if !x.isDynamic =>
        val s = Kill(x.dstAgent)
        x.assignAttributesTo(s)
        s.dstAgent = agent.ds.get(x.dstAgent.name)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Kill if x.isDynamic =>
        val s = Kill.apply(x.dstAgentVar,x.agentStateRead1)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m, agent))
        s
      case x: Lock =>
        val s = Lock.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: UnLock =>
        val s = UnLock.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ReceiveModifyStateRef =>
        val s = ReceiveModifyStateRef(x.variableVar,x.valueVar, x.agentStateRead1,x.agentStateWrite)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ReceiveModifyState if !x.isDynamic =>
        val s = ReceiveModifyState(x.variable, x.value)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ReceiveModifyState if x.isDynamic =>
        val s = ReceiveModifyState(x.variableVar, x.valueVar)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ModifyStateRef =>
        val s = ModifyStateRef.apply(x.variableVar, x.variable, x.agentStateRead1,x.agentStateWrite)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ModifyState if !x.isDynamic && !x.isFunctional =>
        val s = ModifyState(x.variable, x.value)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ModifyState if !x.isDynamic && x.isFunctional =>
        val s = ModifyState(x.variable,x.valueFunc)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ModifyState if x.isDynamic && !x.isFunctional =>
        val s = ModifyState(x.variableVar, x.valueVar)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ModifyState if x.isDynamic && x.isFunctional =>
        val s = ModifyState(x.variableVar, x.valueVar, x.agentStateRead1, x.agentStateWrite)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: ResumeConsume =>
        val s = ResumeConsume.apply
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setAction(action)
        s.setMessage(apply(x.m,agent))
        s
      case x: StopConsume =>
        val s = StopConsume.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Return if !x.isDynamic && x.returnValue == null =>
        val s = Return.apply(x.associatedWith)
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match{
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Return if !x.isDynamic && x.returnValue != null =>
        val s = Return.apply(x.returnVariable, x.returnValue)(x.associatedWith)
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match{
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Return if x.isDynamic =>
        val s = Return.apply(x.returnVariable, x.returnValueVariable)(x.associatedWith)
        x.assignAttributesTo(s)
        s.associatedWith = action.search(x.associatedWith) match{
          case None => null
          case Some(st) => st
        }
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Start if !x.isDynamic =>
        val s = Start.apply(x.dstAgent,x.args)
        x.assignAttributesTo(s)
        s.dstAgent = agent.ds.get(x.dstAgent.name)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Start if x.isDynamic =>
        val s = Start.apply(x.dstAgentVar,x.argsVar,x.agentStateRead1,x.agentStateRead2)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Stop if !x.isDynamic =>
        val s = Stop.apply(x.dstAgent)
        x.assignAttributesTo(s)
        s.dstAgent = agent.ds.get(x.dstAgent.name)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Stop if x.isDynamic =>
        val s = Stop.apply(x.dstAgentVar,x.agentStateRead1)
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: UnStash =>
        val s = UnStash.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: UnStashAll =>
        val s = UnStashAll.apply
        x.assignAttributesTo(s)
        s.setAction(action)
        s.setAgent(agent)
        s.setMessage(apply(x.m,agent))
        s
      case x: Statement =>
        val s = new Statement
        x.assignAttributesTo(s)
        s.setAgent(agent)
        s.setAction(action)
        s.setMessage(apply(x.m, agent))
        s // don't forget to assign "taskFrom" in case the statement is already in the scheduler
    }
    copy
  }

  def apply(afr: ActivationFrame, agent: Agent): ActivationFrame = {
    val copy = new ActivationFrame
    copy.stack = agent.stack
    copy.parameterList = afr.parameterList match{
      case Left(x) => Left(x.map(copySpecific(_, agent)))
      case r@Right(x) => r
    }
    copy.localState = apply(afr.localState, agent) // copy it
    copy.popped = afr.popped
    copy.pushed = copy.pushed
    copy.parameterAccessPattern = afr.parameterAccessPattern
    copy
  }

  def apply(future: DummyFuture, agent: Agent): DummyFuture = {
    val copy = DummyFuture.apply(future.resolved,
      future.submittedTime,
      agent.ds.get(future.promisedBy.name),
      agent.ds.get(future.waitingFor.name)
    )
    copy.value = future.value
    copy.id = future.id
    copy.storedInVariable = future.storedInVariable
    copy
  }

  def apply(msg: Message, agent: Agent): Message = {
    val copy = new Message(msg.name)
    copy.sendMethod = msg.sendMethod
    copy.sender = copy.sender match{
      case null => null
      case x:Agent => agent.ds.get(msg.sender.name)
    }
    copy.direction = msg.direction
    copy.id = msg.id
    copy.isResponse = msg.isResponse
    copy.payload = msg.payload.map{x => copySpecific(x,agent)}
    copy
  }

  def apply(ls: LocalState, agent: Agent): LocalState = {
    val copy = LocalState.apply(ls.agentName)
    copy.garbageCollection = ls.garbageCollection.clone()
    copy.varToMem = ls.varToMem.clone()
    copy.memToMem = ls.memToMem.clone()
    copy.memToVal = ls.memToVal.map{
      case (k,v) if v.isInstanceOf[Iterator[_]] =>
        // find the variable in which this is stored
        val memToMemKey = copy.memToMem.find{case (k1,v1) => v == v1} match{
          case None => null // impossible situation
          case Some(x) => x
        }
        val varToMemKey = copy.varToMem.find{case (k2,v2) => v2 == memToMemKey._1} match{
          case None => null // another impossibility
          case Some(x) => x
        }
        // then duplicate
        val (iterator1, iterator2) = v.asInstanceOf[Iterator[_]].duplicate
        // store one copy in the ls(variable)
        ls(varToMemKey._1) = iterator1
        // return the other copy
        (k -> iterator2)
      case (k,v) => k -> copySpecific(v, agent)
    }
    copy
  }

  def copySpecific(any: Any, newAgent: Agent): Any = {
    import edu.utah.cs.gauss.serialization.IO.{toBytes, fromBytes}

    any match {
      case x: Agent => newAgent.ds.get(x.name)
      case x: DummyFuture => apply(x, newAgent)
      case m: Message => apply(m, newAgent)
      case x => fromBytes(toBytes(x))
    }
  }
}
