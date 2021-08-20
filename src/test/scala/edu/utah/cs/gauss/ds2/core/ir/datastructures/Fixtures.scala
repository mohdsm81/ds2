package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.tracing.TraceEntryEnd
import edu.utah.cs.gauss.ds2.core.tracing.ExecuteTimedAction
import edu.utah.cs.gauss.ds2.core.tracing.Send
import edu.utah.cs.gauss.ds2.core.tracing.TraceEntry
import edu.utah.cs.gauss.ds2.core.tracing.Trace
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.History
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.LinearizabilityInvocation
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured.LinearizabilityResponse

object Fixtures {
  def statementInstance: Statement = {
    val code = (m: Message, a: Agent) => println("Goooood")
    val stmt = Statement(code)    
//    val ask = new Ask
//    stmt.code = code

    stmt
  }

  def messageInstance: Message = {
    val agent = new Agent("cool")
    val msg = new Message("Show", agent, true)
    msg
  }

  def actionInstance: Action = {
    val act = new Action

    val code = (m: Message, a: Agent) => println("yay!")

    act + Statement(code) // + Statement(code)

    act.setAgent(new Agent("cooler"))
    act.setMessage(messageInstance)

    act
  }

  def localStateInstance(agentName: String): LocalState = {
    val ls = new LocalState(agentName)

    val value1 = agentInstance
    val value2 = futureInstance
    val value3 = 3
    val value4 = "what ever to byte"
    val value5 = actionInstance2

    val variable1 = "var1" + DELIM + value1.getClass.getSimpleName
    val variable2 = "var2" + DELIM + value1.getClass.getSimpleName
    val variable3 = "var3" + DELIM + value1.getClass.getSimpleName
    val variable4 = "var4" + DELIM + value1.getClass.getSimpleName
    val variable5 = "var5" + DELIM + value1.getClass.getSimpleName

    ls.setVar(variable1, value1)
    ls.setVar(variable2, value2)
    ls.setVar(variable3, value3)
    ls.setVar(variable4, value4)
    ls.setVar(variable5, value5)

    ls
  }

  def actionInstance2: Action = {
    distributedSystemWithSchedulerInstance.get("client").onStart(new Start)
  }

  def behaviorInstance: Behavior = {
    val b = new Behavior("whatever")

    b += (messageInstance, actionInstance)
    b += (messageInstance, actionInstance)

  }

  def agentInstance: Agent = {
    val a = new Agent("coolest")
    a.q = a.q ++ Seq(messageInstance, messageInstance)

    a.reactions = behaviorInstance
    a.defaultBehavior = behaviorInstance
    a
  }

  def futureInstance: DummyFuture = {
    // this instance can not happen in any distributed system
    // Only that I wanted to test the most extreme cases
    new DummyFuture(false, 4, agentInstance, null)
  }

  def taskInstance: SuspendableTask = {
    val act = actionInstance
    act.setAgent(agentInstance)
    val task = new SuspendableTask(act)
    task.isTimed = true
    task
  }

  def timedActionInstance: TimedAction = {
    new TimedAction(1, 5, actionInstance, 6, 10)
  }

  def distributedSystemWithSchedulerInstance: DistributedSystem = {

    import edu.utah.cs.gauss.ds2.core.integration.TestBeds._

    val ds = echoServerInstance

    ds + agentInstance
  }
  
  def distributedSystemWithAskInstance: DistributedSystem = {
    edu.utah.cs.gauss.ds2.core.integration.TestBeds.echoServerWithAskInstance
  }

  def traceEntry1: TraceEntry = {
    val te = TraceEntryEnd(new ExecuteTimedAction(timedActionInstance)(() => 10))

    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    te.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te
  }

  def traceEntry2: TraceEntry = {
    val te = TraceEntryEnd(new Send(agentInstance, messageInstance, agentInstance)(() => 10))

    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    te.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te
  }

  def traceInstance: Trace = {
    val tr = new Trace
    tr += traceEntry1
    tr += traceEntry2
    tr
  }


  def linearizabilityInvocation: LinearizabilityInvocation = {
    val li = new LinearizabilityInvocation
    li.event = new ExecuteTimedAction(timedActionInstance)(() => 10)

    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    li.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    li.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    li
  }

  def linearizabilityResponse: LinearizabilityResponse = {
    val lr = LinearizabilityResponse(linearizabilityInvocation)
    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    lr.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    lr.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    lr
  }

  def historyInstance: History = {
    val h = new History
    h += linearizabilityInvocation
    h += linearizabilityResponse
    h
  }

}
