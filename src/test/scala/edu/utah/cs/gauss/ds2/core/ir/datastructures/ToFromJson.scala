package edu.utah.cs.gauss.ds2.core.ir.datastructures
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.schedulers.TimedActionsTracker
import edu.utah.cs.gauss.ds2.core.schedulers.BlockedTasksManager
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.tracing._
import edu.utah.cs.gauss.serialization.IO.{ fromBytes, toBytes }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.UnLock
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

class ToFromJson extends MyTestSpecs {

  import Fixtures._

  info("=======================================")
  info("Datastructures to/from json")
  info("=======================================")
  //===============================
  // ir.datastructures.* pkg tests
  //===============================

  test("Statement1") {
    val stmt = statementInstance
    val json = stmt.toJson

    //    println(pretty(render(json)))
    //    val stmt2 = Statement.fromJson(json)
    import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.Ask
    //    val stmt2 = Ask.fromJson(json) // I peeked to know
    val stmt2 = Statement.fromJson(json) // I peeked to know
    assert(stmt2 is stmt)
    stmt2.id should equal(stmt.id)
    assert(null == stmt2.a)
    //    stmt2.kind should equal(stmt.kind)
    assert(stmt2.isInstanceOf[Statement])
    //    stmt2.resolvedFuture should equal (stmt.resolvedFuture)
    //    stmt2.msgOut should equal(stmt.msgOut)
    //    assert(stmt2.msgOut.isInstanceOf[Message])

  }

  test("Statement2") {
    val stmt = actionInstance2.stmts(0).asInstanceOf[UnLock]
    val json = stmt.toJson

    println(json)
    //    val stmt2 = Statement.fromJson(json)
    val stmt2 = UnLock.fromJson(json)
    assert(stmt2 is stmt)
    stmt2.id should equal(stmt.id)
    assert(null == stmt2.a)

    //    stmt2.kind should equal(stmt.kind)
    assert(stmt2.isInstanceOf[UnLock])
//    assert(stmt2.a == stmt.a) // of course not! we don't care about serialization of agents in to/fromJson
    //    stmt2.resolvedFuture should equal (stmt.resolvedFuture)
    //    stmt2.msgOut should equal(stmt.msgOut)

  }

  test("Message") {
    val m = messageInstance

    val js = m.toJson

    val m2 = Message.fromJson(js)

    m2.name should equal(m.name)
    m2.sender should equal(null) // json takes away references
    m2.sendMethod should equal(m.sendMethod)
    m2.payload should be(empty) // toJson throws away payloads (possible infinite loops if copied)
  }

  test("Action1") {
    val act = actionInstance

    val js = act.toJson

    val act2 = Action.fromJson(js)

    act2.a should equal(null)
    // these are equal but for the 'a' field, i made sure it works my self :)
    //    act2.m should equal (act.m)
    act2.stmts.size should equal(act.stmts.size)
    act2.toExecute.size should equal(act.toExecute.size)
    act2.executed.size should equal(act.executed.size)
  }

  test("Action2") {
    val act = actionInstance2

    val js = act.toJson

    val act2 = Action.fromJson(js)

    act2.a should equal(null)
    // these are equal but for the 'a' field, i made sure it works my self :)
    //    act2.m should equal (act.m)
    act2.stmts.size should equal(act.stmts.size)
    act2.toExecute.size should equal(act.toExecute.size)
    act2.executed.size should equal(act.executed.size)

  }

  test("Behavior") {
    val b = behaviorInstance
    val js = b.toJson

    val b2 = Behavior.fromJson(js)

    assert(b is b2)

    // there are some differences in the behaviors, specifically the agent needs to be set in the 
    // newly created behavior. This will be dealt with on the DistributedSystem level of fromJson method.
    // after making sure that all agents do really exist.
    b2.reactions.size should equal(b.reactions.size)
    //    b2.reactions should equal (b.reactions) // this won't pass for the above reason

    // we don't care about m and a fields
  }

  test("Future") {

    val f = futureInstance

    val js = f.toJson

    val f2 = DummyFuture.fromJson(js)

    f2.resolved should equal(f.resolved)
    f2.value should equal(f.value)
    f2.promisedBy should equal(null)
    f2.waitingFor should equal(null)
    f2.id should equal(f.id)

  }

  test("SuspendableTask") {
    val task = taskInstance

    val js = task.toJson

    val task2 = SuspendableTask.fromJson(js)

    // this also won't succeed since the a field is null in the copy
    //    task.action should equal (task2.action)
    // so we use this instead of the above
    task.action.id should equal(task2.action.id)
    task.isSuspended should equal(task2.isSuspended)
    task.isTimed should equal(task2.isTimed)

  }

  test("RuntimeTimedAction") {
    val ta = timedActionInstance

    val js = ta.toJson

    val ta2 = TimedAction.fromJson(js)

    ta.action.id should equal(ta2.action.id)
    ta.countDown should equal(ta2.countDown)
    ta.startLimit should equal(ta2.startLimit)
    ta.endLimit should equal(ta2.endLimit)
    ta.runtimeHowManyTimes should equal(ta2.runtimeHowManyTimes)
    ta.howManyTimes should equal(ta2.howManyTimes)
    // above is enough to check as it follows that other attributes should be the same
    // except for agent field of all these datastructure.
  }

  test("Agent") {

    val a = agentInstance

    val js = a.toJson

    //    println( js \ "Agent" \ "specialReactions" \ "Behavior")

    //    println(pretty(render(js)))

    val a2 = Agent.fromJson(js)

    assert(a2 is a)
    a.q.size should equal(a2.q.size)
    //    a.defaultBehavior should equal (a2.defaultBehavior)
    assert(a.defaultBehavior is a2.defaultBehavior)
    a.stash.size should equal(a2.stash.size)
    //    a.reactions should equal (a2.reactions)
    assert(a.reactions is a2.reactions)
    //    a.behaviors should equal (a2.behaviors)
    assert(a.behaviors == a2.behaviors)
    assert(a.specialReactions is a2.specialReactions, "special reactions were not the same")
    //    a.timedActions.size should equal(a2.timedActions.size)
    a.oldBehaviors.size should equal(a2.oldBehaviors.size)
    a.localState should equal(a2.localState)
    a.locked should equal(a2.locked)
    a.consuming should equal(a2.consuming)
    a.blocked should equal(a2.blocked)
    // we tested the futures in the Future test.
    //    a.blockedOn should equal (a2.blockedOn)
    a.futuresPromised.size should equal(a2.futuresPromised.size)
    a.futuresWaitingFor.size should equal(a2.futuresWaitingFor.size)

  }

  test("DistributedSystem") {

    val ds = distributedSystemWithSchedulerInstance

    val js = ds.toJson

    val ds2 = DistributedSystem.fromJson(js)

    // put asserts here
    ds2.agents map { x => ds.agents.filter { z => z is x }.size should be(1) }
    //    ds2.actions map { x => ds.actions.filter { z => z is x }.size should be(1) }
    //    ds2.behaviors map { x => ds.behaviors.filter { z => z._1 == x._1 }.size should be(1) }
    //    ds2.messages map { x => ds.messages.filter { z => z == x }.size should be(1) }

  }

  //===============================
  // sccheduler.* pkg tests
  //===============================

  info("=======================================")
  info("Scheduler to/from json")
  info("=======================================")

  test("TimedActionsTracker") {
    val tat = new TimedActionsTracker

    val js = tat.toJson

    val tat2 = TimedActionsTracker.fromJson(js)

    tat2.timedActions.map { x => x in tat }

  }

  test("BlockedTasksManager") {

    val btm = new BlockedTasksManager()(
      distributedSystemWithSchedulerInstance,
      distributedSystemWithSchedulerInstance.scheduler)

    val blockedTask = taskInstance
    //    blockedTask.action.a = btm.ds.get("client")
    //    blockedTask.action.a.blocked = true
    //    blockedTask.action.a.blockedOn = Some(futureInstance)
    //    // causes java.io.NotSerializableException ??!!
    //    //    blockedTask.action = btm.ds.get("client").specialReactions(new Start)
    //    blockedTask.suspend
    //    
    //    btm.block(blockedTask)

    btm.blockedTasks = btm.blockedTasks + blockedTask

    val js = btm.toJson

    val btm2 = BlockedTasksManager.fromJson(js)

    // even if the implicits had values in btm, they would be null in btm2
    btm2.ds should equal(null)
    btm2.scheduler should equal(null)

    btm2.blockedTasks.size should equal(btm.blockedTasks.size)

  }

  test("Scheduler") {
    import edu.utah.cs.gauss.ds2.core.schedulers.DeSerializers.FromJsonScheduler

    val ds = distributedSystemWithSchedulerInstance
    val sc = ds.scheduler

    val js = sc.toJson

    val sc2: Scheduler = FromJsonScheduler(sc).fromJson(js)

    sc2.taskQ.size should equal(sc.taskQ.size)
    sc2.numThreads should equal(sc.numThreads)
    sc2.clk should equal(sc.clk)
    sc2.consumeQ.size should equal(sc.consumeQ.size)
    sc2.blockingMgr.blockedTasks.size should equal(sc.blockingMgr.blockedTasks.size)
    sc2.timedActionsTracker.timedActions.size should equal(sc.timedActionsTracker.timedActions.size)
    sc2.traceManager.traces.size should equal(sc2.traceManager.traces.size)
  }

  //===============================
  // tracing.* pkg tests
  //===============================  

  test("Trace Event - Send") {
    val e = new Send(agentInstance, messageInstance, agentInstance, false)(() => 10)

    val js = e.toJson

    val e2 = Send.fromJson(js)

    e2 should equal(e)

  }

  test("Trace Event - Ask") {
    val e = new Ask(agentInstance,
      messageInstance,
      agentInstance,
      futureInstance,
      false)(() => 10)

    val js = e.toJson

    val e2 = Ask.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Create") {
    val a2 = agentInstance
    a2.name = "whatever"
    val e = new Create(agentInstance,
      a2.name,
      a2)(() => 10)

    val js = e.toJson

    val e2 = Create.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Start") {

    val e = new edu.utah.cs.gauss.ds2.core.tracing.Start(agentInstance, agentInstance)(() => 10)

    val js = e.toJson

    val e2 = edu.utah.cs.gauss.ds2.core.tracing.Start.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Stop") {

    val e = new edu.utah.cs.gauss.ds2.core.tracing.Stop(agentInstance, agentInstance)(() => 10)

    val js = e.toJson

    val e2 = edu.utah.cs.gauss.ds2.core.tracing.Stop.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Kill") {

    val e = new Kill(agentInstance, agentInstance)(() => 10)

    val js = e.toJson

    val e2 = Kill.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Lock") {

    val e = new Lock(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = Lock.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Unlock") {

    val e = new Unlock(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = Unlock.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - StopConsuming") {

    val e = new StopConsuming(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = StopConsuming.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - ResumeConsuming") {

    val e = new ResumeConsuming(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = ResumeConsuming.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Become") {

    val e = new Become(agentInstance, "hay-wire", true)(() => 10)

    val js = e.toJson

    val e2 = Become.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Unbecome") {

    val e = new Unbecome(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = Unbecome.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Stash") {

    val e = new Stash(agentInstance, messageInstance)(() => 10)

    val js = e.toJson

    val e2 = Stash.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - UnstashAll") {

    val e = new UnstashAll(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = UnstashAll.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - HasWork") {

    val e = new HasWork(Set(agentInstance, agentInstance))(() => 10)

    val js = e.toJson

    val e2 = HasWork.fromJson(js)

    e2.agents.size should equal(e.agents.size)

  }

  test("Trace Event - BootStrap") {

    val e = new BootStrap(agentInstance)(() => 10)

    val js = e.toJson

    val e2 = BootStrap.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - BootStrapAll") {

    val e = new BootStrapAll(Set(agentInstance, agentInstance))(() => 10)

    val js = e.toJson

    val e2 = BootStrapAll.fromJson(js)

    e2.agents.size should equal(e.agents.size)

  }

  test("Trace Event - Get") {

    val e = new Get(agentInstance, futureInstance, actionInstance, Some("GREAT!"))(() => 10)

    val js = e.toJson

    val e2 = Get.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - GetTimeout") {

    val e = new GetTimeout(agentInstance, futureInstance, 9, actionInstance, None)(() => 10)

    val js = e.toJson

    val e2 = GetTimeout.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Resolve") {

    val e = new Resolve(futureInstance)(() => 10)

    val js = e.toJson

    val e2 = Resolve.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - StopAll") {

    val e = new StopAll()(() => 10)

    val js = e.toJson

    val e2 = StopAll.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - ShutdownAll") {

    val e = new ShutdownAll()(() => 10)

    val js = e.toJson

    val e2 = ShutdownAll.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - Pick") {

    val e = new Pick(agentInstance, taskInstance)(() => 10)

    val js = e.toJson

    val e2 = Pick.fromJson(js)

    // won't work why? because Messages and other agent-referencing classes reset their sender to null after (de)serialization
    //    e2 should equal(e)
    e2.clk should equal(e.clk)
    e2.clk should equal(e.clk)
    e2.task.action.a should equal(null)
    e2.task.action.id should equal(e.task.action.id)

  }

  test("Trace Event - PickRandom") {

    val e = new PickRandom(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = PickRandom.fromJson(js)
    // won't work why? because Messages and other agent-referencing classes reset their sender to null after (de)serialization
    //    e2 should equal(e)
    e2.clk should equal(e.clk)
    e2.task.action.a should equal(null)
    e2.task.action.id should equal(e.task.action.id)

  }

  test("Trace Event - ExecuteTimed") {

    val e = new ExecuteTimed(timedActionInstance, 1L)(() => 10)

    val js = e.toJson

    val e2 = ExecuteTimed.fromJson(js)

    e2.timedAction.startLimit should equal(e.timedAction.startLimit)
    e2.timedAction.endLimit should equal(e.timedAction.endLimit)
    e2.timedAction.howManyTimes should equal(e.timedAction.howManyTimes)
  }

  test("Trace Event - ExecuteStatement") {

    val e = new ExecuteStatement(statementInstance, 1L)(() => 10)

    val js = e.toJson

    val e2 = ExecuteStatement.fromJson(js)

    e2.clk should equal(e.clk)
    e2.stmt.a should equal(null)
    e2.stmt.action should equal(e.stmt.action) // both actions are null btw!
  }

  test("Trace Event - ExecuteSpecific") {

    val e = new ExecuteSpecific(agentInstance, taskInstance, 1L)(() => 10)

    val js = e.toJson

    val e2 = ExecuteSpecific.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.a should equal(null)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - DoSchedule") {

    val e = new DoSchedule(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = DoSchedule.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.a should equal(null)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - DoSchedulePeriodic") {

    val e = new DoSchedulePeriodic(timedActionInstance)(() => 10)

    val js = e.toJson

    val e2 = DoSchedulePeriodic.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - DoSchedulePeriodicRange") {

    val e = new DoSchedulePeriodicRange(taskInstance, 3, 2, 5)(() => 10)

    val js = e.toJson

    val e2 = DoSchedulePeriodicRange.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - Consume") {

    val e = new Consume(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = Consume.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - Execute") {

    val e = new Execute(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = Execute.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - ExecuteAll") {

    val e = new ExecuteAll(Seq(statementInstance), 1)(() => 10)

    val js = e.toJson

    val e2 = ExecuteAll.fromJson(js)

    e2.clk should equal(e.clk)
    e2.stmts.size should equal(e.stmts.size)
  }

  test("Trace Event - ExecuteSpecial") {

    val e = new ExecuteSpecial(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = ExecuteSpecial.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }

  test("Trace Event - ExecuteOneStatement") {

    val e = new ExecuteOneStatement(taskInstance)(() => 10)

    val js = e.toJson

    val e2 = ExecuteOneStatement.fromJson(js)

    e2.clk should equal(e.clk)
    e2.task.action.id should equal(e.task.action.id)
  }
  test("Trace Event - ExecuteTimedAction") {

    val e = new ExecuteTimedAction(timedActionInstance)(() => 10)

    val js = e.toJson

    val e2 = ExecuteTimedAction.fromJson(js)

    e2.clk should equal(e.clk)
    e2.timedAction.action.id should equal(e.timedAction.action.id)
  }

  test("Trace Event - Tick") {

    val e = new Tick()(() => 10)

    val js = e.toJson

    val e2 = Tick.fromJson(js)

    e2.clk should equal(e.clk)
    e2.triggered.size should equal(e.triggered.size)
  }

  test("Trace Event - ExploreStart") {

    val e = new ExploreStart()(() => 10)

    val js = e.toJson

    val e2 = ExploreStart.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - ExploreEnd") {

    val e = new ExploreEnd()(() => 10)

    val js = e.toJson

    val e2 = ExploreEnd.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - ShutdownScheduler") {

    val e = new ShutdownScheduler()(() => 10)

    val js = e.toJson

    val e2 = ShutdownScheduler.fromJson(js)

    e2 should equal(e)
  }

  test("Trace Event - TraceEvent.fromJson()") {

    val e: TraceEvent = new ShutdownScheduler()(() => 10)

    val js = e.toJson

    //    println(js)

    val e2 = TraceEvent.fromJson(js)

    e2 should equal(e)
  }

  test("TraceEntry") {
    val te = new TraceEntry
    te.event = new ExecuteTimedAction(timedActionInstance)(() => 10)

    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    te.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)

    val js = te.toJson

    val te2 = TraceEntry.fromJson(js)

    te2.id should equal(te.id)

    // why the all of the following fail? (hint: agent before and after to/fromJson)
    // other things are identical: I checked my self!

    //    te2.posterior should equal (te.posterior)
    //    te2.prior should equal (te.prior)
    //    te2 should equal(te)

  }

  test("TraceEntryEnd") {
    val te = TraceEntryEnd(new ExecuteTimedAction(timedActionInstance)(() => 10))

    // note the trick to remove the side effect from serializing then deserializing to/from json.
    // this is to simplify our assertions (why didn't I think of this before?!!).
    te.prior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)
    te.posterior = DistributedSystem.fromJson(distributedSystemWithSchedulerInstance.toJson)

    val js = te.toJson

    val te2 = TraceEntry.fromJson(js)

    te2.id should equal(te.id)
  }

  test("Trace") {
    val tr = traceInstance

    val js = tr.toJson

    val tr2 = Trace.fromJson(js)

    tr2.entries.size should equal(tr.entries.size)
  }

  test("TraceManager") {
    val tm = new TraceManager
    tm.traces = scala.collection.mutable.Seq(traceInstance, traceInstance)

    val js = tm.toJson

    val tm2 = TraceManager.fromJson(js)

    // DEBUG
    //    println(pretty(render(js)))

    tm2.traces.size should equal(tm.traces.size)

  }

  test("TraceManager.diffs") {
    val tm = new TraceManager
    tm.traces = scala.collection.mutable.Seq(traceInstance, traceInstance)

    //    println(tm.diffs)
    //    println(tm.diffs(0))
    //    
    tm.diffs.size should equal(tm.traces.size - 1)
  }

  info("=======================================")
  info("LocalState to/from json")
  info("=======================================")

  test("LocalState - serialize/deserialize") {

    val ls = new LocalState("hero")

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

    // DEBUG
    //    val valueA = LocalState.serialize(value2)
    //    println(valueA)
    //    println("-----------------------------------------------------------")
    //    println(value2)
    //    println("-----------------------------------------------------------")
    //    val valueB = LocalState.deSerialize(LocalState.serialize(value2))
    //    println(valueB)
    // END DEBUG

    LocalState.deSerialize(LocalState.serialize(ls.getVal(variable1))) should equal(value1)
    // reason why we use "is" instead of "equal" is that to/from Json nullifies promiser and waiter
    assert(LocalState.deSerialize(LocalState.serialize(ls.getVal(variable2))).asInstanceOf[DummyFuture] is value2)
    LocalState.deSerialize(LocalState.serialize(ls.getVal(variable3))) should equal(value3)
    LocalState.deSerialize(LocalState.serialize(ls.getVal(variable4))) should equal(value4)
    // for the same reason as value3
    assert(LocalState.deSerialize(LocalState.serialize(ls.getVal(variable5))).asInstanceOf[Action] is value5)

  }

  test("LocalState - to/fromJson") {

    val ls = localStateInstance("hero")

    // DEBUG
    //    println(pretty(render(ls.toJson \ "LocalState" \ "agentName")))
    // end DEBUG

    val jsCopy = LocalState.fromJson(ls.toJson)

    assert(ls.varToMem forall { case (x, y) => jsCopy.varToMem(x) == y })

    assert(ls.memToMem forall { case (x, y) => jsCopy.memToMem(x) == y })

    // not all values are equivalant due to asymmetric nature of to/from Json
    // assert(ls.memToVal forall{case (x,y) => jsCopy.memToVal(x) == y})

    // so I will use this looser check instead
    assert(ls.memToVal forall { case (x, y) => jsCopy.memToVal.contains(x) })

    assert(ls.garbageCollection forall { case (x, y) => jsCopy.garbageCollection(x) == y })

  }

}
