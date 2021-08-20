package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.MyTestSpecs

import scala.collection.mutable
import scala.collection.parallel.ParSet

// to avoid creating ir data structures
import edu.utah.cs.gauss.ds2.core.integration.TestBeds.echoServerInstance
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

class SnapshottingSuite extends MyTestSpecs {

  import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._

  //=================================
  // IR Datastructures
  //=================================

  info("=======================================")
  info("Data structures part of Copying Suite")
  info("=======================================")

  test("Action") {
    val act = new Action
    val code = (m: Message, a: Agent) => {}

    act + Statement(code) + Statement(code)
    act.reset

    act.executeOne

    //    val copy = act.copy
    val copy = act.runtimeCopy
    copy should equal(act)
  }

  test("Action2") {
    val a = actionInstance2
    //    val c = a.copy
    val c = a.runtimeCopy
    c should equal(a)
  }

  test("Action3") {
    val a = actionInstance
    //    val c = a.copy

    val c = a.runtimeCopy

    // DEBUG
    //    val h1 = a.hashCode()
    //    val h2 = c.hashCode()
    //    
    //    assert(h1 == h2)

    c should equal(a)
  }

  test("Agent") {
    val a = agentInstance
    val state = a.snapshot

    a.q = Seq(new Start) // messing around before we restore it
    a.oldBehaviors = mutable.Stack("whateverBehavior", "behavior2")

    assert(state.q != a.q)
    assert(state.oldBehaviors != a.oldBehaviors)

    state.restore // restores a to its original state
    state.q should equal(a.q)
    state.oldBehaviors should equal(a.oldBehaviors)
  }

//  test("Behavior") {
//
//    val b = behaviorInstance
//    val copy = b.copy
//
//    copy should equal(b)
//
//  }

//  test("Message") {
//
//    // test 1
//    val m = new Message
//    val copy = m.copy
//
//    assert(m.hashCode == copy.hashCode)
//
//  }

  test("DummyFuture") {
    val f = futureInstance
    val state = f.snapshot

    f.value = "yay"
    f.resolved = true

    assert(state.value != f.value)
    assert(state.resolved != f.resolved)

    state.restore

    state.resolved should equal(f.resolved)
    state.value should equal(f.value)
  }

  test("RuntimeTimedAction") {
    val ta = timedActionInstance
    val c = ta.copy

    ta should equal(ta)

  }

//  test("Statement") {
//    // Note that the 'code' need not be copied, it stays the same all the time! so we reference it instead
//    // any Unit in that matter
//
//    val s = statementInstance
//    //    s.link(distributedSystemWithSchedulerInstance)
//    val c = s.copy
//
//    c should equal(s)
//
//  }
  //
  test("SuspendableTask") {
    val t = taskInstance
    val state = t.snapshot
    t.suspended = true
    t.isTimed = false
    t.action = actionInstance2

    assert(state.suspended != t.suspended)
    assert(state.isTimed != t.isTimed)
    assert(state.action.hashCode() != t.action.hashCode)

    state.restore

    state.suspended should equal(t.suspended)
    state.isTimed should equal(t.isTimed)
  }

  test("DistributedSystem-1") {

    val ds = distributedSystemWithSchedulerInstance

    val state = ds.snapshot

    ds + {val a = agentInstance; a.name = "duuuude"; a}

    assert(state.agentsStates.size == 3)
    assert(ds.agents.size == 4)

    state.restore

    assert(ds.agents.size == state.agentsStates.size )
  }

  test("DistributedSystem-2") {
    val ds1 = echoServerInstance
    val state = ds1.snapshot
    ds1.agents = ds1.agents.drop(1) // after this step, ds1 and ds2 must be identical
    assert(ds1.agents.size != state.agentsStates.size)

    state.restore

    assert(state.agentsStates.size == ds1.agents.size)
    assert(state.instanceToRestore == ds1)
  }

  //=================================
  // Scheduler-related
  //=================================

  info("Scheduler part of Snapshotting Suite")

  test("BlockingTasksManager") {
    val btm = distributedSystemWithSchedulerInstance.scheduler.blockingMgr

    val state = btm.snapshot

    btm.blockedTasks = Set(new SuspendableTask(actionInstance2))

    assert(state.blockedTasks.size != btm.blockedTasks.size)

    state.restore

    assert(btm.blockedTasks.size == state.blockedTasks.size)
  }

  test("Copying a TimedActionsTracker") {
    val tat = distributedSystemWithSchedulerInstance.scheduler.timedActionsTracker
    val state = tat.snapshot

    tat.timedActions = ParSet(new TimedAction( 1, 2, actionInstance2, 3,0))

    assert(tat.timedActions.size != state.timedActions.size)

    state.restore

    assert(tat.timedActions.size == state.timedActions.size)
  }

  test("SchedulerState-1") {
    // just its inner state, no need for the algorithm copying
    val ds = distributedSystemWithSchedulerInstance
    val scheduler = ds.scheduler

    val state = scheduler.snapshot

    scheduler.clk = 1023
    scheduler.consumeQ = Seq(statementInstance,statementInstance)
    scheduler.attach(echoServerInstance)

    assert(state.clk != scheduler.clk)
    assert(state.consumeQ != scheduler.consumeQ)

    state.restore

    assert(state.clk == scheduler.clk)
    assert(state.consumeQ == scheduler.consumeQ)

    // yup, ds is not part of the scheduler state, it is just a reference to a DS with its own state.
    assert(scheduler.ds != null)
    assert(scheduler.ds == echoServerInstance)

  }

//  test("LocalState") {
//    val ls = localStateInstance("coolest")
//
//    val ds = distributedSystemWithSchedulerInstance
//
//    val lsCopy = ls.copy
//    lsCopy.link(ds)
//
//    //    lsCopy should equal (ls)
//    ls.varToMem.keySet map { x =>
//      if (!ls(x).isInstanceOf[Agent])
//        ls[Any](x) should equal(lsCopy(x))
//      else
//        ls[Agent](x).equalsDEBUG(lsCopy(x).asInstanceOf[Agent])
//    }
//  }
}
