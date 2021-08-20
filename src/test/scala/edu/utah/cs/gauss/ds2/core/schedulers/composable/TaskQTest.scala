package edu.utah.cs.gauss.ds2.core.schedulers.composable

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.schedulers.TaskQ
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.Get
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

class TaskQTest extends MyTestSpecs {
  info("==========================================")
  info("TaskQ Suite")
  info("==========================================")

  //    Setting up the testbed
  val ds = distributedSystemWithAskInstance
  val sch = ds.scheduler
  val tq = sch.taskQ
  val btm = sch.blockingMgr
  var numStmts = 0
  ds.get("server").locked = false // unlocking so that client.START.ASK doesn't fail

  // consumeQ gets replaced each time, it should be tested directly on a 'sch' ref
  //  val cq = sch.consumeQ

  ds.bootStrap("client")

  test("Scheduling") {
    tq should be(empty)
    sch.schedule("client")
    tq.size should be(1)
  }

  test("Consuming - a task") {
    numStmts = sch.consumeATask("client")
    tq should be(empty)
    sch.consumeQ.size should be(numStmts)
  }

  test("Execution") {
    sch.execute(1) // first UNLOCK
    sch.consumeQ.size should be(numStmts - 1)
    sch.execute(1) // second UNLOCK
    sch.consumeQ.size should be(numStmts - 2)
    sch.execute(1) // ASK
    sch.consumeQ.size should be(numStmts - 3)
    sch.execute(1) // GET
         
    // this will simply not be the case, look at the above comment.
    //    sch.execute(1)  // NONE -- printing happy
    //    sch.consumeQ.size should be (sch.consumeQ.size - 4) 
  }

  test("PreEmption + interaction with Blocking Manager") {
    sch.consumeQ.size should be(0) // why? because now the task is pre-empted so any pending statement is put back where it belongs
    tq.size should be(1) // one task whose statements just has been pre-empted
    tq.countStmts("client") should be(numStmts - 3)
    tq.countStmtsHeadTask("client") should be(numStmts - 3) // double checking the methods work similarly

    // now the blocking manager MUST have one blocked task
    btm.blocked.size should be(1)
    btm.blocked should contain(tq("client").head)
    tq("client").head.action.toExecute.size should be(numStmts - 3)
    // because the statement that blocked the task is the one that should execute next if resolved/timed-out, GET or GET_TIMED.
//    tq("client").head.action.toExecute.head.kind should be (Statement.Kind.GET)
    tq("client").head.action.toExecute.head.isInstanceOf[Get]
//    tq("client").head.action.toExecute.last.kind should be (Statement.Kind.NONE)
    tq("client").head.action.toExecute.last.isInstanceOf[Statement]
  }
}