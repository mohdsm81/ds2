package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures._

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
class TestEchoTestBedAskWithGetTimeout extends MyTestSpecs {
info("""=================================================================""")
  info("""This test suite is an integration test suite. It starts with a 
    fresh instance of a distributed system (an Echo server and client with 
    possibility of blocking over a future with possibility of timing out 
    since it uses Get with timeout variation) and ending with testing the 
    whole pipeline of the runtime""")
info("""=================================================================""")
  val myDS = TestBeds.echoServerWithAskInstanceWithTimedGet

  test("Boot strapping a server") {
    // starting server, which does nothing in our test bed since it basically sends s, then j.
    myDS.bootStrap("server")
    myDS.get("server").q.head.name should be(new Start name)
    myDS.get("server").locked should be(true)
    myDS.get("server").q.head.sender should equal(myDS.bootStraper)
    myDS.get("server").q.head.sendMethod should be(false)
  }

  test("Boot strapping client") {
    // start client, which will send a Show message to server, the server in turn will print it
    myDS.bootStrap("client")

    myDS.get("client").q.head.name should be(new Start name)
    myDS.get("client").locked should be(true)
    myDS.get("client").q.head.sender should equal(myDS.bootStraper)
    myDS.get("client").q.head.sendMethod should be(false)
  }

  val scheduler = myDS.scheduler

  test("DistributedSystem and Schedulers know each other") {

    scheduler shouldNot be(null)
    scheduler should be(myDS.scheduler)
    myDS.scheduler should be(scheduler)
    scheduler.taskQ should be(empty)
    scheduler.consumeQ.size should be(0)
  }

  test("Scheduling all server tasks then all client's tasks") {
    // server has to be started so we schedule it twice (for start and for join)
    scheduler.schedule("server")
    scheduler.taskQ.size should be(1)
    //then schedule anything (in this case it is certainly the client)
    scheduler.schedule("client") // the start ===> generates another message to server
    scheduler.taskQ.size should be(2) // containing 2 tasks
  }

  var clientStmtsCnt = 0
  test("Consuming all tasks results in placing them in the consumeQ") {
    val serverStmtsCnt = scheduler.consumeATask("server")
    clientStmtsCnt = scheduler.consumeATask("client")

    scheduler.taskQ should be(empty) // contains no more tasks
    scheduler.consumeQ.size should be(serverStmtsCnt + clientStmtsCnt) // containing 4 SuspendableTasks
  }

  test("Executing each task") {

    // the server's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "server"}
    scheduler.consumeQ.size should be(clientStmtsCnt)

    // the client's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "client"}
    scheduler.consumeQ.size should be(0)
  }

  test("Scheduling the last pending task in server sent by client") {
    myDS.hasWork should be(Set(myDS.get("server")))
    myDS.get("server").q.head.name should be("Show")
    myDS.get("server").q.size should be(1)
    scheduler.schedule("server")
    myDS.get("server").q.size should be(0)
    scheduler.taskQ.size should be(2)
  }

  test("Consuming and executing the 'Show' task") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeATask("server")
    scheduler.consumeQ.size should be(cnt)

    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "server"}
    scheduler.consumeQ.size should be(0)
  }

  test("Client can't access its future to know if the server reacted, and client is blocked, due to server not resolving it") {
    scheduler.blockingMgr.blocked.size should be(1) // there is a task blocked
    myDS.get("client").q.size should be(0) // no resolving message is in q
  }

  test("Client resuming operation after timing out") {
    (1 to 11) foreach { _ => scheduler.tick } // time passes
    scheduler.timedActionsTracker.timedActions.size should be(0)
    scheduler.blockingMgr.blockedTasks.size should be(0)
    scheduler.taskQ.size should be(1)
  }

  test("Executing the only resuming task of the client after get timed-out") {
    val cnt = scheduler.consumeATask("client") // since its task has timed out
    scheduler.taskQ should be(empty)
    scheduler.consumeQ.size should be(cnt)
    scheduler.executeAll(1) // whatever remained, execute those stmts
    scheduler.consumeQ.size should be(0)
    scheduler.blockingMgr.blockedTasks.size should be(0)
    scheduler.taskQ.size should be(0)
  }

  // Note the following test-case was valid before "strict" timed-actions 
  // semantics, not it is NOT
  
  //  test("Timed action unblocked the client, message should be shown") {
  //    scheduler.consume
  //    scheduler.consumeQ.size should be (1)
  //    scheduler.execute(1)
  //    scheduler.consumeQ should be (empty)
  //  }

  // now we don't have a guaranteed-to-terminate DistributedSystem.stop method.
  // soon we will.

  test("Shutting down the system") {
    myDS.isShutdown should be(false)
    myDS.shutdownSystem
    myDS.isShutdown should be(true)
  }
}
