package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures._

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
//@Ignore
class TestEchoTestBedAskCopy extends MyTestSpecs {
  info("""=================================================================""")
  info("""This test suite is an integration test suite. It starts with a 
    fresh instance of a distributed system (an Echo server and client with 
    possibility of blocking over a future) and ending with testing the whole 
    pipeline of the runtime""")
  info("""=================================================================""")
  val myDS = TestBeds.echoServerWithAskInstanceCopy

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
    scheduler.taskQ.size should be(2) // containing 4 SuspendableTasks
  }

  var clientStmtsCnt = 0
  test("Consuming all tasks results in placing them in the consumeQ") {
    val serverStmtsCnt = scheduler.consumeATask("server")
    clientStmtsCnt = scheduler.consumeATask("client")

    scheduler.taskQ should be(empty) // contains no more tasks
    scheduler.consumeQ.size should be(6) // containing 2 SuspendableTasks, that are 6 statements in total
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

  test("Client trying to access its future to know if the server reacted, and client is blocked") {
    scheduler.blockingMgr.blocked.size should be(1) // there is a task blocked
    myDS.get("client").q.size should be(1) // the resolving message is still in q
    scheduler.schedule("client") // Should invoke a call to scheduler.handle(future)
    myDS.get("client").q.size should be(0)
    scheduler.taskQ.size should be(1)
  }

  test("Client resuming operation after server resolved the future it was blocked on") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeATask("client")
    scheduler.consumeQ.size should be(cnt)
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "client"} // future should be resolved by now
    scheduler.consumeQ.size should be(0)
  }

  // the above does all that is needed. So the following commented test isn't needed
  
//  test("Resuming the client suspended task after the future has been resolved") {
//
//    scheduler.taskQ.size should be(1)
//    scheduler.consumeATask("client") // the unblocked/resumed task from client
//    scheduler.taskQ should be(empty)  
//    scheduler.consumeQ.size should be(1)
//    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "client"} // client is happy
//    scheduler.consumeQ should be(empty)
//  }

  // now we don't have a guaranteed-to-terminate DistributedSystem.stop method.
  // soon we will.

  test("Shutting down the system") {
    myDS.isShutdown should be(false)
    myDS.shutdownSystem
    myDS.isShutdown should be(true)
  }
}