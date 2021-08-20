package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.MyTestSpecs

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
class TestEchoTestBed extends MyTestSpecs {
  info("""=================================================================""")
  info("""This test suite is an integration test suite. It starts with a 
    fresh instance of a distributed system (an Echo server and client) 
    and ending with testing the whole pipeline of the runtime""")
  info("""=================================================================""")

  val myDS = TestBeds.echoServerInstance

  test("Boot strapping a server") {
    // starting server, which dows nothing in our test bed since it basically sends s, then j.
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
    scheduler.taskQ.size should be(2) // containing 2 SuspendableTasks
  }
//  val serverStmtsCnt = scheduler.taskQ.countStmtsHeadTask("server")
//  val clientStmtsCnt = scheduler.taskQ.countStmtsHeadTask("client") //.head.action.toExecute.size

  var clientStmtsCnt = 0
  test("Consuming all tasks results in placing them in the consumeQ") {

    val serverStmtsCnt = scheduler.consumeATask("server")
    clientStmtsCnt = scheduler.consumeATask("client")
    //    (1 to serverStmtsCnt).map ( _ => scheduler.consume(myDS.get("server")))
    //    (1 to clientStmtsCnt).map ( _ => scheduler.consume(myDS.get("client")))

    scheduler.taskQ should be(empty) // contains no more tasks
    scheduler.consumeQ.size should be(serverStmtsCnt + clientStmtsCnt) // containing # statements
  }

  test("Executing each task") {

    // the server's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "server"}
    //    scheduler.consumeQ.size should be(1)
    scheduler.consumeQ.size should be(clientStmtsCnt)

    // the client's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "client"}
    //    scheduler.execute(1)
    scheduler.consumeQ.size should be(0)
  }

  test("Scheduling the last pending task in server sent by client") {
    myDS.hasWork should be(Set(myDS.get("server")))
    myDS.get("server").q.head.name should be("Show")
    myDS.get("server").q.size should be(1)
    scheduler.schedule("server")
    myDS.get("server").q.size should be(0)
    scheduler.taskQ("server").size should be(1)
  }

//  val clientShowTaskSize = scheduler.taskQ("client").head.action.toExecute.size

  test("Consuming and executing the 'Show' task") {
	 val clientShowTaskSize =  scheduler.consumeATask("server")
    scheduler.taskQ("server").size should be(0)
    //    scheduler.consume
    scheduler.consumeQ.size should be(clientShowTaskSize)

    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "server"}
//    scheduler.consumeQ map { _ => scheduler.execute(1)}
    scheduler.consumeQ.size should be(0)
  }

  // now we don't have a guaranteed-to-terminate DistributedSystem.stop method.
  // soon we will.

  test("Shutting down the system") {
    myDS.isShutdown should be(false)
    myDS.shutdownSystem
    myDS.isShutdown should be(true)
  }
}