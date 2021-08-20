package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Start

class BehaviorTests extends MyTestSpecs{
  info("""This test suite is an integration test suite. It starts with a 
    fresh instance of a distributed system (a Town_Crier and Number_Cruncher) 
    and ending with testing the whole pipeline of the runtime""")

  val myDS = DistributedSystemGenerator.generateNumberManipulatorSystem()
  
  
  
  test("Boot strapping a Number_Cruncher") {
    // starting server, which does nothing in our test bed since it basically sends s, then j.
    myDS.bootStrap("Number_Cruncher")
    myDS.get("Number_Cruncher").q.head.name should be(new Start name)
    myDS.get("Number_Cruncher").locked should be(true)
    myDS.get("Number_Cruncher").q.head.sender should equal(myDS.bootStraper)
    myDS.get("Number_Cruncher").q.head.sendMethod should be(false)
  }

  test("Boot strapping Town_Crier") {
    // start client, which will send a Show message to server, the server in turn will print it
    myDS.bootStrap("Town_Crier")

    myDS.get("Town_Crier").q.head.name should be(new Start name)
    myDS.get("Town_Crier").locked should be(true)
    myDS.get("Town_Crier").q.head.sender should equal(myDS.bootStraper)
    myDS.get("Town_Crier").q.head.sendMethod should be(false)
  }

  val scheduler = myDS.scheduler

  test("DistributedSystem and Schedulers know each other") {

    scheduler shouldNot be(null)
    scheduler should be(myDS.scheduler)
    myDS.scheduler should be(scheduler)
    scheduler.taskQ should be(empty)
    scheduler.consumeQ.size should be(0)
  }

  test("Scheduling all Number_Cruncher tasks then all of Town_Crier's tasks") {
    // server has to be started so we schedule it twice (for start and for join)
    scheduler.schedule("Number_Cruncher")
    scheduler.taskQ.size should be(1)
    //then schedule anything (in this case it is certainly the client)
    scheduler.schedule("Town_Crier") // the start ===> generates another message to server
    scheduler.taskQ.size should be(2) // containing 2 SuspendableTasks
  }

  var clientStmtsCnt = 0
  test("Consuming all tasks results in placing them in the consumeQ") {
    val serverStmtsCnt = scheduler.consumeATask("Number_Cruncher")
    clientStmtsCnt = scheduler.consumeATask("Town_Crier")
    
//    scheduler.consume
//    scheduler.consume

    scheduler.taskQ should be(empty) // contains no more tasks
    scheduler.consumeQ.size should be(serverStmtsCnt + clientStmtsCnt) // containing # stmts
  }

  test("Executing each task") {

    // the server's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "Number_Cruncher"}
//    scheduler.execute(1)
    scheduler.consumeQ.size should be(clientStmtsCnt)

    // the client's start
    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "Town_Crier"}
//    scheduler.execute(1)
    scheduler.consumeQ.size should be(0)
    
    println("=================== Has Work ====================")
    println(myDS.hasWork.toString())
    println("=================== Has Work End ====================")
  }

  test("Scheduling the next pending task in Number_Cruncher sent by Town_Crier") {    
    myDS.hasWork should be(Set(myDS.get("Number_Cruncher")))
    myDS.get("Number_Cruncher").q.head.name should be("modify_number")
    myDS.get("Number_Cruncher").q.size should be(1)
    scheduler.schedule("Number_Cruncher")
    myDS.get("Number_Cruncher").q.size should be(0)
    scheduler.taskQ.size should be(1)
  }

  test("Consuming and executing the 'modify_number' task") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeATask("Number_Cruncher")
//    scheduler.consumeWhile("Number_Cruncher"){x => x.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(cnt)

    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(0)
  }
  
  test("Scheduling the next pending task in Town_Crier sent by Number_Cruncher"){
    myDS.hasWork should be(Set(myDS.get("Town_Crier")))
    myDS.get("Town_Crier").q.head.name should be("Message")
    myDS.get("Town_Crier").q.size should be(1)
    scheduler.schedule("Town_Crier")
    myDS.get("Town_Crier").q.size should be(0)
    scheduler.taskQ.size should be(1)
  }
  
  test("Consuming and executing the 'Message' task") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeATask("Town_Crier")
//    scheduler.consumeWhile("Town_Crier"){x => x.a.name == "Town_Crier"}
    scheduler.consumeQ.size should be(cnt)
    scheduler.executeWhile(1)(scheduler.consumeQ.head.a.name == "Town_Crier")
//    scheduler.execute(1)
    scheduler.consumeQ.size should be(0)
  }
  
  test("Scheduling the modify_behavior task in Number_Cruncher sent by Town_Crier"){
    myDS.hasWork should be(Set(myDS.get("Number_Cruncher")))
    myDS.get("Number_Cruncher").q.head.name should be("modify_behavior")
    myDS.get("Number_Cruncher").q.size should be(2)
    scheduler.schedule("Number_Cruncher")
    myDS.get("Number_Cruncher").q.size should be(1)
    scheduler.taskQ.size should be(1)
  }
  
  test("Changing behavior of Number_Cruncher") {
    myDS.get("Number_Cruncher").reactions.name should be("default")
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeATask("Number_Cruncher")
//    scheduler.consumeWhile("Number_Cruncher"){ _.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(cnt)

    scheduler.executeWhile(1){ scheduler.consumeQ.head.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(0)
    myDS.get("Number_Cruncher").reactions.name should be("multiply")
    myDS.get("Number_Cruncher").oldBehaviors.size should be(0)
  }
  
  test("Scheduling the final pending task in Number_Cruncher sent by Town_Crier") {    
    myDS.hasWork should be(Set(myDS.get("Number_Cruncher")))
    myDS.get("Number_Cruncher").q.head.name should be("modify_number")
    myDS.get("Number_Cruncher").q.size should be(1)
    scheduler.schedule("Number_Cruncher")
    myDS.get("Number_Cruncher").q.size should be(0)
    scheduler.taskQ.size should be(1)
  }

  test("Consuming and executing the multiply version of 'modify_number' task") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeAll("Number_Cruncher")
    
//    scheduler.consumeWhile("Number_Cruncher"){ _.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(cnt)

    scheduler.executeWhile(1){scheduler.consumeQ.head.a.name == "Number_Cruncher"}
//    scheduler.executeWhile(1){ _.a.name == "Number_Cruncher"}
    scheduler.consumeQ.size should be(0)
  }
  
  test("Scheduling the final pending task in Town_Crier sent by Number_Cruncher"){
    myDS.hasWork should be(Set(myDS.get("Town_Crier")))
    myDS.get("Town_Crier").q.head.name should be("Message")
    myDS.get("Town_Crier").q.size should be(1)
    scheduler.schedule("Town_Crier")
    myDS.get("Town_Crier").q.size should be(0)
    scheduler.taskQ.size should be(1)
  }
  
  test("Consuming and executing the final 'Message' task") {
    scheduler.consumeQ.size should be(0)
    val cnt = scheduler.consumeAll("Town_Crier")
//    scheduler.consumeWhile("Town_Crier") {_.a.name == "Town_Crier"}
    scheduler.consumeQ.size should be(cnt)

    scheduler.executeWhile(1) {scheduler.consumeQ.head.a.name == "Town_Crier"}
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