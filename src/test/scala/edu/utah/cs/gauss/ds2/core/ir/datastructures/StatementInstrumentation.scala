package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import org.scalatest.Ignore
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._

class StatementInstrumentation extends MyTestSpecs {
  info("==================================")
  info("Statement Instrumentation tests")
  info("==================================")

  test("SEND") {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val src = agentInstance
    ds + src

    val s = Send(messageInstance, ds.get("server"))
    s.setAgent(src)

    //    assert(null != s.srcAgent, "srcAgent is NULL!")
    assert(null != s.dstAgent, "dstAgent is NULL!")
    assert(null != s.msgOut, "msgOut is NULL!")

    s.apply

    assert(ds.get("server").q.size == 1, "Server didn't receive the message!")

  }


  test("ASK") {
    import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM

    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    //        val src = agentInstance
    //    val dst = agentInstance


    val variableName = "myVar" + DELIM + "DummyFuture"

    //    var s = Statement(ASK, ds.get("client"), messageInstance, ds.get("server"),variableName)
    val src = ds.get("client")
    val msgOut = messageInstance
    val dst = ds.get("server")

    val srcVar = s"client${DELIM}Agent"
    val msgOutVar = s"message${DELIM}Message"
    val dstVar = s"server${DELIM}Agent"

    src.localState(srcVar) = src
    src.localState(dstVar) = dst
    src.localState(msgOutVar) = msgOut
    var f = null

    //    var s = Ask(srcVar, msgOutVar, dstVar, variableName)
    var s = Ask(msgOutVar, dstVar, variableName)
    s.setAgent(src)

    s.apply

    assert(null != s.future && s.future.isInstanceOf[DummyFuture], "There isn't future returned!")
    assert(s.future == src.localState[DummyFuture](variableName))
    assert(ds.get("server").q.size == 1, "Server didn't receive the message!")
  }

  test("DistributedSystem - SEND") {
    val ds = new DistributedSystem("Test")

    ds.attach(new BasicScheduler)

    var sender = new Agent("dude1")
    var receiver = new Agent("dude2")

    //    val st = Statement(SEND,sender,new Start,receiver)
    //    val st = Send(sender,new Start,receiver)
    val st = Send(new Start, receiver)

    val act = new Action
    act.stmts = act.stmts :+ st

    sender.specialReactions + (new Start, st)
    //    sender.specialReactions  += (new Start, act)

    //    println("BEFORE picking: "+  sender.specialReactions(new Start).stmts.size)


    ds + receiver + sender // add them to the distributed system
    ds.refresh

    // hacking them to make it short
    receiver.locked = false
    sender.locked = false
    ds.bootStrap(sender) // invokes the start action  
    //    println("=====================================================")
    //    println(sender)
    //    println(receiver)
    //    println(ds.scheduler)
    //    println("=====================================================")
    ds.scheduler.schedule(sender)
    //    println("=====================================================")
    //    println(sender)
    //    println(receiver)
    //    println(ds.scheduler)
    //    println("=====================================================")
    //
    ds.scheduler.consumeATask(sender)

    //    println("=====================================================")
    //    println(sender)
    //    println(receiver)
    //    println(ds.scheduler)
    //    println("=====================================================")
    //
    ds.scheduler.executeWhile(1) {
      ds.scheduler.consumeQ.head.a.name == sender.name
    }

    //    println("=====================================================")
    //    println(sender)
    //    println(receiver)
    //    println(ds.scheduler)
    //    println("=====================================================")
    //


    //    println("first: "+receiver.q.size)

    receiver.q.size should be(1) // should pass


    // =======================
    // SECOND PART (snapshotting)
    // =======================    
    val schState = ds.scheduler.snapshot
    val dsState = ds.snapshot

    schState.restore
    dsState.restore

    sender = ds.get("dude1")
    receiver = ds.get("dude2")

    ds.bootStrap(sender) // invokes the start action

    ds.scheduler.schedule(sender)
    ds.scheduler.consumeATask(sender)
    ds.scheduler.executeWhile(1) {
      ds.scheduler.consumeQ.head.a.name == sender.name
    }

    //    println("Second: "+receiver.q.size)

    receiver.q.size should be(2) // should pass
  }

}