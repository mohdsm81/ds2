

package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Start => StartMsg }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{ Ask, Get => GetStmt }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Start => StartMsg }

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import java.util.concurrent.ScheduledExecutorService
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.tracing.Get
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM
/**
 * @author <br>
 * Mohammed S. Al-Mahfoudh <br/>
 * mahfoudh@cs.utah.edu <br/>
 * SoC - Gauss Group <br/>
 */
object TestBeds {
  /*
   * for example:
   * 1- Echo Server and Client interacting with SEND
   * 2- Echo Server and Client but interacting with ASK
   * 
   *  Try to add more examples.
   */

  def echoServerInstance: DistributedSystem = {

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    //    ds.enableTracing
    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)
    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val s = new Agent("server")

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    //    //------------------------------
    //    // Overriding the start action 
    //    // of both Server and Client
    //    //------------------------------
    //
    //    // the start action shared by both server and client (unlocking by default for starting)
    //
    //    val startAction = new Action
    //    val code = (m: Message, a: Agent) => {
    //      ds.unlock(a)
    //    }
    //    val startStmt = Statement(code)
    //    startAction.stmts = Seq(startStmt)
    //
    //    c.specialReactions += (new Start -> startAction.copy)
    //    s.specialReactions += (new Start -> startAction.copy)
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode = (m: Message, a: Agent) => {
      println("--------------------------OUTPUT---------------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------OUTPUT---------------------------")
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction + stmtErr

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //    // NEVER EVER forget to refresh the Agent before you use it, strange things can and will happen.
    //    s.refresh

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg
    val code2 = (m: Message, a: Agent) => { ds.send(a, clientMsg, s) }
    val cAction = new Action
    cAction + Statement(code2)

    c.specialReactions += (new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start -> cAction)

    //    c.refresh

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh
    ds
  }

  def echoServerInstanceCopy: DistributedSystem = {
    // this test-bed is primarily to make sure the two-phase 
    // copy-link process works as expected

    import statement.kinds._

    // creating a Distributed System
    var ds = new DistributedSystem("echo")
    //    ds.enableTracing
    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)
    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val s = new Agent("server")

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    //    //------------------------------
    //    // Overriding the start action 
    //    // of both Server and Client
    //    //------------------------------
    //
    //    // the start action shared by both server and client (unlocking by default for starting)
    //
    //    val startAction = new Action
    //    val code = (m: Message, a: Agent) => {
    //      ds.unlock(a)
    //    }
    //    val startStmt = Statement(code)
    //    startAction.stmts = Seq(startStmt)
    //
    //    c.specialReactions += (new Start -> startAction.copy)
    //    s.specialReactions += (new Start -> startAction.copy)
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode = (m: Message, a: Agent) => {
      println("--------------------------OUTPUT---------------------------")
      println(m.sender.name +
        " sent a request to print: "
        + m.payload.mkString)
      println("--------------------------OUTPUT---------------------------")
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction + stmtErr

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //    // NEVER EVER forget to refresh the Agent before you use it, strange things can and will happen.
    //    s.refresh

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg
    // non-instrumented way of doing it
    //    val code2 = (m: Message, a: Agent) => { ds.send(a, clientMsg, s) }
    val cAction = new Action

    // non instrumented
    //    cAction + Statement(code2)

    // instrumented!
    //    cAction + Statement(SEND, c, clientMsg, s)
    //    cAction + Send(c, clientMsg, s)
    cAction + Send(clientMsg, s)

    c.specialReactions += (new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start -> cAction)

    //DEBUG
    println(c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start).stmts(0))
    println(c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start).stmts(1))


    //    c.refresh

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh

    // only following changed from original test-bed

    //    val newDS = ds.copy // this copy is to be disposed
    //    val sched = ds.scheduler // because this scheduler will be used to
    val state = ds.snapshot
    state.restore
    ds.scheduler.ds // return the same copy of the DS but with the same state as before!
  }

  def echoServerWithAskInstance: DistributedSystem = {

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    // ds.enableTracing

    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)

    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val cVar = s"c${LocalState.DELIM}Agent"
    val s = new Agent("server")
    val sVar = s"s${LocalState.DELIM}Agent"

    // i just needed to initialize it anyways ... good question:
    var future: DummyFuture = new DummyFuture(false, 0, s, c)
    var futureVar = s"future${LocalState.DELIM}DummyFuture"
    var futureValue = s"futureValue${LocalState.DELIM}Any"
    c.localState(sVar) = s

    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode1 = (m: Message, a: Agent) => {
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      // now let the client, IN ANOTHER STATEMENT to support unblocking through another scheduler.tick
    }

    val sCode2 = (m: Message, a: Agent) => {
      ds.resolve(future, true)
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    val stmt1 = Statement(sCode1) // NONE
    val stmt2 = Statement(sCode2) // RESOLVE
    //    stmt2.kind = Statement.Kind.RESOLVE 
    sAction + stmt1 + stmt2

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction.stmts = Seq(stmtErr)

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg

    // val ccode1 = (m: Message, a: Agent) => {
    //   future = ds.ask(a, clientMsg, s)
    //   // i can put c instead of a, a will be set anyways to the containing agent which is c in this case.
    // }

    // val cstmt1 = Statement(ccode1)
    val clientMsgVar = s"clientMessage${LocalState.DELIM}Message"
    c.localState(clientMsgVar) = clientMsg
    val cstmt1 = Ask(clientMsgVar, sVar, futureVar)

    //    cstmt1.kind  = ASK 

    future.storedInVariable = futureVar
    c.localState(futureVar) = future

    // val ccode2 = (m: Message, a: Agent) => {
    //   // blocking get (not timing out).
    //   ds.get(future, Some(futureValue), c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start)) // note we didn't store it to a local variable!
    //   // one question: then how can we say someVar = f.value as it may happen in code we parse?
    //   // ans: add one more entry to local state with (someVar.toString -> localState(f.id.toString)).

    //   // note the future when gotten (of course if it is resolved first), it gets stored 
    //   // in the agent's localState

    //   /*
    //    * NOTE:
    //    * 1- the get(future) (or timing out variation of it) need to be last to achieve block/resume of the action.
    //    * 2- why do we need the "cAction" in the get method?
    //    *    Ans: because we need to know in which action this get-call is happening, again to support block and resume
    //    *    of the action.
    //    */

    // }

    // val cstmt2 = Statement(ccode2)
    //    cstmt2.kind = GET
    val valueVar = s"value${LocalState.DELIM}Any"
    val cstmt2 = GetStmt(futureVar, valueVar)

    val ccode3 = (m: Message, a: Agent) => {
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")

      // now we resume (also we need that future again!) -- you see how we saved a lot of coding by making it global "future".
      a.localState[Boolean](valueVar) match {
        case true => println("GREAT!!! I got a reply: my message was shown to the world")
        case _    => println("SAD!!! I will stay alone: no one saw my message!")
      }
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
    }

    val action = c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start)

    //    action + Statement(ccode1) + Statement(ccode2) + Statement(ccode3)
    action + cstmt1 + cstmt2 + Statement(ccode3)

    c.specialReactions += (new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start -> action)

    /*
     * did you notice yet that c doesn't have a default or current reactions? this is intentional
     * but when you do it for real distributed systems, this is semantic error. Every one should 
     * have a brain :)
     */

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh
    ds
  }

  def echoServerWithAskInstanceCopy: DistributedSystem = {
    // this test-bed is primarily to make sure the two-phase 
    // copy-link process works as expected

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    //    ds.enableTracing

    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)

    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val cVar = s"c${LocalState.DELIM}Agent"
    val s = new Agent("server")
    val sVar = s"s${LocalState.DELIM}Agent"
    c.localState(sVar) = s

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    val future = s"future${LocalState.DELIM}DummyFuture"
    val resolvingValue = s"resolved${LocalState.DELIM}Any"

    //------------------------------
    // SERVER setup
    //------------------------------
    val senderVar = s"sender${DELIM}Agent"
    val replyMsgVar = s"reply${DELIM}Message"

    val sCode1 = (m: Message, a: Agent) => {
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------SERVER-OUTPUT-ASK----------------------")

      // yup, i should have put these in two modify-state, but i wanted to do it fast
      // besides that the new stmt kinds are being tested separately at moment.
      a.localState(senderVar) = m.sender
      a.localState(replyMsgVar) = new Message(future) // it doesn't matter what msg to use here
      a.localState(replyMsgVar).asInstanceOf[Message].payload = Seq(true)
      // now let the client, IN ANOTHER STATEMENT to support unblocking through another scheduler.tick
    }

    //    val sCode2 = (m: Message, a: Agent) => {
    //      ds.resolve(future, true)
    //    }

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg
    //    val stmt1 = Statement(ASK, c, clientMsg, s, future)
    val clientMsgVar = s"message${LocalState.DELIM}Message"

    //    s.localState(cVar) = c // not even sure if it is needed, just put it there in case.
    c.localState(sVar) = s
    c.localState(clientMsgVar) = clientMsg

    import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.Ask
    //    val stmt1 = Ask(c, clientMsg, s, future)
    //    val stmt1 = Ask(cVar, clientMsgVar, sVar, future)
    val stmt1 = Ask(clientMsgVar, sVar, future)
    // create an action
    val sAction = new Action

    /*
     * NOTE:
     * I could have added a ModifyState statement and then a dynamic send,
     * to store the sender of the received message and then using it by the
     * send statement, respectively. 
     * 
     * For the sake of simplicity and finishing debugging the project (as
     * all stmts are being tested separately) I would go with sending a
     * direct ResolveDummyFuture message to client. This is NOT assuming
     * the server knows it is resolving anything more than to simplify
     * things for now.     
     */
    //    sAction + Send(new Message(future), c) // the R-SEND

    sAction + Statement(sCode1) + Send(replyMsgVar, senderVar) // the R-SEND

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction.stmts = Seq(stmtErr)

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //    val stmt2 = Statement(GET, c, future) // a GET statement, unique signature so no need for the GET tag
    val stmt2 = GetStmt(future, resolvingValue) // a GET statement, unique signature so no need for the GET tag
    //    val stmt2 = Statement(c, stmt1.future)

    val ccode3 = (m: Message, a: Agent) => {
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
      // now we resume (also we need that future again!) -- you see how we saved a lot of coding by making it global "future".
      // a.getVariable(resolvingValue).asInstanceOf[DummyFuture].value match {
      a.localState[Boolean](resolvingValue) match {
        case true => println("GREAT!!! I got a reply: my message was shown to the world")
        case _    => println("SAD!!! I will stay alone: no one saw my message!")
      }
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
    }

    val action = c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start)

    //    action + Statement(ccode1) + Statement(ccode2) + Statement(ccode3)
    action + stmt1 + stmt2 + Statement(ccode3)

    c.specialReactions += (new StartMsg -> action)

    /*
     * did you notice yet that c doesn't have a default or current reactions? this is intentional
     * but when you do it for real distributed systems, this is semantic error. Every one should 
     * have a brain :)
     */

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s
    ds + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh

    // This is the copying part
    val state = ds.snapshot
    state.restore
    ds // return the same COPY of the DS but with the new state restored
  }

  def echoServerWithAskInstanceWithTimedGet: DistributedSystem = {

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    //    ds.enableTracing

    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)

    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val cVar = s"c${DELIM}Agent"
    val s = new Agent("server")
    val sVar = s"s${DELIM}Agent"

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    // i just needed to initialize it anyways ... good question:
    // why would I make this thing global if it can live inside 
    // the agent's state? Ans: because I don't have to cast it back from
    // Any to DummyFuture. (in case we need to investigate the internal
    // localState of the agent to do additional analysis, then it is 
    // better to add it inside the agent).
    var future: DummyFuture = new DummyFuture(false, 0, s, c)
    val futureVar = s"future${DELIM}DummyFuture"
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode1 = (m: Message, a: Agent) => {
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      // now let the client, IN ANOTHER STATEMENT to support unblocking through another scheduler.tick
    }

    val sCode2 = (m: Message, a: Agent) => {
      // do not resolve, make it time out

      //      ds.resolve(future, true)
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode1) + Statement(sCode2)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction.stmts = Seq(stmtErr)

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    val clientMsgVar = s"clientMsg${DELIM}Message"
    c.localState(clientMsgVar) = clientMsg
    c.localState(futureVar) = future
    c.localState(sVar) = s

    // val ccode1 = (m: Message, a: Agent) => {
    //   future = ds.ask(a, clientMsg, s)
    //   // i can put c instead of a, a will be set anyways to the containing agent which is c in this case.
    // }
    val cstmt1 = Ask(clientMsgVar, sVar, futureVar)
    val ccode2 = (m: Message, a: Agent) => {
      // blocking get (not timing out).
      //      ds.get(c, future, c.specialReactions(new Start)) // note we didn't store it to a local variable!

      ds.getTimed(future, None, 10, c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start))

      // one question: then how can we say someVar = f.value as it may happen in code we parse?
      // ans: add one more entry to local state with (someVar.toString -> localState(f.id.toString)).

      // note the future when gotten (of course if it is resolved first), it gets stored 
      // in the agent's localState

      /*
       * NOTE:
       * 1- the get(future) (or timing out variation of it) need to be last to achieve block/resume of the action.
       * 2- why do we need the "cAction" in the get method?
       *    Ans: because we need to know in which action this get-call is happening, again to support block and resume
       *    of the action.
       */

    }

    val valueVar = s"value${DELIM}Any"
    val timeoutVar = s"timeout${DELIM}BigInt"
    c.localState(timeoutVar) = BigInt(10)
    c.localState(valueVar) = false

    val cstmt2 = TimedGet(futureVar, valueVar, timeoutVar)

    val ccode3 = (m: Message, a: Agent) => {
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
      // now we resume (also we need that future again!) -- you see how we saved a lot of coding by making it global "future".
      //      a.localState(future.id.toString) 
      // a.futuresWaitingFor(future.id).value
      a.localState[Boolean](valueVar) match {
        case true => println("GREAT!!! I got a reply: my message was shown to the world")
        case _    => println("SAD!!! I will stay alone: no one saw/replied-to my message!")
      }
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
    }

    val action = c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start)

    // action + Statement(ccode1) + Statement(ccode2) + Statement(ccode3)
    action + cstmt1 + cstmt2 + Statement(ccode3)

    c.specialReactions += (new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start -> action)

    /*
     * did you notice yet that c doesn't have a default or current reactions? this is intentional
     * but when you do it for real distributed systems, this is semantic error. Every one should 
     * have a brain :)
     */

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh
    ds

  }

  def echoServerWithAskInstanceWithTimedGetCopy: DistributedSystem = {

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    //    ds.enableTracing

    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)

    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val cVar = s"c${DELIM}Agent"
    val s = new Agent("server")
    val sVar = s"s${DELIM}Agent"

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    // i just needed to initialize it anyways ... good question:
    // why would I make this thing global if it can live inside 
    // the agent's state? Ans: because I don't have to cast it back from
    // Any to DummyFuture. (in case we need to investigate the internal
    // localState of the agent to do additional analysis, then it is 
    // better to add it inside the agent).
    var future: DummyFuture = new DummyFuture(false, 0, s, c)
    val futureVar = s"future${DELIM}DummyFuture"
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode1 = (m: Message, a: Agent) => {
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------SERVER-OUTPUT-ASK----------------------")
      // now let the client, IN ANOTHER STATEMENT to support unblocking through another scheduler.tick
    }

    val sCode2 = (m: Message, a: Agent) => {
      // do not resolve, make it time out

      //      ds.resolve(future, true)
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode1) + Statement(sCode2)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction.stmts = Seq(stmtErr)

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    val clientMsgVar = s"clientMsg${DELIM}Message"
    c.localState(clientMsgVar) = clientMsg
    c.localState(futureVar) = future
    c.localState(sVar) = s

    // val ccode1 = (m: Message, a: Agent) => {
    //   future = ds.ask(a, clientMsg, s)
    //   // i can put c instead of a, a will be set anyways to the containing agent which is c in this case.
    // }
    val cstmt1 = Ask(clientMsgVar, sVar, futureVar)
    val ccode2 = (m: Message, a: Agent) => {
      // blocking get (not timing out).
      //      ds.get(c, future, c.specialReactions(new Start)) // note we didn't store it to a local variable!

      ds.getTimed(future, None, 10, c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start))

      // one question: then how can we say someVar = f.value as it may happen in code we parse?
      // ans: add one more entry to local state with (someVar.toString -> localState(f.id.toString)).

      // note the future when gotten (of course if it is resolved first), it gets stored 
      // in the agent's localState

      /*
       * NOTE:
       * 1- the get(future) (or timing out variation of it) need to be last to achieve block/resume of the action.
       * 2- why do we need the "cAction" in the get method?
       *    Ans: because we need to know in which action this get-call is happening, again to support block and resume
       *    of the action.
       */

    }

    val valueVar = s"value${DELIM}Any"
    val timeoutVar = s"timeout${DELIM}BigInt"
    c.localState(timeoutVar) = BigInt(10)
    c.localState(valueVar) = false

    val cstmt2 = TimedGet(futureVar, valueVar, timeoutVar)

    val ccode3 = (m: Message, a: Agent) => {
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
      // now we resume (also we need that future again!) -- you see how we saved a lot of coding by making it global "future".
      //      a.localState(future.id.toString) 
      // a.futuresWaitingFor(future.id).value
      a.localState[Boolean](valueVar) match {
        case true => println("GREAT!!! I got a reply: my message was shown to the world")
        case _    => println("SAD!!! I will stay alone: no one saw/replied-to my message!")
      }
      println("--------------------------CLIENT-OUTPUT-ASK----------------------")
    }

    val action = c.specialReactions(new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start)

    // action + Statement(ccode1) + Statement(ccode2) + Statement(ccode3)
    action + cstmt1 + cstmt2 + Statement(ccode3)

    c.specialReactions += (new edu.utah.cs.gauss.ds2.core.ir.datastructures.Start -> action)

    /*
     * did you notice yet that c doesn't have a default or current reactions? this is intentional
     * but when you do it for real distributed systems, this is semantic error. Every one should 
     * have a brain :)
     */

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh

    //------------------------------
    // But we decide to copy it
    //------------------------------
    val state = ds.snapshot
    state.restore
    ds // return the same COPY of the DS, but with new state
  }

  def echoServerInstanceWithTracing: DistributedSystem = {

    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    ds.enableTracing
    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)
    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val s = new Agent("server")

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    //    //------------------------------
    //    // Overriding the start action 
    //    // of both Server and Client
    //    //------------------------------
    //
    //    // the start action shared by both server and client (unlocking by default for starting)
    //
    //    val startAction = new Action
    //    val code = (m: Message, a: Agent) => {
    //      ds.unlock(a)
    //    }
    //    val startStmt = Statement(code)
    //    startAction.stmts = Seq(startStmt)
    //
    //    c.specialReactions += (new Start -> startAction.copy)
    //    s.specialReactions += (new Start -> startAction.copy)
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode = (m: Message, a: Agent) => {
      println("--------------------------OUTPUT---------------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------OUTPUT---------------------------")
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction + stmtErr

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //    // NEVER EVER forget to refresh the Agent before you use it, strange things can and will happen.
    //    s.refresh

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg
    val code2 = (m: Message, a: Agent) => { ds.send(a, clientMsg, s) }
    val cAction = new Action
    cAction + Statement(code2)

    c.specialReactions += (new StartMsg -> cAction)

    //    c.refresh

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh
    ds
  }

  def echoServerInstanceWithTracingCopy: DistributedSystem = {
    // creating a Distributed System
    val ds = new DistributedSystem("echo")
    ds.enableTracing
    //------------------------------
    // Link a new scheduler to DS
    //------------------------------

    // Select a scheduler, now will default to basic
    // note ds and scheduler both call-back to each other.
    ds.attach(new BasicScheduler)
    // creating agents and setting their attributes, overrides, ..., etc
    val c = new Agent("client")
    val s = new Agent("server")

    //NOTE: add ActionsToSends manually (as it is part of SA done by the front end).

    //    //------------------------------
    //    // Overriding the start action 
    //    // of both Server and Client
    //    //------------------------------
    //
    //    // the start action shared by both server and client (unlocking by default for starting)
    //
    //    val startAction = new Action
    //    val code = (m: Message, a: Agent) => {
    //      ds.unlock(a)
    //    }
    //    val startStmt = Statement(code)
    //    startAction.stmts = Seq(startStmt)
    //
    //    c.specialReactions += (new Start -> startAction.copy)
    //    s.specialReactions += (new Start -> startAction.copy)
    //------------------------------
    // SERVER setup
    //------------------------------

    val sCode = (m: Message, a: Agent) => {
      println("--------------------------OUTPUT---------------------------")
      println(m.sender.name + " sent a request to print: " + m.payload.mkString)
      println("--------------------------OUTPUT---------------------------")
    }

    // create an action
    val sAction = new Action
    // add the statement(s) to the action
    sAction + Statement(sCode)

    // create another statement
    val stmtErr = Statement(
      (m: Message, a: Agent) => {
        if (m.name != "Show")
          println("Agent(" + a.name + ") sent an unknown request")
      })
    // create another action
    val sErrorAction = new Action
    // add statement(s) to it
    sErrorAction + stmtErr

    val sReaction = (new Message("Show"), sAction)
    val sErrReaction = (new Message, sErrorAction)

    val reactions = new Behavior("default")
    reactions += sReaction
    reactions += sErrReaction

    s.defaultBehavior = reactions
    s.reactions = reactions

    //    // NEVER EVER forget to refresh the Agent before you use it, strange things can and will happen.
    //    s.refresh

    //------------------------------
    // CLIENT setup
    //------------------------------

    val clientMsg = new Message("Show", "Whatever to show")
    // don't forget to define it in the DistributedSystem
    //    ds.messages += clientMsg
    //    val code2 = (m: Message, a: Agent) => { ds.send(a, clientMsg, s) }

    val cAction = new Action
    //    cAction + Statement.apply(SEND, c, clientMsg, s)
    cAction + Send(clientMsg, s)

    c.specialReactions += (new StartMsg -> cAction)

    //    c.refresh

    //------------------------------
    // Add agents to DS
    //------------------------------

    // add the agents to the Distributed system
    ds + s + c

    //------------------------------
    // DS ready for exploration
    //------------------------------

    // returning the distributed system, most likely for a scheduler to explore
    ds.refresh
    val state = ds.snapshot
    state.restore
    ds // return the same COPY of the DS but with the new state restored
  }

}
