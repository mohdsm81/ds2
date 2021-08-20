package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._

object LinearizabilityTestBeds {

  case class Put(var nme: String = "put", var payld: Seq[Any] = Seq(0)) extends Message(nme, payld)
  case class Get(var nme: String = "get", var payld: Seq[Any] = Seq(s"x${LocalState.DELIM}Int")) extends Message(nme, payld)
  case class Value(var nme: String = "value", var payld: Seq[Any] = Seq(0)) extends Message(nme, payld) // can be used for Ack for both Get & Put

  val putAckName = "PutAck"
  val getAckName = "GetAck"

  //  def createSinkAgent(suffix: Int): Agent = new Agent(s"sink-${suffix}") 
  //  val commonSink = createSinkAgent(1)
  //  val commonSinkVar = s"${commonSink.name}${LocalState.DELIM}Agent"

  def createBogusRegisterServer(name: String, fieldName: String): Agent = {
    LocalState.validateVariable(fieldName)
    val s = new Agent(name)
    s.localState(fieldName) = 0

    //----------------------------------------
    // Adding client interface reactions to server
    //----------------------------------------
    // put(x) and get(x)
    val putAction = new Action + Statement((m: Message, a: Agent) => { println(s"Received a PUT(${m.payload[Any](0)}) request from ${m.sender.name}") })
    putAction + ReceiveModifyState(fieldName, (m: Message, a: Agent) => { a.localState(fieldName) = m.payload[String](0) })

    val getAction = new Action + Statement((m: Message, a: Agent) => { println(s"Received a GET(${m.payload[Any](0)}) request from ${m.sender.name}") })

    val replyVar = s"reply${LocalState.DELIM}Message"
    val clientVar = s"sender${LocalState.DELIM}Agent"

    // these two statments are equivalant to the one commented below it
    // preparing the reply
    getAction + ReceiveModifyState(replyVar, (m: Message, a: Agent) => { Value("value", Seq(a.localState(fieldName))) })
    getAction + ReceiveModifyState(clientVar, (m: Message, a: Agent) => { m.sender })

    // // preparing the reply
    // getAction + Statement((m:Message, a:Agent) => {
    //                         // could I have used 2 modify statements here instead?
    //                         a.localState(replyVar) = Value("value", Seq(a.localState(fieldName)))
    //                         a.localState(clientVar) = m.sender
    //                       })
    // response 
    getAction + Send(replyVar, clientVar)

    getAction + Statement((m: Message, a: Agent) => { println(s"sent a reply (${a.localState(replyVar)}) to ${a.localState(clientVar)}") })

    s.reactions += (Put() -> putAction)
    s.reactions += (Get() -> getAction)

    //----------------------------------------
    // Adding servers protocol reactions/implementation
    //----------------------------------------

    // this part is only solved in the qurum based Seif-Heridi
    // version. This one on the other hand is a bogus one

    /*
     NOTES:
     - Use default behavior for client interface + protocol implementation
     - use join/leave special reactions for making the party communicate with each other
     - use start special reaction to start a server (normally starting a register server is joining)
     - catch up to others
     - let them know about your existence
     - wait for ack before responding to requests, otherwise forward requests to others
     - Given above, how will the scheduler know where to find what? 
     - always using the same setup can help? i.e. default behavior for regular interface?
     */

    s
  }

  def createBogusRegisterServerWithSeqDedup(name: String, fieldName: String, seqFieldName: String): Agent = {
    // TODO code this with deduplication using Seq numbers
    LocalState.validateVariable(fieldName)
    LocalState.validateVariable(seqFieldName)

    val s = new Agent(name)
    s.localState(fieldName) = 0
    s.localState(seqFieldName) = -1 // jsut in case clients used 0-based sequence
    // val howManyVar = s"howManyRetries${LocalState.DELIM}Int"
    // s.localState(howManyVar) = howManyRetries // this is client-side retries

    //----------------------------------------
    // Adding client interface reactions to server
    //----------------------------------------
    // put(x) and get(x)
    val putAction = new Action + Statement((m: Message, a: Agent) => { println(s"Received a PUT(${m.payload[Any](0)}) request from ${m.sender.name}") })
    // the If(localstate(seqNo) < seqNoReceived) comes here
    putAction + If((m: Message, a: Agent) => { a.localState[Int](seqFieldName) <= m.payload[Int](1) }) (
        ReceiveModifyState(fieldName, (m: Message, a: Agent) => { a.localState(fieldName) = m.payload[String](0) }), // 0 is field name
        ReceiveModifyState(seqFieldName, (m: Message, a: Agent) => { a.localState(seqFieldName) = m.payload[String](1) })
    ) // seq specifying request number

    val replyVar = s"reply${LocalState.DELIM}Message"
    val clientVar = s"sender${LocalState.DELIM}Agent"
    val putAckVar = s"putAck${LocalState.DELIM}Message"
    // val getAckVar = s"getAck${LocalState.DELIM}Message"

    // FIXME this will always give the same result
    // s.localState(putAckVar) = Value(putAckName, Seq(s.localState(seqFieldName))) // so the client calls this function to know the value
    // s.localState(getAckVar) = Value(getAckName, Seq(s.localState(fieldName),s.localState(seqFieldName)))

    // send ack to client 
    // putAction + ReceiveModifyState(clientVar, putAckVar)
    putAction + ReceiveModifyState(putAckVar, (m: Message, a: Agent) => { Value(putAckName, Seq(a.localState(fieldName), s.localState(seqFieldName))) })
    putAction + Send(putAckVar, clientVar) // dynamically gets the actual values and sends them

    // these two statments are equivalant to the one commented below it
    // preparing the reply
    val getAction = new Action + Statement((m: Message, a: Agent) => { println(s"Received a GET(${m.payload[Any](0)}) request from ${m.sender.name}") })
    getAction + ReceiveModifyState(replyVar, (m: Message, a: Agent) => { Value(getAckName, Seq(a.localState(fieldName), a.localState(seqFieldName))) })
    getAction + ReceiveModifyState(clientVar, (m: Message, a: Agent) => { m.sender })
    getAction + Send(replyVar, clientVar)
    getAction + Statement((m: Message, a: Agent) => { println(s"sent a reply (${a.localState(replyVar)}) to ${a.localState(clientVar)}") })

    s.reactions += (Put() -> putAction)
    s.reactions += (Get() -> getAction)

    //----------------------------------------
    // Adding servers protocol reactions/implementation
    //----------------------------------------

    // this part is only solved in the qurum based Seif-Heridi
    // version. This one on the other hand is a bogus one

    /*
     NOTES:
     - Use default behavior for client interface + protocol implementation
     - use join/leave special reactions for making the party communicate with each other
     - use start special reaction to start a server (normally starting a register server is joining)
     - catch up to others
     - let them know about your existence
     - wait for ack before responding to requests, otherwise forward requests to others
     - Given above, how will the scheduler know where to find what? 
     - always using the same setup can help? i.e. default behavior for regular interface?
     */

    s
  }

  def createMostGenericClient(name: String = "stupid", valVarToGet: String, valVarToPut: String, dst: String): Agent = {
    LocalState.validateVariable(valVarToGet)
    LocalState.validateVariable(valVarToPut)
    LocalState.validateVariable(dst)

    // How to implement this one? should be method calls only? or should there be reactions.
    // maybe both?

    val c = new Agent(name)

    val putAction = new Action
    val getAction = new Action

    // c.localState()
    putAction + Send(Put("Put", Seq(c.localState(valVarToPut))), c.localState(dst).asInstanceOf[Agent])

    getAction + Send(Get("Get", Seq(c.localState(valVarToGet))), c.localState(dst).asInstanceOf[Agent])

    c.reactions += (Put() -> putAction)
    c.reactions += (Get() -> getAction)

    c
  }

  def createMostGenericClientWithRetries(name: String = "ClientWithRetiries", valVarToGet: String, valVarToPut: String, dst: String, numRetriesVar: String): Agent = {
    LocalState.validateVariable(valVarToGet)
    LocalState.validateVariable(valVarToPut)
    LocalState.validateVariable(dst)
    LocalState.validateVariable(numRetriesVar)

    val c = new Agent(name)

    val putAction = new Action
    val getAction = new Action
    val ackAction = new Action

    val futureVar = s"future${LocalState.DELIM}DummyFuture"
    val futureValueVar = s"futureValue${LocalState.DELIM}Any"
    c.localState(futureValueVar) = null

    val ackedValueVar = futureValueVar // they are the same but i needed it be more readable
    c.localState(ackedValueVar) = null

    val setOfFuturesVar = s"setOfFutures${LocalState.DELIM}Set[DummyFuture]"
    c.localState(setOfFuturesVar) = Set[DummyFuture]()
    val timeoutVar = s"timeout${LocalState.DELIM}Int"

    val seqNoVar = s"seqNo${LocalState.DELIM}Int"
    c.localState(seqNoVar) = 0
    c.localState(numRetriesVar) = 3

    val requestVar = s"request${LocalState.DELIM}Message"

    //=============================
    // PUT
    //=============================

    // increment seqNo
    putAction + ModifyState(seqNoVar, (m: Message, a: Agent) => a.localState(seqNoVar).asInstanceOf[Int] + 1)

    putAction + While((m: Message, a: Agent) => {
      a.localState(numRetriesVar).asInstanceOf[Int] >= 0 &&
        a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]].forall { x => !x.resolved }
    }) (
        ModifyState(requestVar, (m: Message, a: Agent) => Put("Put", Seq(a.localState(valVarToPut), a.localState(seqNoVar)))),
        Ask(requestVar, dst, futureVar), // returns a future from the sink, use it to wait for some time, not forever
        ModifyState(setOfFuturesVar, (m: Message, a: Agent) => {
          a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]] +
            a.localState(futureVar).asInstanceOf[DummyFuture]
        }),
        ModifyState(numRetriesVar, (m: Message, a: Agent) => { a.localState(numRetriesVar) = a.localState(numRetriesVar).asInstanceOf[Int] - 1 }), // decrement 
        TimedGet(futureVar, futureValueVar, timeoutVar) // waits till it times out, time is abstract so scheduler can havoc the way it wants
    )
    // if a future was resolved, print confirmation
    putAction + If((m: Message, a: Agent) => a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]].exists { x => x.resolved }) (
      Statement((m: Message, a: Agent) => println(s"SUCCEEDED putting value: ${a.localState(valVarToPut)} with seqNo: ${a.localState(seqNoVar)}"))
    )
    putAction + Else(
      Statement((m: Message, a: Agent) => println(s"FAILD putting value: ${a.localState(valVarToPut)} with seqNo: ${a.localState(seqNoVar)}")))(
      putAction.stmts.last)

    // empty the futures set, and reset the retries count
    putAction + ModifyState(numRetriesVar, 3)
    putAction + ModifyState(setOfFuturesVar, Set[DummyFuture]())

    //=============================
    // GET
    //=============================

    getAction + While((m: Message, a: Agent) => {
      a.localState(numRetriesVar).asInstanceOf[Int] >= 0 &&
        a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]].forall { x => !x.resolved }
    }) (
        ModifyState(requestVar, (m: Message, a: Agent) => Get("Get", Seq(a.localState(valVarToGet), a.localState(seqNoVar)))),
        Ask(requestVar, dst, futureVar), // returns a future from the sink, use it to wait for some time, not forever
        ModifyState(setOfFuturesVar, (m: Message, a: Agent) => {
          a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]] +
            a.localState(futureVar).asInstanceOf[DummyFuture]
        }),
        ModifyState(numRetriesVar, (m: Message, a: Agent) => { a.localState(numRetriesVar) = a.localState(numRetriesVar).asInstanceOf[Int] - 1 }), // decrement 
        TimedGet(futureVar, futureValueVar, timeoutVar) // waits till it times out, time is abstract so scheduler can havoc the way it wants
    )

    // if a future was resolved, print confirmation
    getAction + If((m: Message, a: Agent) => a.localState(setOfFuturesVar).asInstanceOf[Set[DummyFuture]].exists { x => x.resolved }) (Statement((m: Message, a: Agent) => println(s"SUCCEEDED getting value: ${m.payload[Any](0)} with seqNo: ${m.payload[Any](1)}")))
    getAction + Else(
      Statement((m: Message, a: Agent) => println(s"FAILD getting value of ${a.localState(valVarToGet)}")))(
      getAction.stmts.last)

    // empty the futures set, and reset the retries count
    getAction + ModifyState(numRetriesVar, 3)
    getAction + ModifyState(setOfFuturesVar, Set[DummyFuture]())

    //=============================
    // ACK
    //=============================

    ackAction + If((m: Message, a: Agent) => { m.name == putAckName })(
        Statement((m: Message, a: Agent) => { println(s"Received a PUT acknowledgement for storing ${m.payload[Any](0)} with seqNo: ${m.payload[Any](1)}") }),
        ModifyState(ackedValueVar, (m: Message, a: Agent) => m.payload[Any](0)),
        ModifyState(seqNoVar, (m: Message, a: Agent) => m.payload[Any](1)))

    ackAction + ElseIf((m: Message, a: Agent) => { m.name == getAckName }) (
        Statement((m: Message, a: Agent) => { println(s"Received a GET acknowledgement for retrieving ${m.payload[Any](0)} with seqNo: ${m.payload[Any](1)}") }),
        ModifyState(ackedValueVar, (m: Message, a: Agent) => m.payload[Any](0)),
        ModifyState(seqNoVar, (m: Message, a: Agent) => m.payload[Any](1))
    )(ackAction.stmts.last)

    ackAction + Else (Statement((m:Message, a:Agent) => println(s"Unknown request/message received: ${m.name}")))(ackAction.stmts.last)

    //=============================
    // Add reactions
    //=============================

    c.reactions += (Put() -> putAction)
    c.reactions += (Get() -> getAction)
    c.reactions += (Value() -> ackAction)

    c
  }

  def createAMultiRegisterServer(name: String, fieldNameValMap: Map[String, Any]) = {
    // TODO later for implementing the aggregate register data types/distributed systems
    ???
  }

  def dumbRegisterDistributedSystem(howMany: Int): DistributedSystem = {
    require(howMany >= 2, """Number of agents in the distributed system
    needs to be greater than TWO, no agents means no distributed
    system and one agent means no distribution""")

    val ds = new DistributedSystem("register")

    // don't attach any schedulers for now, later after developing
    // mixins for linearizabilityscheduler then you can mix things
    // together freely

    // ds.attach(new LinearizabilityScheduler(args))

    // adding agents
    (1 to howMany) map { x => ds + createBogusRegisterServer(x.toString, s"x${LocalState.DELIM}Any") } // end map

    ds.refresh
    ds
  }
}

