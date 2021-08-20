package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.state.{ AgentState, Snapshot }
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.{ Map => MMap }
import scala.language.postfixOps

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 * This is the main Entity in any distributed system. A communicating
 * process with encapsulated state that is divided into 3 categories:
 * 1- Internal State (e.g. localState field)
 * 2- Communication state (e.g. the stash and the incoming queue)
 * 3- Special reactions state (e.g. onStart, onJoin, ...etc).
 *
 * Note that to override the special behaviors (3), you need to assign
 * Actions to the corresponding key in the
 * <code>specialBehaviors</code> Map[Message,Action].
 *
 * Almost all important constraints and functions are implemented in
 * the containing DistributedSystem's class.  That includes the
 * tracing functionality, correctness checks, and updating most of
 * agent's state.
 *
 */

@SerialVersionUID(300)
case class Agent(var name: String) extends Serializable with Snapshot[AgentState] {

  var q: Seq[Message] = Seq() // incoming message queue, I am using Seq since it provides more flexibility that a Queue 

  var defaultBehavior: Behavior = new Behavior("default") // theta, the default behavior

  var stash: Seq[Message] = Seq() // S, the stash. I am using Seq since it provides more flexibility than a Queue

  var reactions: Behavior = defaultBehavior // R, initially set to 'defaultBehavior'

  var behaviors: Map[String, Behavior] = Map() // B, the set of behaviors the agent is allowed to exhibit

  var specialReactions: Behavior = new Behavior("special") // sigma, a set of special reactions (Message, Action) pairs, initially empty. Override-able by users.

  // not used, no plans to use any time soon, maybe later.
  //  var timedActions: Set[RuntimeTimedAction] = Set() // scheduled/periodic actions

  var oldBehaviors: mutable.Stack[String] = mutable.Stack() // beta, a stack of old behaviors, initially empty

  var ds: DistributedSystem = _ // = new BasicScheduler with MessageDropping with MessageReordering with TaskInterleavings
  // this is checked by send methods to "enqueue" messages in the 'q'
  var locked = true

  var blocked = false
  var blockedOn: Option[DummyFuture] = None
  // this is checked by the scheduler to "dequeue" tasks from the 'q'
  // it is necessary for become/unbecome atomicity
  var consuming = true

  var partitionNo: BigInt = 0

  var futuresPromised  : MMap[UUID, DummyFuture ] = MMap[UUID, DummyFuture]()
  var futuresWaitingFor: MMap[UUID, DummyFuture ] = MMap[UUID, DummyFuture]()

  // InternalState
  // var localState = MMap[String, Any]() // The old way of doing it
  // var localState = new LocalState(name) // the new way

  /*
   Nor how we replaced the old localState with the new stack, agent activation-frame, and a function called localState
   */
//  private var frame: ActivationFrame = new ActivationFrame()
//  frame.localState = new LocalState(name)
//  var stack: Stack[ActivationFrame] = Stack(frame)

  var stack: mutable.Stack[ActivationFrame] = mutable.Stack( new ActivationFrame )
  stack.last.localState = new LocalState()

//  var sender: Agent = _ // used exactly the same way as used in Akka Actor

  //=================================
  // utilities
  //=================================

  def localState: LocalState = stack.last.localState

  def hasWork: Boolean = {
    q.nonEmpty &&
      consuming &&
      (q.head.isInstanceOf[ResolveDummyFuture] || !blocked)
  }

  //==============================
  // Equality handling
  //==============================
//  override def hashCode: Int = {
//
////    val blockedOnHash = if (null == blockedOn) 0 else blockedOn.hashCode
////
////    q.hashCode +
////      defaultBehavior.hashCode +
////      stash.hashCode +
////      reactions.hashCode +
////      behaviors.hashCode +
////      specialReactions.hashCode +
////      oldBehaviors.hashCode +
////      locked.hashCode +
////      blocked.hashCode +
////      blockedOnHash +
////      consuming.hashCode +
////      //      localState.hashCode +
////      partitionNo.hashCode +
////      futuresPromised.hashCode +
////      futuresWaitingFor.hashCode +
//      name.hashCode
//  }

  override def equals(that: Any): Boolean = {
    that match{
      case null => false
      case _ => hashCode == that.hashCode
    }
  }

  def equalsDEBUG(that: Agent): Unit = {
    if (q != that.q) {
      println(s"THIS:$q" )
      println(s"THAT:${that.q}")
      assert( assertion = false, "Agent.equalsDEBUG -- queues are not equal" )
    }
    if (defaultBehavior != that.defaultBehavior) {
      println(s"THIS:$defaultBehavior" )
      println(s"THAT:${that.defaultBehavior}")
      assert( assertion = false, "Agent.equalsDEBUG -- defaultBehavior's are not equal" )
    }
    if (stash != that.stash) {
      println(s"THIS:$stash" )
      println(s"THAT:${that.stash}")
      assert( assertion = false, "Agent.equalsDEBUG -- stashes are not equal" )
    }
    if (reactions != that.reactions) {
      println(s"THIS:$reactions" )
      println(s"THAT:${that.reactions}")
      assert( assertion = false, "Agent.equalsDEBUG -- reactions are not equal" )
    }
    if (behaviors != that.behaviors) {
      println(s"THIS:$behaviors" )
      println(s"THAT:${that.behaviors}")
      assert( assertion = false, "Agent.equalsDEBUG -- behaviors are not equal" )
    }
    if (specialReactions != that.specialReactions) {
      println(s"THIS:$specialReactions" )
      println(s"THAT:${that.specialReactions}")
      assert( assertion = false, "Agent.equalsDEBUG -- specialReactions are not equal" )
    }
    if (oldBehaviors != that.oldBehaviors) {
      println(s"THIS:$oldBehaviors" )
      println(s"THAT:${that.oldBehaviors}")
      assert( assertion = false, "Agent.equalsDEBUG -- oldBehaviors are not equal" )
    }
    if (locked != that.locked) {
      println(s"THIS:$locked" )
      println(s"THAT:${that.locked}")
      assert( assertion = false, "Agent.equalsDEBUG -- locked fields are not equal" )
    }
    if (blocked != that.blocked) {
      println(s"THIS:$blocked" )
      println(s"THAT:${that.blocked}")
      assert( assertion = false, "Agent.equalsDEBUG -- blocked fields are not equal" )
    }
    if (blockedOn != that.blockedOn) {
      println(s"THIS:$blockedOn" )
      println(s"THAT:${that.blockedOn}")
      assert( assertion = false, "Agent.equalsDEBUG -- blockedOn fields are not equal" )
    }
    if (consuming != that.consuming) {
      println(s"THIS:$consuming" )
      println(s"THAT:${that.consuming}")
      assert( assertion = false, "Agent.equalsDEBUG -- consuming fields are not equal" )
    }
    if (partitionNo != that.partitionNo) {
      println(s"THIS:$partitionNo" )
      println(s"THAT:${that.partitionNo}")
      assert( assertion = false, "Agent.equalsDEBUG -- partitionNo fields are not equal" )
    }
    if (futuresPromised != that.futuresPromised) {
      println(s"THIS:$futuresPromised" )
      println(s"THAT:${that.futuresPromised}")
      assert( assertion = false, "Agent.equalsDEBUG -- futuresPromised fields are not equal" )
    }
    if (futuresWaitingFor != that.futuresWaitingFor) {
      println(s"THIS:$futuresWaitingFor" )
      println(s"THAT:${that.futuresWaitingFor}")
      assert( assertion = false, "Agent.equalsDEBUG -- futuresWaitingFor fields are not equal" )
    }
    if (name != that.name) {
      println(s"THIS:$name" )
      println(s"THAT:${that.name}")
      assert( assertion = false, "Agent.equalsDEBUG -- names are not equal" )
    }
  }

  //=======================
  // Initialization
  //=======================  
  def refresh( ): Unit = { // this method is the easiest way to set all contained state message+Agent parameters of e.g. behaviors
    // setting Agent in all executable code
    defaultBehavior.setAgent(this)
    reactions.setAgent(this)
    behaviors = behaviors.map { x => val beh = x._2; beh.setAgent(this); (x._1, beh) }
    specialReactions.setAgent(this)

    // setting toExecute statements in all actions 
    defaultBehavior.resetActions
    reactions.resetActions
    specialReactions.resetActions
    behaviors.values.foreach { x => x.resetActions }
  }

  //=======================
  // Future methods
  //=======================
  def promise(m: Message): DummyFuture = {
    // When overridden:
    // possibly has to do some work here
    ds.scheduler.blockingMgr.synchronized {
      val f = new DummyFuture(false, ds.scheduler.clock(), this, m.sender)
      //      futuresPromised(f.id) = f
      m.sender.futuresWaitingFor(f.id) = f
      f
    }

    // possibly more work here? so far NOT
  }

  //=======================
  // Special reactions (user override-able)
  //=======================
  if (specialReactions.reactions.isEmpty) {
    val sAc = new Action
    // I had to rename it not to conflict with UnLock event type (it was giving an error just for that...)
    import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{ UnLock => UnLockStmt }

    sAc + UnLockStmt.apply

    val jAc = new Action

    val rjAc = new Action

    val lAc = new Action

    val dAc = new Action

    val lfAc = new Action

    val stopAc = new Action

    val b = new Behavior("special")

    b.reactions = Map(
      new Start -> sAc,
      new Stop -> stopAc,
      new Join -> jAc,
      new ReJoin -> rjAc,
      new Leave -> lAc,
      new Demise -> dAc,
      new LinkFailed -> lfAc )
    b.setAgent(this)
    specialReactions = b
  }
  def onStart(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onStop(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onJoin(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onReJoin(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onLeave(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onDemise(m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def onLinkFailure( m: Message): Action = {
    // users override this one, we provide the stubs only
      specialReactions(m)
  }

  def setSpecialReaction(m: Message, action: Action): Unit = {
    require(m in specialReactions, "Agent.setSpecialReaction method - you can't add a reaction whose trigger isn't one of the special messages")
    action.setAgent(this)
    action.setMessage(m)
    specialReactions += (m, action)
  }

  //==================================
  // Tracing facilities
  //==================================


//  /**
//   * This method is used for copying the local state references of Agent
//   * objects.  The only difference between this one and normal copy is
//   * that it doesn't copy the frame stack so that it doesn't create
//   * infinite loop of recursions.
//   *
//   * When this method is used, referenceLink(ds) must be used in conjunction.
//   */
//  def referenceCopy: Agent = { // without copying the frame stack
//    //    Agent.fromJson(toJson)
//
//    // constant name referenced
//    val agent = new Agent(name)
//
//    // have to copy
//    agent.q = q map { x => x.copy }
//
//    // can copy or reference, will copy just in case
//    agent.defaultBehavior = defaultBehavior //.copy
//
//    // copied
//    agent.stash = stash map { x => x.copy }
//
//    // can referenced or copied, I copied
//    agent.reactions = reactions.copy
//
//    // can referenced or copied
//    agent.behaviors = behaviors map { x => (x._1 -> x._2.copy) }
//
//    // can reference or copy
//    agent.specialReactions = specialReactions.copy
//
//    // setting the special reaction to refer to the new agent copy reference
//
//    //    val actTMP = agent.specialReactions(new Start)
//
//    // removes last statement from action
//    //    actTMP - ()
//    //    // change this statement to "instrumented" unlock statement
//    ////    actTMP + Statement((m: Message, a: Agent) => { agent.scheduler.ds.unlock(agent) })
//    ////    actTMP + Statement(Statement.Kind.UNLOCK,agent)
//    ////
//    //
//    //    actTMP.setAgent(agent)
//    //    actTMP.reset
//    //    agent.specialReactions += (new Start -> actTMP)
//
//    // copied (it changes)
//    //    agent.timedActions = timedActions map { x => x.copy }
//
//    // copied, since it changes, but Strings are constants
//    agent.oldBehaviors = oldBehaviors map { x => x }
//
//    //    agent.name = name
//
//    // referenced, we can't copy the "runtime", only save some of its state
//    agent.scheduler = scheduler
//    // this is checked by send methods to "enqueue" messages in the 'q'
//    agent.locked = if (locked) true else false
//
//    //DEBUG
//    //    agent.locked == false
//
//    agent.blocked = if (blocked) true else false
//
//    // copied, it changes with time
//    agent.blockedOn = blockedOn match {
//      case Some(x) => Some(x.copy)
//      case _       => None
//    }
//
//    // this is checked by the scheduler to "dequeue" tasks from the 'q'
//    // it is necessary for become/unbecome atomicity
//    agent.consuming = if (consuming) true else false
//
//    // InternalState (copied with each object also copied)
//    //    agent.localState = localState map { x => (x._1, fromBytes[Any](toBytes(x._2))) }
//
//    // agent.localState = copyLocalState
//    // agent.localState = localState.copy
//
//    // copy stack of frames and replace the agent frame with stack.bottom
//    // agent.stack = stack.map(_.copy)
////    agent.frame = stack.last // last is the first that was pushed, i.e. agent's frame
//
//    agent.partitionNo = partitionNo
//
////    agent.futuresPromised = futuresPromised map { x => val f = x._2.copy; (f.id, f) }
//    futuresPromised map { x => val f = x._2.copy; agent.futuresPromised(f.id) = f }
////    agent.futuresWaitingFor = futuresWaitingFor map { x => val f = x._2.copy; (f.id, f) }
//    futuresWaitingFor map { x => val f = x._2.copy; agent.futuresWaitingFor(f.id) = f }
//    agent
//  }

//   def copy: Agent = { // copies the frame stack too
//
//     val agent = referenceCopy
//     // copy stack of frames and replace the agent frame with stack.bottom
//     agent.stack = stack.map(_.copy)
//     agent
//   }

//   /**
//    * This is a utility method not intended to be used by user's code.
//    *
//    * It takes care of cross-references between agents and their contained
//    * data structures after copying agents.
//    *
//    * This is to be called by the fresh copy of the distributed system copy method.
//    *
//    * @param ds the fresh copy of the Distributed system
//    */
//   def link(ds: DistributedSystem): Unit = {
//
//     /*
//      * To refresh
//      * - q
//      * - stash
//      * - blockedOn
//      * - futuresPPromised
//      * - futuresWaitingFor
//      * - behaviors all of them since all of them are taken care of in the "refresh" call above
//      */
//
//     this.scheduler = ds.scheduler
//
//     q map { x => x.link(ds) }
//
//     stash map { x => x.link(ds) }
//
//     futuresPromised map (x => x._2.link(ds))
//     futuresWaitingFor map (x => x._2.link(ds))
//
//     defaultBehavior.link(ds)
//     reactions.link(ds)
//     behaviors map { x => x._2.link(ds) }
//     specialReactions.link(ds)
//     localState.link(ds)
// //    frame.link(ds)
// //    stack(stack.size -1) = frame
//
//     // this will link stack, and localState at once
//     stack.map(_.link(ds))
//   }

//   def referenceLink(ds: DistributedSystem): Unit = {
//     // additional work
//     link(ds) // will try to link an empty stack, doesn't hurt
//     val actualAgent = ds.get(name)
//     stack = actualAgent.stack
//   }

  //----------------------------------------
  // Now get/set vars are handled inside the
  // LocalState.scala
  //----------------------------------------
  /**
   * If there is a variable in the agent's local state, returns its value. Otherwise, throw an exception/Error
   * @param variableName The variable whose value we need be returned by the method
   * @return the value of the 'variableName'
   */
  def getVariable(variableName: String): Any = {
    //    require(localState.contains(variableName), "Agent.getVariable() - localState doesn't contain the variable name: " + variableName)
    localState(variableName)
  }

  /**
   * Adds or modifies an already
   *
   * @param variableName the variable name appended with its type using the '[]' separator
   * @param value any object of type Any
   */
  def setVariable(variableName: String, value: Any): Unit = {
    require(variableName != null, "Agent.setVariable() - variable name is null!")
    localState.setVar(variableName, value)
  }

  def traceCopy: Agent = {
    Agent.fromJson(toJson)
  }

  def toJson: JValue = {

    val blockedOnFuture = blockedOn match {
      case Some(x) => Some(x.toJson)
      case _       => Some(JString("None"))
    }

    "Agent" ->
      ("name" -> name) ~
      ("q" -> q.map { x => x.toJson }) ~
      ("defaultBehavior" -> defaultBehavior.toJson) ~
      ("stash" -> stash.map { x => x.toJson }) ~
      ("reactions" -> reactions.toJson) ~
      // x._1 is the name of the behavior, x._2 is the Behavior object
      ("behaviors" -> behaviors.map { x => JField(x._1, x._2.toJson) }) ~
      ("specialReactions" -> specialReactions.toJson) ~
      //      ("timedActions" -> timedActions.map { x => x.toJson }) ~
      ("oldBehaviors" -> oldBehaviors) ~
      ("localState" -> localState.toJson) ~
      ("partitionNo" -> partitionNo) ~
      ("locked" -> locked) ~
      ("consuming" -> consuming) ~
      ("blocked" -> blocked) ~
      ("blockedOn" -> blockedOnFuture) ~
      ("futuresPromised" -> futuresPromised.map { x => x._2.toJson }) ~
      ("futuresWaitingFor" -> futuresWaitingFor.map { x => x._2.toJson }) ~
        JsonDSL.pair2jvalue( "stack" -> stack.map( _.toJson ) )
  }
  //==================================
  // Utilities
  //==================================
  override def toString: String = {
    name //+ ": queue = " + q.mkString(",")
  }
  def in(agents: Set[Agent]): Boolean = {
    require(agents != null, "Agent.in() method - doesn't accept null arguments")
    agents.contains(this)
  }

  def in(ds: DistributedSystem): Boolean = {
    require(ds != null, "Agent.in(ds) method - doesn't accept null arguments")
    ds.agents.exists { x => x.name == name }
  }

  def is(that: Agent): Boolean = {
    if (null == that)
      false
    else
      name == that.name

  }

  def snapshot: AgentState = AgentState(this)

  override def restore(state: AgentState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}

object Agent {
  def fromJson(js: JValue): Agent = {
    implicit val formats: DefaultFormats.type = net.liftweb.json.DefaultFormats
    //    js.extract[Agent]

    val ajs = js \ "Agent"

    val name = ajs \ "name" match {
      case JString(x) => x
      case _          => throw new Error("Agent.fromJson - can't extract 'name'")
    }

    val q: scala.collection.Seq[Message] = ajs \ "q" match {
      case JArray(x) => x map { z => Message.fromJson(z) }
      case _         => throw new Error("Agent.fromJson - can't extract 'q'")
    }

    val defaultBehavior = ajs \ "defaultBehavior" match {
      case x: JObject => Behavior.fromJson(x)
      case _          => throw new Error("Agent.fromJson - can't extract 'defaultBehavior'")
    }

    val stash = ajs \ "stash" match {
      case JArray(x) => x map { z => Message.fromJson(z) }
      case _         => throw new Error("Agent.fromJson - can't extract 'stash'")
    }

    val reactions = ajs \ "reactions" match {
      case x: JObject => Behavior.fromJson(x)
      case _          => throw new Error("Agent.fromJson - can't extract 'reactions'")
    }

    // extracting behaviors and names
    val behaviors: Map[String, Behavior] = ajs \ "behaviors" match {
      case JArray(listOfPairs) =>
        listOfPairs map {
          case JField(bName, b) => bName -> Behavior.fromJson(b)
          case _                => throw new Error("Agent.fromJson - can't extract a pair from 'behaviors'")
        } toMap
      case _ =>
        println(ajs \ "behaviors")
        throw new Error("Agent.fromJson - can't extract 'behaviors'")
    }

    // extracting special reactions
    val specialReactions = ajs \ "specialReactions" match {
      case x: JObject => Behavior.fromJson(x)
      case _          => throw new Error("Agent.fromJson - can't extract 'specialReactions'")
    }
    

    val oldBehaviorsSeq = ajs \ "oldBehaviors" match {
      case JArray(x) =>
        x map {
          case JString(z) => z
          case _          => throw new Error("Agent.fromJson - can't extract an old behavior")
        }
      case _ => throw new Error("Agent.fromJson - can't extract 'oldBehaviors'")
    }
    val oldBehaviors = mutable.Stack( oldBehaviorsSeq: _* ) // splice values of the sequence in a Stack

    // DEBUG
    //        println(ajs \ "localState")
    // extracting localState map
    //----------------------------------------
    // The following code was replaced by one line down there
    //----------------------------------------
    // val localStatePairs = ajs \ "localState" match {
    //   case JArray(List()) => Seq[(JValue, JValue)]()
    //   case JArray(x)      => x
    //   case _              => throw new Error("Agent.fromJson - can't extract a pair from 'localState'")
    // }
    // var localState = Map[String, Any]()
    // localStatePairs map {
    //   case JField(varName, varJArrayBytes) =>
    //     val valBytes = varJArrayBytes.extract[Seq[Byte]]
    //     localState = localState + (varName -> fromBytes[Any](valBytes))
    //   case _ => throw new Error("Agent.fromJson - can't extract a 'localState' entry")
    // }

    val locked = ajs \ "locked" match {
      case JBool(x) => x
      case _        => throw new Error("Agent.fromJson - can't extract 'locked'")
    }

    val consuming = ajs \ "consuming" match {
      case JBool(x) => x
      case _        => throw new Error("Agent.fromJson - can't extract 'consuming'")
    }

    val blocked = ajs \ "blocked" match {
      case JBool(x) => x
      case _        => throw new Error("Agent.fromJson - can't extract 'blocked'")
    }

    //DEBUG
    //    println(ajs \ "blockedOn")
    val blockedOn = ajs \ "blockedOn" match {
      case JString("None") => None
      case JObject(x)      => Some(DummyFuture.fromJson(x))
      case _               => throw new Error("Agent.fromJson - can't extract 'blockedOn'")
    }

    // extracting futures promised
    val futuresPromisedList = ajs \ "futuresPromised" match {
      case JArray(listOfFutures) => listOfFutures map { x => DummyFuture.fromJson(x) }
      case _                     => throw new Error("Agent.fromJson - can't extract 'futuresPromised' list")
    }

    //extracting futures waitingFor
    val futuresWaitingForList = ajs \ "futuresWaitingFor" match {
      case JArray(listOfFutures) => listOfFutures map { x => DummyFuture.fromJson(x) }
      case _                     => throw new Error("Agent.fromJson - can't extract 'futuresWaitingFor' list")
    }

    // extracting the partition number
    val partitionNo = ajs \ "partitionNo" match {
      case JInt(x) => x
      case _       => throw new Error("Agent.fromJson -- couldn't extract the 'partitionNo' field")
    }

    // extracting the call stack
    val stack = js \ "Agent" \ "stack" match {
      case JArray(x) => mutable.Stack( x map { y => ActivationFrame.fromJson( y ) }: _* )
      case _         => throw new Error("Agent.fromJson -- can't extract the call stack")
    }
  
    // populating both futures promised and waitingFor
    val futuresPromised = scala.collection.mutable.Map[ UUID, DummyFuture ]()
    futuresPromisedList foreach { x => futuresPromised( x.id ) = x }
  
    val futuresWaitingFor = scala.collection.mutable.Map[ UUID, DummyFuture ]()
    futuresWaitingForList foreach { x => futuresWaitingFor( x.id ) = x }

    // don't forget to assign extracted values to agent returned fields
    val agent = new Agent(name)

    agent.q = q
    agent.defaultBehavior = defaultBehavior
    agent.stash = stash
    agent.reactions = reactions
    agent.behaviors = behaviors
    agent.specialReactions = specialReactions
    //    agent.timedActions = timedActions.toSet
    agent.oldBehaviors = oldBehaviors
    // agent.localState = LocalState.fromJson(ajs \ "localState")

    agent.partitionNo = partitionNo
    // this should de-serialize the stack and the localstate
    agent.stack = stack
    // again the localState is defined as a function over the agent.frame

    agent.locked = locked
    agent.consuming = consuming
    agent.blocked = blocked
    agent.blockedOn = blockedOn
    agent.futuresPromised = futuresPromised
    agent.futuresWaitingFor = futuresWaitingFor

    agent
  }
}
