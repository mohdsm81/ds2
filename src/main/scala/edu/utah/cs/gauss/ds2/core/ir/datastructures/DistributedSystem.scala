package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.schedulers._
import edu.utah.cs.gauss.ds2.core.state.{ DistributedSystemState, Snapshot }
import edu.utah.cs.gauss.ds2.core.tracing._
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import java.util.UUID
import scala.collection.mutable.{ Stack, Set => MSet }
import scala.collection.parallel.ParSet

/**
 * @author
 *        Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 */
@SerialVersionUID(500)
class DistributedSystem(var name: String = "ds") extends Serializable with Snapshot[DistributedSystemState]{
  // A
  var agents = Set[Agent]()
  // M
  //  var messages = Set[Message]()
  // Gamma
  //  var actions = Set[Action]()
  // Delta 
  //  var behaviors = Map[String, Behavior]()
  // Chi, encoded in a finer grain manner in each statement object
  //  var actionToMessageSent: ActionsToSends = new ActionsToSends

  var scheduler: Scheduler = _ // Scheduler(this, Scheduler.Basic) // default but can be changed

  var tracingEnabled = false

  //==============================
  // attaching to a scheduler
  //==============================
  /**
   * Attaches a scheduler to this distributed system, and updates the scheduler's <code>ds</code> field
   * to point to this distributed system.
   *
   * @param sch the scheduler to be attached to this distributed system
   */
  def attach(sch: Scheduler): Unit = {
    scheduler = sch
    scheduler.ds = this
    scheduler.blockingMgr.ds = this
    refresh
  }

  //==============================
  // Future Handling additions
  //==============================
  var temporaries: ParSet[String] = ParSet()

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    name.hashCode +
      agents.hashCode +
      //      messages.hashCode +
      temporaries.hashCode
    //      + actions.hashCode 
    //      + behaviors.hashCode
  }

  override def equals(that: Any): Boolean = {
    that match{
      case null => false
      case ds: DistributedSystem => hashCode == that.hashCode
    }
  }

  //===================
  // init code
  //===================
  val bootStraper = new Agent("boot")
  bootStraper.ds = this
  bootStraper.locked = false

  //===================
  // implicits for cleaner code
  //===================

  implicit val ds = this

  implicit val clock: Function0[BigInt] = () ⇒ scheduler.clock()

  //===================
  // Populating special Messages
  //===================

  //  messages += new Start
  //  messages += new Stop
  //  messages += new Leave
  //  messages += new Demise
  //  messages += new LinkFailed
  //  messages += new Join
  //  messages += new ReJoin
  //  messages += new PoisonPill
  //  messages += new Message

  // ResolveDummyFuture is hard to add and verify since it has a dynamic field, so it is a special case anyways

  //==========================
  // Special Commands 
  // (communication)
  //==========================

  def send(src: Agent, m: Message, dst: Agent, special: Boolean = false): Unit = {

    require(src != null, "Send method - src agent is null")
    require(dst != null, "Send method - dst agent is null")
    require(m != null, "Send method - message to send is null")

    require((src in this) || src.name == bootStraper.name || special, s"Send method - the src agent is not in the distributed system: ${src.name}")
    require((dst in this) || src.name == bootStraper.name || special, s"Send method - the dst agent is not in the distributed system: ${dst.name}")
    // note that messages can be created on the fly so no 
    // guarantees the are in messages or not. We only have 
    // to make sure all "types"+"names" of messages are in "messages'

    //      if (!m.isInstanceOf[ResolveDummyFuture])
    //        require(m in this, "Send method - Message (" + m.name + ") is not known for this distributed system")

    if (!special)
      require(!dst.locked, "Send Method - the destination agent incoming queue is locked.")

    val te = new TraceEntry
    var dest = dst
    if (isTracingEnabled) {
      src.name match {
        case "boot" ⇒ te.event = new Send(bootStraper.traceCopy, m, dst.traceCopy, special)
        case _ ⇒ te.event = new Send(src.traceCopy, m, dst.traceCopy, special)
      }

      te.prior = traceCopy
    }

    // Read the note at the bottom to know why is this necessary.
    if (src.name == bootStraper.name)
      //        m.sender = bootStraper
      m.setSender(bootStraper)
    else {
      //        m.sender = src
      m.setSender(src)
    }

    // Error handling
    if (dst.locked && !special)
      throw new Error("Send-method - locked agent - Communication error, can't send to agent: " + dst.name)

    //actual work done
    // DEBUG
    //    def dstIsTemporary: Boolean = { dst.name.split("\\[\\]").size == 2 }
    //    if (dstIsTemporary) // a resolving send
    if (temporaries.contains(dst.name)) // a resolving send
    {

      /*
       * STEPS:
       * 1- First check the name of the "dst" actor against "temporaries"
       * 		1.1- if it is NOT there, then it is simply a non-resolving send. Proceed normally
       * 		1.2- if it is there:
       * 					a - call "resolve" method on the actual destination: ds.get(dst.name.split("\\[\\]")(0))
       * 						  setting the resolver to the "real" src, and future to the future found from the real 
       * 						  destination's "waitingFor" futures using the dst.name.split("\\[\\]")(1).
       * 					b - remove the temporary from "temporaries", it has done its life-task.
       * 2- Things should proceed normally from both Scheduler and dst.
       * 
       * NOTE: This send() can either act as RESOLVE or as regular SEND as before, DO NOT assume it 
       * works as both at the same time. Make sure it takes either path, not both, in one run.
       * 
       * Why? because a resolve calls send, and if send calls resolve, then infinite loop. Also, to avoid
       * duplicating a message in the dst buffer, in case no infinite loops.
       */

      // STEP 1.2

      val segments = dst.name.split("\\[\\]")
      val agent = get(segments(0))
      val future = agent.futuresWaitingFor(UUID.fromString(segments(1)))
      temporaries = temporaries - (dst.name) // step 1.2.b

      // We assume the value is at the head of the payload of the message
      resolve(future, m.payload.head) // step 1.2.a

      agents = agents - dst // send and ask checks if agents are there (temporary agents for futures)

      /*
         * NOTE:
         * "future" contains both real dst and real src. This is done by ask(...)
         */
    } else // non-resolving, normal, send
      // STEP 1.1
      m match {
        case x: ResolveDummyFuture ⇒ // system message treated specially
          if (dst.q.lastIndexWhere { x ⇒ x.isInstanceOf[ResolveDummyFuture] } == -1)
            dst.q = x +: dst.q // insert in front of the queue since this is a system msg
          else {
            val slice1 = dst.q.slice(0, dst.q.lastIndexWhere(_.isInstanceOf[ResolveDummyFuture]))
            val slice2 = dst.q.slice(dst.q.lastIndexWhere(_.isInstanceOf[ResolveDummyFuture]), dst.q.size - 1)
            dst.q = slice1 :+ x // add message to first slice
            dst.q = dst.q ++ slice2 // add the remaining elements
          }
        case _ ⇒ dst.q = dst.q :+ m
      }

    if (isTracingEnabled) {

      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }

    /*
     * Some explanation:
     * Why is the methods here use get(agent.name) instead of the passed agent?
     * 
     * This has to do with Copying.
     * 
     * Note that after we copy a DistributedSystem, yet keep the same code in the statements, it will refer to
     * older entities, except for the DS itself. Entities include src and dst agents for example in the send().
     * 
     * Other methods of course need to be aware of this too, and possibly more entities from old vs new 
     * distributed system and keep referring to what is in the "current" distributed system.
     * 
     */

  }

  def ask(src: Agent, m: Message, dst: Agent, special: Boolean = false): DummyFuture = {

    require(src != null, "Ask method - the src is null")
    require(m != null, "Ask method - the msgOut is null")
    require(dst != null && m != null, "Ask method - the dst is null")

    require(src in this, "Ask method - One of the src agent is not in the distributed system")
    require(dst in this, "Ask method - One of the dst agent is not in the distributed system")
    // note that messages can be created on the fly so no 
    // guarantees the are in messages or not. We only have 
    // to make sure all "types" of messages are in "messages'

    //      require((m in this), "Ask method - Message (" + m + "is not known for this distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      scheduler.traceManager.current += te
    }

    /* ADDING temporaries support for the ASK pattern
       * 
       * STEPS:
       * 1- take note of: src.name and future.id
       * 	1.1- in order to make temp = src.name + "[]" + future.id
       * 	1.2- add it to "temporaries"
       * 2- Set the src of the "send()" to: new Agent(temp), and set its locked field to false.
       *  2.1- Also, make the promiser to be the original "dst", in the dst.promise() method
       *  2.2- Now, if the temp-agent receives a message from original "dst",
       *  			temp-agent has both the REAL asker name, and the id of the 
       *  			future to resolve.
       * 3- any one that replies to temp-agent, send() should be able to detect that, and do the
       * 		necessary of resolving the future by invoking the ds.resolve() on the appropriate future.
       * 4- things should proceed normally as they were before.
       * 
       * NOTE: note that now with the new implementation, we really don't need the 
       * 			"Agent.futuresPromised" any more. Things should be transparent from the promiser point 
       * 			of view.
       */

    m.sendMethod = true // It indicates ask (as opposed to send), I forgot why we need it, but will keep it to avoid bugs.
    m.sender = src
    val result = dst.promise(m) // step 2.1
    val temp = new Agent(src.name + "[]" + result.id) // step 1.1
    temp.locked = false
    temporaries = temporaries + temp.name // step 1.2
    agents = agents + temp // send and ask checks if agents are there (temporary agents for futures)

    //      temp.futuresWaitingFor += (result.id -> result)

    send(temp, m, dst) // step 2

    if (!result.resolved) {
      dst.futuresPromised += (result.id -> result)
      src.futuresWaitingFor = src.futuresWaitingFor + (result.id -> result)
    }

    if (isTracingEnabled) {
      te.posterior = traceCopy
      te.event = new Ask(src.traceCopy, m, dst.traceCopy, result, special)
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
    result
  }

  def resolve(df: DummyFuture, value: Any = None): Unit = {
    require(df.promisedBy != null, "Resolve method - future.promisedBy is null")
    require(df.waitingFor != null, "Resolve method - future.waitingFor is null")
    require(df != null, "Resolve method - future is null")

    //some one could wait on behalf of someone !
    //    require(agents.contains(df.promisedBy) && agents.contains(df.waitingFor), "Resolve method - One of the resolver/asker agents are not in the distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Resolve(df.traceCopy)
      scheduler.traceManager.current += te
    }

    //    df.resolve(df.promisedBy, value)
    // Inlining the above here
    df.value = value
    df.resolved = true

    // I disabled these because the GET method complains if it doesn't
    // find the future in the Agent.futures-maps

    // TODO enable these if needed later. They are actually needed till now. 
    // But we better not rely on them for now. We better move them to get/timed-get later
    //     df.promisedBy.futuresPromised -= df.id
    //     df.waitingFor.futuresWaitingFor -= df.id
    df.storedInVariable match {
      // case Some(x) => df.waitingFor.localState += (df.id.toString -> value)
      case "" =>
        df.waitingFor.futuresWaitingFor(df.id) = df
      case x =>
        df.waitingFor.futuresWaitingFor(df.id) = df
        df.waitingFor.localState(x) = value // this way the x is checked against valid regex
    }

    // df.waitingFor.localState += (df.id.toString -> value)
    df.waitingFor.blocked = false

    //    send(df.promisedBy, new ResolveDummyFuture(df), df.waitingFor)
    send(df.promisedBy, new ResolveDummyFuture(df), df.waitingFor)

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += new TraceEntryEnd
    }

  }

  def create(p: Agent, nameOfNewOne: String): Agent = {
    require(p != null && nameOfNewOne != null && nameOfNewOne != "", "Create method One of create paramaters is null or name is empty string")
    // no two agents have same id, name is id here.
    require(!agents.exists { x ⇒ x.name == nameOfNewOne }, "Create method - name conflicts with another existing agent's name")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
    }

    val agentCreated = new Agent(nameOfNewOne)
    this + agentCreated
    agentCreated.q = Seq[Message]()
    agentCreated.defaultBehavior = p.defaultBehavior.traceCopy
    agentCreated.stash = Seq[Message]()
    agentCreated.reactions = p.defaultBehavior.traceCopy
    agentCreated.behaviors = p.behaviors.map {
      x ⇒
        val beh = x._2.traceCopy;
        (x._1 -> beh)
    }
    agentCreated.specialReactions = new Behavior("special")
    //    agentCreated.timedActions = Set[RuntimeTimedAction]()
    agentCreated.oldBehaviors = Stack[String]()
    agentCreated.name = nameOfNewOne
    agentCreated.ds.scheduler = scheduler
    agentCreated.refresh


    if (isTracingEnabled) {
      te.event = new Create(p.traceCopy, nameOfNewOne, agentCreated.traceCopy)
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
    agentCreated
  }

  def start(starter: Agent, started: Agent, args: Seq[Any] = Seq()): Unit = {

    require(starter != null && started != null, "Start method - One of start parameters is null")
    require((starter in this) && (started in this) || starter.name == bootStraper.name, "Start method - At least one of agents isn't present in the distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy

      starter.name match {
        case "boot" ⇒ te.event = new edu.utah.cs.gauss.ds2.core.tracing.Start(bootStraper.traceCopy, started.traceCopy)
        case _ ⇒ te.event = new edu.utah.cs.gauss.ds2.core.tracing.Start(starter.traceCopy, started.traceCopy)
      }

      scheduler.traceManager.current += te
    }

    val msg = new Start
    msg.payload = args
    // actual work done
    starter.name match {
      case "boot" ⇒ send(bootStraper, msg, started, true)
      case _ ⇒ send(starter, msg, started, true)
    }

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def stop(stopper: Agent, stopped: Agent): Unit = {
    require(stopper != null && stopped != null, "Stop method - One of stop parameters is null")
    require((stopper in this) && (stopped in this), "Stop method - At least one of agents isn't present in the distributed system")

    // tracing
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new edu.utah.cs.gauss.ds2.core.tracing.Stop(stopper.traceCopy, stopped.traceCopy)
      scheduler.traceManager.current += te
    }

    // actual work
    send(stopper, new Stop, stopped, true) // takes care of new references

    //tracing
    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }

    // stay locked forever, or till a start message arrives again (i.e. this is resume)
  }

  def kill(killer: Agent, victim: Agent): Unit = {

    require(killer != null && victim != null, "Kill method - One of kill parameters is null")
    require((killer in this) && (victim in this), "Kill method - At least one of the agents to kill isn't present in the distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Kill(killer.traceCopy, victim.traceCopy)
    }
    // not the no synchronization and messages in-flight will be lost
    send(killer, new PoisonPill, victim)
    // now we wait till possibly bad things happen due to scheduler

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def lock(a: Agent): Unit = {

    require(a != null, "Lock method - agent to lock is null")
    require((a in this), "Lock method - agent to lock isn't present in distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Lock(a.traceCopy)
    }

    a.synchronized { a.locked = true }

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def unlock(a: Agent): Unit = {
    require(a != null, "Unlock method - agent to lock is null")
    require((a in this), "Unlock method - agent to lock isn't present in distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Unlock(a.traceCopy)
    }

    a.locked = false

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def stopConsuming(a: Agent): Unit = {
    require(a != null, "StopConsuming method - agent to lock is null")
    require((a in this), "StopConsuming method - agent to lock isn't present in distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new StopConsuming(a.traceCopy)
    }

    a.synchronized { a.consuming = false }

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)

    }
  }

  def resumeConsuming(a: Agent): Unit = {
    require(a != null, "ResumeConsuming method - agent to lock is null")
    require((a in this), "ResumeConsuming method - agent to lock isn't present in distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new ResumeConsuming(a.traceCopy)
    }

    a.synchronized { a.consuming = true }

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def become(a: Agent, b: String, remember: Boolean = false): Unit = {
    require(a != null && b != null, "Become method - One of become parameters is null")
    require(a.behaviors.contains(b), "Become method - Unknown behavior to become")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Become(a.traceCopy, b, remember)

      // this is added so that any thing after happens after, stop/resume consuming.
      scheduler.traceManager.current += te
    }

    // actual work done
    stopConsuming(a)
    if (remember)
      a.oldBehaviors.push(a.reactions.name) // remember old behavior
    a.reactions = a.behaviors(b)
    resumeConsuming(a)

    if (isTracingEnabled) {
      // this should still reflect in the trace entry added above
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def unbecome(a: Agent): Unit = {
    require(a.oldBehaviors != null, "Unbecome method - agent to unbecome is null")
    require((a in this), "Unbecome method - agent to unbecome doesn't exist in the distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Unbecome(a.traceCopy)
      scheduler.traceManager.current += te
    }

    // actual work done
    // here we don't care about synchronization, msgs can still flow in
    if (a.oldBehaviors.size == 0) {
      //      println("Unbecome and empty oldBehaviors, restoring to default behavior")
      stopConsuming(a) // takes care of new references
      a.reactions = a.defaultBehavior
      resumeConsuming(a) // takes care of new references
    } else {
      stopConsuming(a) // takes care of new references
      a.reactions = a.behaviors(a.oldBehaviors.pop)
      resumeConsuming(a) // takes care of new references
    }

    if (isTracingEnabled)
      te.posterior = traceCopy
    scheduler.traceManager.current += TraceEntryEnd(te)
  }

  def stash(a: Agent, m: Message): Unit = {
    require(a != null && m != null, "Stash method - one of parameters is null")
    require((a in this), "Stash method - agent to stash msg from doesn't exist in the distributed system")
    require(a.stash != null, "Stash method - stash is null!")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Stash(a.traceCopy, m)
    }
    // imagine if scheduler takes a message to consume by force ...
    //stopConsuming(a)
    a.stash = a.stash :+ m
    //resumeConsuming(a)

    /*
     *  I don't think stopping consumption is any problem.
     *  the scheduler will keep consuming no matter what, during
     *  executing of the action ,,, things will get stashed!
     *  
     *  It, however, matters when it comes to unstashing.
     */

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def unstash(a: Agent): Unit = {
    require(a != null && a.q != null, "Unstash method - agent and/or its queue is null")
    require((a in this), "Unstash method - agent to stash msg from doesn't exist in the distributed system")
    require(a.stash != null, "Unstash method - stash is null!")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Unstash(a.traceCopy)
      scheduler.traceManager.current += te
    }

    /*
     *  not necessary to lock it, incoming msgs will be added at the back
     *  in constrast to unstashing (to the front)
     */
    //lock(a) // lock the queue to prevent concurrent mods
    stopConsuming(a) // also stop consuming since we will unstash to front of q
    if (a.stash.size >= 1) {

      a.q.lastIndexWhere { x ⇒ x.isInstanceOf[ResolveDummyFuture] } match {
        case -1 ⇒ // base case
          a.q = (a.stash.head) +: a.q // prepend the first message on stash infront of q
          a.stash = a.stash.tail // remove the first element from stash
        case x ⇒ // system messages are in front of the queue
          val slice1 = a.q.slice(0, x)
          val slice2 = a.q.slice(x, a.q.size - 1)
          a.q = slice1 ++ Seq(a.stash.head) ++ slice2
          a.stash = a.stash.tail
      }

    }

    // else do nothing
    resumeConsuming(a)
    //unlock(a)

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def unstashAll(a: Agent): Unit = {
    require(a != null && a.q != null, "UnstashAll method - agent and/or its queue is null")
    require((a in this), "UnstashAll method - agent to stash msg from doesn't exist in the distributed system")
    require(a.stash != null, "UnstashAll method - stash is null!")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new Unstash(a.traceCopy)
      scheduler.traceManager.current += te
    }

    //lock(a)
    stopConsuming(a) // takes care of new references

    if (a.stash.size >= 1) {

      a.q.lastIndexWhere { x ⇒ x.isInstanceOf[ResolveDummyFuture] } match {
        case -1 ⇒ // base case
          a.q = a.stash ++ a.q
          a.stash = Seq[Message]()
        case x ⇒ // system messages are in front of the queue
          val slice1 = a.q.slice(0, x)
          val slice2 = a.q.slice(x, a.q.size - 1)
          a.q = slice1 ++ a.stash ++ slice2
          a.stash = Seq[Message]()
      }
    }

    resumeConsuming(a) // takes care of new references
    //unlock(a)

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  // thread safe
  def hasWork: Set[Agent] = {
    //    agents.filter { x ⇒ x.synchronized { x.q.size > 0 && x.consuming && (!x.blocked || x.q.head.isInstanceOf[ResolveDummyFuture]) } }
    //    agents.filter { x ⇒  x.q.size > 0 && x.consuming && (!x.blocked || x.q.head.isInstanceOf[ResolveDummyFuture]) } 
    agents.filter { x ⇒ x.hasWork}

    /*
     * NOTE:
     * the reason why this doesn't detect the RF msg is that the rsend doesn't peel 
     * and wrap the message content to a temporary agent so that it is from
     * normal Message to ResolveDummyFuture type (a subtype of message)
     */

  }

  def bootStrap(a: Agent, args: Seq[Any] = Seq()): Unit = {
    require(a != null, "BootStrap method - The agent to boot strap is null")
    require((a in this), "Bootstrap method - The agent to boot strap doesn't exist in the distributed system")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new BootStrap(a.traceCopy)
      scheduler.traceManager.current += te
    }

    start(bootStraper, a, args) // takes care of new references

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  /**
   * This is due to scala not being able to handle an override methods when
   * both overridden and overriding both
   * with default arguments.
   * @param agentName
   */
  def bootStrap(agentName: String): Unit = {
    bootStrap(get(agentName))
  }
  def bootStrap(agentName: String, args: Array[Any]): Unit = {
    bootStrap(get(agentName), args)
  }

  //  def bootStrap(agnts: Set[Agent]): Unit = {
  //    require(agnts != null, "BootStrap Many method - the set of agents to boot strap is null")
  //    require(agnts.size >= 1, "BootStrap Many method - the agents-set to boot strap doesn't have agents!")
  //    require(agnts.forall { x => (x in this) }, "BootStrap Many method - NOT all agents exist in the distributed system")
  //
  //    val te = new TraceEntry
  //    if (isTracingEnabled) {
  //      te.prior = traceCopy
  //      te.event = new BootStrapAll(agnts.map { x => x.traceCopy })
  //      scheduler.traceManager.current += te
  //    }
  //
  //    // initialize them simultaneously as a transaction.
  //    agnts.synchronized {
  //      agnts.map { x => bootStrap(x) } // bootStrap(Agent) takes care of new references
  //    }
  //
  //    if (isTracingEnabled) {
  //      te.posterior = traceCopy
  //      scheduler.traceManager.current += TraceEntryEnd(te)
  //    }
  //  }

  /**
   * Checks if the future is resolved then it returns its value.
   * Otherwise it returns <code>None</code>.
   *
   * There is NO return value of this method!! a question arrizes, where is the value
   * of that future go then?
   * Ans: in the agents local state mapping that future's UUID.toString to the future's value
   *
   * @param f future involved. The reason why it is provided as a concrete object rather than a
   *        variable name is that the future to block on must materialize before we can block it it and query it.
   * @param variableName name of the variable at which to place the value that resolved the future.
   *  Note that it is optional, some times a process just needs to block in order to sync with other
   *  process(es) and not store the value of a future.
   * @param act the action that is possible to be suspended because of this future being unresolved.
   *
   */
  def get(f: DummyFuture, variableName: Option[String] = None, act: Action): Unit = {
    //    require(null != asker, "Blocking Get method - asker agent parameter is null!")
    require(null != f, "Blocking Get method - future parameter is null!")
    require(null != act, "Blocking Get method - action parameter is null!")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      scheduler.traceManager.current += te
    }

    val result =
      if (f.resolved) {
        updateLocalStateAndUnblock(f, variableName)
      } else {

        f.waitingFor.blocked = true // gets blocked in the Scheduler.execWithPossibleBlocking
        f.waitingFor.blockedOn = Some(f)

        // suspending the task and the rest such as blocking the
        // task will be taken care of in the Scheduler.execWithPossibleBlocking
        // method
        None
      }

    if (isTracingEnabled) {
      te.event = new Get(f.waitingFor.traceCopy, f.traceCopy, act.traceCopy, result)
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  /**
   * Checks if the future is resolved, if not it blocks till <code>timeout</code> reaches ZERO.
   * Then, it reschedules resuming from the statement after the get.
   *
   * @param f the future on which the agent may/not block
   * @param timeout the time out in terms of scheduler ticks.
   * @param act the action to be suspended, and later resumed when resolved/timeout
   * @return The value of the future if it has been resolved within the timeout, or
   * <code>None</code> if it didn't.
   */
  def getTimed(f: DummyFuture, variableName: Option[String] = None, timeout: BigInt, act: Action): Unit = {
    require(null != f, "Timed Get method - future is null")
    require(null != act, "Timed Get method - Action is null")
    require(timeout >= 0, "Timed Get method - timeout is negative?!!")
    require(f.promisedBy.futuresPromised.contains(f.id), "Timed Get method - The dst agent says it doesn't know if it promised some other aget with a future like that.")

    /*
     * STEPS:
     * 1- At the time of calling, check if resolved then return if yes.
     * 
     * 2- Otherwise, add the agent to blockingManager, and associate 
     * it with its currently running Action (to keep the PC state). And
     * schedule a timed action to timeout and resume from there.
     * 
     * 3- Either it will be unblocked by another agent meanwhile, resolving
     * the future.
     * 
     * 4- OR, the timed action YOU schedule to execute ONLY ONCE will time 
     * out and check again:
     * 
     * IF resolved:
     *   1. update the Agent's local state
     *   2. And mark the future as resolved
     *   3. trigger the action (which is the suspended action, because we 
     *   need to advance its current state - its PC)
     *   4- "Reschedule" the action in its current state using the same 
     *   message that triggered it (the action itself stores the message 
     *   and agent.
     * ELSE
     *  Skip the TimedGet statement that blocked it in the first place 
     *  and resume from there.
     */

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      scheduler.traceManager.current += te
    }

    // STEP 1
    val result =
      (
        if (f.resolved) {
          updateLocalStateAndUnblock(f, variableName)
        } // STEP 2 + 3
        else {
          /*
           * Steps:
           * 1- make a timed action
           * 2- schedule it
           * 3- make the agent also blocked and on the future 
           *    (temporarily as the action triggers should 
           *    unblock and resume normal operation)
           */
          f.waitingFor.blocked = true
          f.waitingFor.blockedOn = Some(f)

          /*
           * NOTE:
           * - What happens if it was not resolved by another future, yet the timed-get timed out?
           * 
           * - Answer: we "skip" that timed-get statement and nothing happens, we just continue
           * executing the remaining statements of the action!
           * 
           * - How to "skip" a statement?
           * - Call Action.advancePC method!
           */

          // this is the statement to be executed by the timed action (executes on timeout of the get-future)
          val code: Function2[Message, Agent, Unit] = (m: Message, a: Agent) ⇒ {
            if (f.resolved)
              updateLocalStateAndUnblock(f, variableName)
            else {
              skipAndUnblock(f, act)
            }
          }

          val newAction = new Action

          newAction + Statement(code)
          newAction.setAgent(f.waitingFor)
          newAction.setMessage(act.m)
          newAction.reset // has to reset the newAction.toExecute to newAction.stmts, otherwise no code to execute!

          val timedAction = new TimedAction(timeout, timeout, newAction, 1, scheduler.clock())
          // when the timed action happens, it has to execute the stmt above
          scheduler.schedulePeriodic(timedAction)
          None
        }
      )

    if (isTracingEnabled) {
      te.event = new GetTimeout(f.waitingFor.traceCopy, f.traceCopy, timeout, act.traceCopy, result)
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  private def skipAndUnblock(f: DummyFuture, act: Action) = {
    act.advancePC
    scheduler.blockingMgr.unblock(f.waitingFor)
  }

  private def updateLocalStateAndUnblock(f: DummyFuture, variableName: Option[String]): Option[Any] = {
    variableName match {
      case None =>
        // f.waitingFor.localState += (f.id.toString -> f.value)
        scheduler.blockingMgr.unblock(f.waitingFor) // it reschedules all unblocked
        None
      case Some(x) =>
        f.waitingFor.localState(x) = f.value
        scheduler.blockingMgr.unblock(f.waitingFor) // it reschedules all unblocked
        Some(f.value)
    }
  }

  // private def doItTimed(f: DummyFuture, variableName: Option[String]): Option[Any] = {
  //   var result: Option[Any] = None
  //   (f.resolved, variableName) match {
  //     case Tuple2(true, Some(x)) =>
  //       f.waitingFor.localState(x) = f.value
  //       scheduler.blockingMgr.unblock(f.waitingFor)
  //       Some(f.value)
  //     case Tuple2(false, _) =>
  //       scheduler.blockingMgr.unblock(f.waitingFor)
  //       None
  //     case _ => None
  //   }
  // }

  def stopSystem: Unit = {
    ???

    /*
     * This is a research task! 
     * 
     * How can we guarantee an orderly termination that ACTUALLY terminates?!!...
     * 
     * Imagine this: if the currently remaining tasks to execute keep sending and generating messages,
     * then all receiving agents will either one of the following:
     * - not able to receive them if their queues are locked
     * - keep processing and generating more tasks that may, in turn, generate more messages flying around, if their 
     *   queues are not locked. 
     *   
     * How to prove termination of a distributed system so that we say: ok, the distributed system is stopped in an orderly manner? 
     */

    /*
     * GANESH: find any algorithm to implement here, do not do any termination things. it is difficult. 
     */
  }

  def shutdownSystem: Unit = {

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new ShutdownAll
    }

    agents = Set()

    scheduler.shutdown

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  def isShutdown: Boolean = {
    agents.isEmpty &&
      //      messages.isEmpty &&
      //      actions.isEmpty &&
      //      behaviors.isEmpty &&
      //      actionToMessageSent.a2s.isEmpty &&
      scheduler.isShudown
  }
  //=======================================
  // Utility functions
  //=======================================

  def refresh: Unit = {
    agents.map { x ⇒
      x.ds = this
      x.ds.scheduler = scheduler
      x.refresh
    }
  }

  def contains(agentName: String): Boolean = {
    agents.find { x ⇒ x.name == agentName } != None
  }

  /**
   * Adds an agent to the distributed system and populates all related fields of
   * the distributed system by the respective Agent's data.
   * @param a
   */
  def +(a: Agent): DistributedSystem = {

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = traceCopy
      te.event = new AddAgent(a.traceCopy)
      scheduler.traceManager.current += te
    }

    // add it to agents
    a.ds = this
    agents = agents + a

    // DEBUG
    //    a.link(this)

    // all remaining state we are NOT supposed to mess up with

    if (isTracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }

    this
  }

  def get(agentID: Any): Agent = get(agentID.toString)
  
  def get(agentName: String): Agent = {
    val a = agents.find { x ⇒ x.name == agentName }
    if (a == None)
      null
//      throw new Error(s"There is no agent with the specified name in this distributed system: $agentName")
    else
      a.get
  }

  def snapshot: DistributedSystemState = DistributedSystemState(this)

  def restore(state: DistributedSystemState): Unit = {
    state.instanceToRestore = this
    state.restore
  }

  /**
   * A deep copy using json utilities.
   * @return a copy of the calling DistributedSystem object.
   */
  def traceCopy: DistributedSystem = {
    DistributedSystem.fromJson(toJson)
  }

  def is(that: DistributedSystem): Boolean = {
    name == that.name
  }

  override def toString: String = {
    //    "DistributedSystem '" + name + "': \n===============\n" + agents.toString + "\n" +
    //      "Scheduler: \n===============\n" + scheduler

    def toString(a: Agent): String = {
      a.name + "--->" + a.q.map { _.name }.mkString(",")
    }

    "DistributedSystem '" + name +
      "' agents: \n===============\n" +
      agents.map { toString(_) }.mkString("\n") +
      "\n===============\n"
  }

  //=======================================
  // Tracing
  //=======================================
  def toJson: JValue = {
    ("DistributedSystem" ->
      ("name" -> this.name) ~
      ("agents" -> agents.map { x ⇒ x.toJson }) //      ("messages" -> messages.map { x ⇒ x.toJson })
    )
  }

  // ======================================
  // Faults Semantics
  // ======================================

  /**
   * Agent - from which a message is dropped
   * Message - the dropped message (while the sender is encapsulated inside the
   * Message)
   */
  type DroppedMessage = (Agent, Message)
  //  /**
  //    * Drops a message from an agent and places it in an optional sink.
  //    * @param a the agent whose intended message is dropped from its queue
  //    * @param m the message to be dropped
  //    * @param sink the optional sink to which dropped messages are added
  //    * @return <code>true</code> if the message is dropped; <code>false</code>
  //    * otherwise.
  //    */
  //  def drop( a: Agent, m: Message, sink: MSet[DroppedMessage] = MSet() ): Unit = {
  //
  //  }
  /**
   * Drops a message from an agent (indexed
   * with its location) and places it in an optional sink
   *
   * @param a the agent from which the message is dropped
   * @param idx the index of the agent's queue at which the message to be
   * dropped is
   * @param sink the optional sink to which the dropped message is added
   * @return <code>true</code> if the message is dropped; <code>false</code>
   * otherwise.
   */
  def drop(a: Agent, idx: Int, sink: MSet[DroppedMessage] = MSet[DroppedMessage]()): Unit = {
    require(null != a, "DistributedSystem.drop(m) -- agent is null")
    require(0 <= idx && idx < a.q.size, "DistributedSystem.drop(m) -- message is null")

    // tracing
    val te = new TraceEntry
    if (tracingEnabled) {
      te.prior = traceCopy
      te.event = DropMessage(a, idx, a.q(idx))
      scheduler.traceManager.current += te
    }

    // first we take note of the message
    sink += (a -> a.q(idx))

    // then we drop it
    val (p1, p2) = a.q.splitAt(idx)
    a.q = p1 ++ p2.tail // the element on that index is added to the second sub-sequence splitted

    // tracing
    if (tracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
  }

  /**
   * Duplicates a message in the agent's queue and places it in the indicated
   * index, or
   * at the back of the queue if the index exceeds the size of the queue. If
   * the message isn't found in the queue, then the method doesn't do anything
   * and returns false.
   * @param a the agent whose message is duplicated
   * @param m the message to be duplicated
   * @param idx the index at which the duplicate message is inserted
   * @return <code>true</code> if it succeeds finding the message to duplicate,
   * <code>false</code> otherwise.
   */
  def duplicate(a: Agent, m: Message, idx: Int): Unit = {
    require(a.q.contains(m), "DistributedSystem.duplicate(a,m,idx) -- can't duplicate a message that isn't in the agent's queue")
    require(null != a, "DistributedSystem.duplicate(a,m,idx) -- agent is null")
    require(null != m, "DistributedSystem.duplicate(a,m,idx) -- message is null")
    // yes, you can duplicate a message to stay at the end of a sequence, that is idx == size
    require(0 <= idx && idx <= a.q.size, "DistributedSystem.duplicate(a,m,idx) -- index should be not null and >= 0")

    // tracing
    val te = new TraceEntry
    if (tracingEnabled) {
      te.prior = traceCopy
      te.event = DuplicateMessage(a, m, idx)
      scheduler.traceManager.current += te
    }

    // actual work
    val (p1, p2) = a.q.splitAt(idx)
    val newMsg = new Message(m.name,get(m.sender.name), m.sendMethod, m.payload)
    a.q = p1 ++ Seq(newMsg) ++ p2

    // tracing
    if (tracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += te
    }

  }
  /**
   * Duplicates a message in the
   * indicated by the index (idx1)
   * in agent's queue and places it in the indicated
   * index
   * (idx2), or
   * at the back of the queue if the index exceeds the size of the queue. If
   * the message isn't found in the queue, then the method doesn't do anything
   * and returns false.
   * @param a the agent whose message is duplicated
   * @param idx1 the index of the message to be duplicated
   * @param idx2 the index at which the duplicate message is inserted
   * @return <code>true</code> if it succeeds finding the message to duplicate,
   * <code>false</code> otherwise.
   */
  def duplicate(a: Agent, idx1: Int, idx2: Int): Unit = {
    duplicate(a, a.q(idx1), idx2)
  }

  /**
   * Determines two unused partition numbers by any partition in the distributed
   * system, and returns them.
   * @return Two un used partition numbers that can be used for newly created
   * partition numbers.
   */
  def twoUnUsedPartitionNumbers: (BigInt, BigInt) = {
    var idx1: BigInt = 0
    var idx2: BigInt = 0
    val uniques = Set(agents.map(_.partitionNo))
    while (uniques.contains(Set(idx1)))
      idx1 += 1

    val uniques2 = uniques + Set(idx1)
    while (uniques2.contains(Set(idx2)))
      idx2 += 1
    (idx1, idx2)
  }

  /**
   * Determines unused partition number by any partition and returns it
   * @return an unused partition number that can be used to create a new
   * partition.
   */
  def oneUnusedPartitionNumber: BigInt = {
    var idx: BigInt = 0
    val uniques = Set(agents.map(_.partitionNo))
    while (uniques.contains(Set(idx)))
      idx += 1
    idx
  }

  /**
   * Partitions the two sets of agents into two partitions, then confirms by
   * returning <code>true</code> if two partitions were created. It returns
   * <code>false</code> if it didn't create additional partitions, i.e. the two
   * sets of agents are already partitioned. That is, when ALL agents from each
   * set is living in the same partition and the two sets are already
   * partitioned.
   * If the agents in one set are in different partitions, then they are added
   * to the same
   * resultant partition with
   * their peers in the the same
   * set. The method returns the partition numbers to which each set is added.
   * It makes sure it uses a partition number that is not currently used by the
   * rest of existing partitions before the call to this method.
   * @param parted the first set of agents to form a partition
   */
  def partition(parted: Set[Agent]): BigInt  = {

    // tracing
    val te = new TraceEntry
    if (tracingEnabled) {
      te.prior = traceCopy
      scheduler.traceManager.current += te
    }

    // actual work
    val p = oneUnusedPartitionNumber
    agents.filter(parted.contains(_)).map(_.partitionNo = p)

    // val (one, two) = twoUnUsedPartitionNumbers
    // part1.toSeq map (_.partitionNo = one)
    // part2.toSeq map (_.partitionNo = two)

    // tracing
    if (tracingEnabled) {
      te.event = Partitioning(parted, p)
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }
    p
  }

  /**
   * Creates a new partition from all agents passed in the set, and assigns it
   * the partition number based on the <code>partitionNo</code> function passed
   * to it. By default the function for the resultant partition number is
   * determined to be the minimum of all
   * @param part1 one set of agents to be put in one partition with another partition
   * @param part2 the other partition to be unified
   * @return the partition number created from all agents
   */
  def unpartition(part1: Set[Agent], part2: Set[Agent]): BigInt = {

    require(null != part1, "DistributedSystem.unpartition -- part1 can't be null")
    require(null != part2, "DistributedSystem.unpartition -- part2 can't be null")
    require(!part1.isEmpty, "DistributedSystem.unpartition -- part1 can't be empty")
    require(!part2.isEmpty, "DistributedSystem.unpartition -- part2 can't be empty")
    require((part1 union part2) forall(_ in this), "DistributedSystem.unpartition -- at least one agent in the two partitions isn't in the distributed system")

    //tracing
    val te = new TraceEntry
    if (tracingEnabled) {
      te.prior = traceCopy
      te.event = UnPartitioning(part1,part2)
      scheduler.traceManager.current += te
    }

    // actual work
    val p = twoUnUsedPartitionNumbers
    part1 map { _.partitionNo = p._1 }
    part2 map { _.partitionNo = p._2 }

    // tracing
    if (tracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }

    // returning the partition number
    p._1
  }

  def crash(a: Agent): Unit = {
    require(a in agents, "DistributedSystem.crash -- can't crash an agent that isn't in the distributed system")

    // tracing
    val te = new TraceEntry
    if (tracingEnabled) {
      te.prior = traceCopy
      te.event = Crashing(a)
      scheduler.traceManager.current += te
    }

    // actual work
    a.locked = true
    agents = agents.filter(_ != a)

    // tracing 
    if (tracingEnabled) {
      te.posterior = traceCopy
      scheduler.traceManager.current += TraceEntryEnd(te)
    }

  }

  // ======================================
  def enableTracing: Unit = {
    tracingEnabled = true
  }

  def disableTracing: Unit = {
    tracingEnabled = false
  }

  def isTracingEnabled: Boolean = {
    tracingEnabled
  }
}

object DistributedSystem {
  def fromJson(js: JValue): DistributedSystem = {
    implicit val formats = DefaultFormats
    //        js.extract[DistributedSystem]

    val name = js \ "DistributedSystem" \ "name" match {
      case JString(x) ⇒ x
      case _ ⇒ throw new Error("DistributedSystem.fromJson - can't extract 'name'")
    }

    val agents = js \ "DistributedSystem" \ "agents" match {
      case JArray(x) ⇒ x map { z ⇒ Agent.fromJson(z) }
      case _ ⇒ throw new Error("DistributedSystem.fromJson - can't extract 'agents'")
    }

    val ds = new DistributedSystem(name)

    // populate the DS fields from extracted js values
    ds.agents = agents.toSet

    ds
  }

}
