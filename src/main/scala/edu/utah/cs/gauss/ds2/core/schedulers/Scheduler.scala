package edu.utah.cs.gauss.ds2.core.schedulers

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{ElseIf, If, While}
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.FunctionCalling
import edu.utah.cs.gauss.ds2.core.state.{SchedulerState, Snapshot}
import edu.utah.cs.gauss.ds2.core.tracing._
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.collection.Seq

/**
 * @author Mohammed S. Al-Mahfoudh
 * 		   mahfoudh@cs.utah.edu
 * 		   Gauss Group - SoC
 * 		   The University of Utah
 *
 *
 *  To use a scheduler, it is important to keep in mind that the following sequence of
 *  methods should be executed <b>in-order</b>:
 *  1. call to pick/pick(agent) to extract a task
 *  2. the task extracted from above should be scheduled using schedule, scheduleOnce
 *    - in case of timed actions schedulePeriodic must be used instead.
 *  3- a call to execute to execute a task from the consumedQ by a thread
 *    - in case of stepping through an action's statements, use execute but schedule the same
 *      <code>StatementExecutionThread</code> more than once. DO NOT however interleave this thread's
 *      statements with other thread's actions/statements, its a violation of the operational semantics
 *      as we don't support fine-grained interleaving of agents' actions. We assume each agent's actions
 *      are atomically executed uninterrupted as a transaction. For blocked agents, this is the only time
 *      where we need to use fine grained interleaving of actions' statements, they can be pre-empted
 *      and their action's statements has to interleave with other agent's in order to resolve the future
 *      they are blocked by.
 */
@SerialVersionUID(1400)
trait Scheduler extends Serializable with Snapshot[SchedulerState] {

  private implicit val scheduler = this

  // once a thread blocks, update the blocked set, decrement available threads num
  var numThreads: Int = 1 // default scheduler is a single threaded one
  //  var schedulingAlgorithm: Algorithm = Basic
  var clk: BigInt = 0 // abstract clock
  implicit val clock = () ⇒ clk
  implicit var ds: DistributedSystem = _

  // decided to get rid of threadPool as it is useless and introduces some problems
  //  var tp: ExecutorService = Executors.newFixedThreadPool(numThreads)
  var taskQ: TaskQ = TaskQ(scheduler) // Seq[SuspendableTask]()
  var consumeQ = Seq[Statement]()
  // execute queue is already there inside the actual thread pool.
  private var tracingEnabled = false
  //---------------------
  // managers/trackers
  //---------------------
  // futures and which agents are blocked by them
  var blockingMgr = new BlockedTasksManager
  // timed actions
  var timedActionsTracker = new TimedActionsTracker
  // traces
  var traceManager = new TraceManager

  //==============================
  // Attach a ds to this scheduler
  //==============================
  /**
   * This is analogous to the DistributedSystem.attach method. Please (@see
   * DistributedSystem.attach)
   * @param ds the distributed system to be attached to this scheduler
   */
  def attach(ds: DistributedSystem): Unit = {
    this.ds = ds
    this.ds.scheduler = this

    blockingMgr.ds = this.ds
    ds.refresh
  }

  //==================
  // Interface methods
  //==================

  def enableTracing: Unit = {
    tracingEnabled = true
  }

  def disableTracing: Unit = {
    tracingEnabled = false
  }

  def isTracingEnabled: Boolean = {
    tracingEnabled
  }

  /**
   * This method advances the scheduler's clock and manages the timed actions.
   */
  def tick: Unit = {
    val te = new TraceEntry
    val ev = new Tick

    //tracing
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = ev
    }

    // actual work
    clk += 1
    val triggered = timedActionsTracker.tick

    // tracing
    if (isTracingEnabled) {
      if (None != triggered)
        ev.triggered = triggered.get.map { x ⇒ x.traceCopy }
      else
        ev.triggered = Set()
    }

    // actual work
    if (triggered != None)
      triggered.get.map { x ⇒ execute(x, 1) }

    // tracing
    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += te
      traceManager.current += TraceEntryEnd(te)
    }
  }

  private def pickAll(a: Agent): Seq[SuspendableTask] = {
    // the power of functional!
    /*
       * Explanation:
       * 1- a.q map all of those (instead of counting and for-loop)
       * 2- pick a task from a.q each time
       * 3- splice the resulting collection
       * 4- make a mutable sequence out of them
       * 
       */
    scala.collection.mutable.Seq((ds.get(a.name).q.map { _ ⇒ pick(a) }): _*)
  }

  /**
   * Picks a task from the specific agent in the parameter.
   * This method doesn't advance the scheduler's clock.
   *
   * @param a the agent whose queue from which a task is dequeued.
   * @return the task.
   */
  private def pick(a: Agent): SuspendableTask = {
    // contract
    require(ds.hasWork.contains(a), "Scheduler.pick(a) method - Agent doesn't have work, can't pick a task from its queue")
    require(ds != null, "Distributed System is null!")
    require(a != null, "Agent to pick a task from is null!")
    require(ds.agents.contains(a), "Agent " + a.name + "isnt part of the distributed system")
    require(ds.hasWork.contains(a), "Agent " + a.name + "dowsn't have tasks")
    require(a.consuming, "Agent " + a.name + "isn't enabled for consuming.")

    // tracing
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      traceManager.current += te
    }

    // actual work
    val m = ds.get(a.name).q.head

    // dummy picked task (scala restriction to init all local vars...)
    val act = new Action
    act.setAgent(ds.get(a.name))
    act.setMessage(m)
    act.reset
    var pickedTask: SuspendableTask = new SuspendableTask(act)

    // Special reactions
    pickedTask =
      if (m.isInstanceOf[ResolveDummyFuture]) {
        //          a.synchronized {
        val msg = m.asInstanceOf[ResolveDummyFuture]
        val resolvedFuture = msg.payload.head.asInstanceOf[DummyFuture]
        // Create an action that resolves the future
        val act = new Action
        val stmt = Statement((message: Message, agent: Agent) ⇒ {
          ds.resolve(resolvedFuture, resolvedFuture.value)
          blockingMgr.setResolved(resolvedFuture)
          // then when the resolvedFuture.waitingFor calls get, he gets the value
        })
        act + (stmt)
        act.m = m
        act.a = ds.get(a.name)
        act.reset
        new SuspendableTask(act)
      } else if (m in ds.get(a.name).specialReactions) {
        // does implicit action.runtimecopy
        val action = ds.get(a.name).specialReactions(m)
        action.m = m
        action.a = ds.get(a.name)
        action.reset
        new SuspendableTask(action)
      } else { // generic case
        // does implicit action.runtimecopy
        val action = ds.get(a.name).reactions(m)
        action.m = m
        action.a = ds.get(a.name)
        action.reset
        new SuspendableTask(action)
      }

    // tracing
    if (isTracingEnabled) {
      te.event = new Pick(a, pickedTask)
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }

    // now get rid of older message from the agent queue
    ds.get(a.name).q = ds.get(a.name).q.tail

    pickedTask
  }

  /**
   * An execute method specialized for RuntimeTimedActions
   * @param timedAction the runtime timed action already triggered and need be executed.
   * @param threadID the id of the thread that will execute the timed action
   */
  def execute(timedAction: TimedAction, threadID: Long): Unit = {

    // tracing
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new ExecuteTimed(timedAction.traceCopy, threadID)
      traceManager.current += te
    }

    // actual work
    val a = ds.get(timedAction.action.a.name)
    val action = timedAction.action
    action.setAgent(a) // sets agent field in all stmts
    action.reset
    //    val task = new SuspendableTask(action )

    action.execute
    /*
     * Weird huh?
     * Because (1) timed actions don't block, (2) they by pass every current statement in the
     * consumeQ and execute.
     * 
     *  It is like pre-empting all the consumeQ at once, then executing the timed action, 
     *  then resuming the pre-empted ones all at once. 
     * 
     */

    //    val t = StandardExecutionThread( task, threadID )
    //    t.start
    //    t.join

    // tracing
    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }
  }

  def executeWhile(threadID: Long)(condition: ⇒ Boolean): Unit = {
    var cnt = 0
    while (!consumeQ.isEmpty && condition) {
      execute(threadID)
      cnt += 1
    }
    cnt
  }

  def consumeWhile(a: Agent)(condition: ⇒ Boolean): Int = {
    var cnt = 0
    while (!taskQ(a).isEmpty && !consumeQ.head.a.blocked && condition) {
      consume(ds.get(a.name))
      cnt += 1
    }
    cnt
  }
  def consumeWhile(agentName: String)(condition: ⇒ Boolean): Int = {
    consumeWhile(ds.get(agentName))(condition)
  }

  // def executeWhile(threadID: Long = 1)(condition: ⇒ Boolean): Int = {
  //   var cnt = 0
  //   while ((!consumeQ.isEmpty) && condition) {
  //     execute(threadID)
  //     cnt += 1
  //   }
  //   cnt
  // }

  def scheduleWhile(a: Agent)(condition: ⇒ Boolean): Int = {
    var cnt = 0
    while ((a in ds.hasWork) && !consumeQ.head.a.blocked && condition) {
      schedule(ds.get(a.name))
      cnt += 1
    }
    cnt
  }
  def scheduleWhile(agentName: String)(condition: ⇒ Boolean): Int = {
    scheduleWhile(ds.get(agentName))(condition)
  }

  /**
   * Adds a task to the scheduler's taskQ for consumption and then execution.
   */
  def schedule(task: SuspendableTask): Unit = {

    // trace
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new DoSchedule(task.traceCopy)
      traceManager.current += te
    }

    // actual work
    if (!task.isSuspended) {
      // so blocking stmts know which task is to be blocked
      // what about "preemption" on blocking? also that helps in preemption
      task.action.toExecute map (_.taskFrom = task)
      taskQ :+ task
      //      taskQ = taskQ :+ task
    }

    // trace
    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }
  }
  def schedule(agentName: String): Unit = {
    schedule(ds.get(agentName))
  }
  def schedule(a: Agent): Unit = {
    val debug = ds.hasWork
    require(ds.hasWork.contains(a), s"Scheduler.schedule -- tried to schedule from an agent (${a.name}) who doesn't have a task")

    if (ds.get(a.name).q.head.isInstanceOf[ResolveDummyFuture]) {
      handleFuture(ds.get(a.name))
    } else {
      val task: SuspendableTask = pick(a)
      schedule(task)
    }

  }

  private def handleFuture(a: Agent): Unit = {
    val rfMsg = ds.get(a.name).q.head
    val f = rfMsg.payload[DummyFuture](0)

    // matching over variableName where the future will end up
    LocalState.isValidVarName(f.storedInVariable) match {
      // note the pattern, it needs to be varName+DELIM+typeName
      case true => ds.get(a.name).localState(f.storedInVariable) = f
      case false => ; //do nothing
    }
    // advance the queue
    ds.get(a.name).q = ds.get(a.name).q.tail

    /*
     STEPS:
     1- payload format = Seq(future, variableNameFutureStored)
     2- update the future living in the asker.localState(variablenamefuturestored) to future
     3- done!

     The one that will access the value of the future, then, is either get/timed-get and can optionally
     update the localState to have the value stored in the future stored in a separate variable.

     Just to be double clear, there are TWO steps to accessing a future:
     1- resolving it, then updating the asker agent
     2- accessing/querying it by the asker agent, possibly storing the value that resolved it
     The handlefuture method is the one that does #1, while GET/TIMED-GET *possibly* do #2.

     Why to have this handleFuture method at all then? ans: to give the scheduler the "choice" to drop 
     the message before resolving the future, for example to induce a deadlock.
     */
  }

  /**
   * Schedule a timed action that keeps triggering at least once in its life time.
   * @param timedAction the timed action that triggers its action each time its timer times-out
   */
  def schedulePeriodic(timedAction: TimedAction): Unit = {

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new DoSchedulePeriodic(timedAction)
      traceManager.current += te
    }

    /*
     * Why don't I update the timed action's toExecute.smtmts to point to tasksFrom?
     * Because timed actions are not supposed to block, then potentially preempted.
     */
    timedActionsTracker + timedAction

    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }
    //    }
  }

  /**
   * Remove from the taskQ and adding tasks to consumeQ.
   */
  def consume(a: Agent): Unit = {
    require(!taskQ.isEmpty, "Scheduler.consume method - Can't consume from an empty taskQ")

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new Consume(taskQ.head(ds.get(a.name)).traceCopy)
      traceManager.current += te
    }

    consumeQ = consumeQ :+ taskQ.consume(ds.get(a.name))

    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }
  }

  def consumeAll(a: Agent): Int = {
    var cnt = 0
    taskQ(a).map {
      x ⇒
        val act = x.action
        while (act.hasMore) {
          consume(ds.get(a.name))
          cnt += 1
        }
    }
    cnt
  }

  def consumeAll(agentName: String): Int = {
    consumeAll(ds.get(agentName))
  }

  def consumeATask(a: Agent): Int = {

    var howManyTimes = 0
    if (!taskQ(ds.get(a.name)).isEmpty) {
      howManyTimes = taskQ.countStmtsHeadTask(a)
      (1 to howManyTimes) map { _ ⇒ { consume(ds.get(a.name)) } }
    }
    howManyTimes
  }

  def consumeATask(agentName: String): Int = {
    consumeATask(ds.get(agentName))
  }

  /**
   * This method alone replaces the whole of Standard execution thread logic.
   *
   * It is used in the implementation of all other "execution" methods
   */
  private def execWithPossibleBlocking(stmt: Statement, threadId: Long) = {
    val a = ds.get(stmt.a.name)
    val m = stmt.m
    val taskFrom = stmt.taskFrom // this is set in the call to Scheduler.schedule()
    var action: Action = stmt.action

    if (!a.blocked || taskFrom.isTimed) {

      stmt.apply
      stmt match{ // the order of matching matters, this is why While first (since it extends If), the If, then FunctionCalling.
        case x: While if x.conditionEvaluation == true =>
          consumeQ = action.toExecute.take(x.body.size) ++ consumeQ
          action.toExecute = action.toExecute.splitAt(x.body.size)._2 // no -1
        case x: If if x.conditionEvaluation == true  =>
          consumeQ = action.toExecute.take(x.body.size) ++ consumeQ // no +1
          action.toExecute = action.toExecute.splitAt(x.body.size)._2 // there is -1
          if(action.hasMore) skipAssociatedWith(x, action.toExecute.head)
        case x: FunctionCalling =>
          consumeQ = x.action.toExecute.take(x.body.size) ++ consumeQ
          action.toExecute = action.toExecute.splitAt(x.body.size - 1)._2
        case _ => ;
      }

      tick // tick with each statement executed
    }
    if (a.blocked && !m.isInstanceOf[ResolveDummyFuture]) {
      taskFrom.suspend
      scheduler.blockingMgr.block(stmt)
      tick
    } else if (m.isInstanceOf[ResolveDummyFuture]) {
      scheduler.blockingMgr.unblock(a) // this will scheduler whatever is unblocked
      a.blockedOn = null
    }
  }

  /**
   * In a control flow If-[elseIf-]Else chain, if one branch executes,
   * the rest should be skipped.
   * @param stmtOrigin the current in the chain statement (can be If or its subclasses)
   * @param stmtNext the next statement in the chain to be skipped
   */
  protected def skipAssociatedWith(stmtOrigin: Statement,stmtNext: Statement): Unit ={
    var skipped = stmtOrigin
    var toSkip = stmtNext
    while(stmtOrigin.action.hasMore &&
      skipped == toSkip.associatedWith &&
      skipped.isInstanceOf[If] &&
      toSkip.isInstanceOf[ElseIf]) { // ElseIf or Else fall in this category
      toSkip.action.advancePC
      skipped = toSkip
      toSkip = toSkip.action.toExecute.head
    }
  }

  /**
   * Submits ONE task to be executed in the thread pool using the
   * StandardExecutionThread.
   *
   * The task is taken from the consumeQ if there exists one.
   */
  def execute(threadID: Long = 1): Unit = {
    require(!consumeQ.isEmpty, "Scheduler.execute(Int) -- can't execute a statement from an EMPTY consumeQ")

    // tracing
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new ExecuteStatement(consumeQ.head.traceCopy, threadID)
      traceManager.current += te
    }

    // actual work
    //    if ( !consumeQ.isEmpty ) {
    val stmt = consumeQ.head
    consumeQ = consumeQ.tail
    execWithPossibleBlocking(stmt, threadID)
    //    }

    // ----------------------------------------------------------------
    // Replaced the StandardExecutionThread by sequential code above 
    // (this makes it easier to follow)
    //    val t = StandardExecutionThread(task, threadID)
    //    t.start
    //    t.join
    // ----------------------------------------------------------------

    // tracing
    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }
  }

  /**
   * calls execute() as many times as there are tasks in consumeQ to execute.
   */
  def executeAll(threadID: Long = 1): Unit = {
    //    this.synchronized {

    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new ExecuteAll(consumeQ.map { x ⇒ x.traceCopy }, threadID)
      traceManager.current += te
    }

    while(!consumeQ.isEmpty)
      execute(threadID)
      
      // this one causes a problem if there are pre-empted tasks (i.e. executing from an empty consumeQ)
//    consumeQ map { _ ⇒ execute(threadID) }

    // I want to execute them sequentially (one after the other, not in parallel).
    //    consumeQ foreach { x => val t = new StandardExecutionThread(x, threadID); t.start; t.join }

    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }

    //    }
  }

  /**
   * Shuts down the thread pool backing this scheduler in an orderly
   * manner. That is after executing all submitted tasks.
   */
  def shutdown = {
    //    this.synchronized {
    val te = new TraceEntry
    if (isTracingEnabled) {
      te.prior = ds.traceCopy
      te.event = new ShutdownScheduler
    }

    clk = 0 // abstract clock
    ds = null
    //    taskQ = Seq[SuspendableTask]()
    taskQ = TaskQ(this)
    consumeQ = Seq[Statement]()
    // execute queue is already there inside the actual thread pool.
    tracingEnabled = false
    // futures and which agents are blocked by them (note in this case it isn't useable since ds is null)
    blockingMgr = new BlockedTasksManager
    // timed actions
    timedActionsTracker = new TimedActionsTracker
    // all that remains are traces in the traceMgr.

    //      tp.shutdown

    if (isTracingEnabled) {
      te.posterior = ds.traceCopy
      traceManager.current += TraceEntryEnd(te)
    }

    //    }
  }

  def isShudown: Boolean = {
    //    tp.isShutdown() &&
    clock() == 0 &&
      null == ds &&
      taskQ.isEmpty &&
      consumeQ.isEmpty &&
      tracingEnabled == false &&
      blockingMgr.blockedTasks.isEmpty &&
      timedActionsTracker.timedActions.isEmpty
  }

  /**
   * Automatically explores all schedules for a distributed
   * system associated with the used scheduler. As this is a
   * default and is only manual implementation, the method is
   * to be overridden by schedulers extending this one.
   */
  def explore: Unit

  override def toString: String = {

    def toString(cq: Seq[Statement]): String = {
      cq.map(x => x.a.name + ":" + x.getClass.getSimpleName).mkString(",")
    }

    "TaskQ = " + taskQ.toString + "\n" +
      "ConsumeQ = " + toString(consumeQ) + "\n" +
      "Blocked Tasks = " + blockingMgr.blockedTasks.toString + "\n" +
      "Timed Actions = " + timedActionsTracker.toString
  }
  //=======================================
  // Tracing
  //=======================================
  def traceCopy: Scheduler = {

    // this line adds the fromJson method to this scheduler
    import edu.utah.cs.gauss.ds2.core.schedulers.DeSerializers.FromJsonScheduler

    // and this pne uses it!
    this.fromJson(toJson)
  }

  def toJson: JValue = {
    ("Scheduler" ->
      ("ds" -> ds.toJson) ~
      ("numThreads" -> numThreads) ~
      //      ("scheduingAlgorithm" -> schedulingAlgorithm.getClass.getSimpleName) ~
      ("clock" -> clk) ~
      ("taskQ" -> taskQ.toJson) ~
      ("consumeQ" -> consumeQ.map { x ⇒ x.toJson }) ~
      ("blockingMgr" -> blockingMgr.toJson) ~
      ("timedActionsTracker" -> timedActionsTracker.toJson) ~
      ("traceManager" -> traceManager.toJson))
  }

  // this maybe not needed as I an check for isInstanceOf[MixinTraitName], then act accordingly.

  // //----------------------------------------
  // // The mixin support: a collection of all methods
  // //----------------------------------------
  // // note nothing does anything significant, till a mixin overrides
  // // them, then they do meaningful things.
  // def partition: DistributedSystem = ds
  // def unpartition: DistributedSystem = ds
  

  // Notes to developer
  /*
   * NOTES for developer:
   * 1- Figure how the scheduler will know that this scheduled task/stmt has to wait for a future to be resolved
   * 2- make use of the blockedAgentsManager to simplify the logic
   * 3- when a blocked agent is encountered:
   *  - pre-empt this agent
   *  - should all agents finished their processing except blocked agents, report a deadlock.
   *  - Should the blocked agents have no more agents, guarantee no-deadlocks
   *  - Should re-ordering of send operations cause different order of any agent's state mutation, report a data race.
   *  - other properties need more elaborate analyses, for later.
   * 
   */
  override def snapshot: SchedulerState = SchedulerState(this)

  override def restore(state: SchedulerState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}