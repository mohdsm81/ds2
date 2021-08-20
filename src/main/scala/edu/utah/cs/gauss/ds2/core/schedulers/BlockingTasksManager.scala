package edu.utah.cs.gauss.ds2.core.schedulers

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.Get

import scala.collection.GenTraversableOnce
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.TimedGet
import edu.utah.cs.gauss.ds2.core.state.{BlockedTasksManagerState, Snapshot}

/**
 * @author <br>
 * Mohammed S. Al-Mahfoudh <br/>
 * mahfoudh@cs.utah.edu <br/>
 * SoC - Gauss Group <br/>
 *
 * As the name implies, it manages agents blocking and unblocking on future objects.
 *
 */

@SerialVersionUID(1100)
class BlockedTasksManager(implicit var ds: DistributedSystem, implicit var scheduler: Scheduler) extends Serializable with Snapshot[BlockedTasksManagerState]{

  var blockedTasks = Set[SuspendableTask]()

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    blockedTasks.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }
  //==============================

  def getSuspendedTask(a: Agent): Option[SuspendableTask] = {
    blockedTasks.find { _.action.a.name == a.name }
  }

  def blocked: Set[SuspendableTask] = {
    blockedTasks.filter { _.isSuspended }
  }

  def removeUnblocked: Unit = {
    blockedTasks = blockedTasks.filter { _.isSuspended }
  }

  def unblocked: Set[SuspendableTask] = {

    blockedTasks.filter { !_.isSuspended }
  }

  def unblock(a: Agent): Unit =
    {
      require(null != a && ds.agents.contains(a), "Unblock method - either agent is null or is not part of the distributed system")
      //      require(blocked.contains(a), "Unblock method - can't unblock an agent that is NOT blocked")

      var task = blockedTasks.find { x ⇒ x.action.a.name == a.name }
      if (task != None)
        //        && !a.blocked)
        blockedTasks map { x ⇒
          if (x.action.a.name == a.name) { x.resume }

          /*
         * This shouldn't happen, because:
         * 1- the blocked/preempted tasks are already at the front of the taskQ
         * 2- scheduling is an explicit act of a scheduler, not blocking manager. So it should be done by a scheduler
         */
          // unblocked map { x ⇒ scheduler.schedule( x ) }
          removeUnblocked
        }
    }

  def setResolved(df: DummyFuture): Unit = {
    require(null != df, "DistributedSystem.resolve(future) method - Can't resolve a null future")
    val thisFutureBlockedTasks = blockedTasks.filter { x ⇒ x.action.a == df.waitingFor }
    val futures = thisFutureBlockedTasks.filter { x ⇒ x.action.a.blockedOn == df }.map { x ⇒ x.action.a.blockedOn }
    futures.map { x ⇒ x.get.resolve(df.promisedBy, df.value) }
  }

  def unblock(agents: Set[Agent]): Unit = {

    require(null != agents)
    agents.map { x ⇒ unblock(x) }
  }

  /**
   * just a proxy method for testing. Don't use this one
   */
  @Deprecated
  def block(task: SuspendableTask): Unit = {
    block(task.action.executed.last)
  }

  def block(stmt: Statement): Unit = {
    require(ds.agents.contains(stmt.taskFrom.action.a), "Block method - can't block a task whose agent is NOT part of the distributed system")

    // why is this mumble jumble?!
    //    val ag = ds.agents.find { x ⇒ task.action.a.name == x.name }
    //    val agentToBlock = ag.get
    //    if ( ag != None ) {
    //      // blocking the agent should be done in the DS.get method
    //      blockedTasks += task
    //    }

    // just block it!
    blockedTasks += stmt.taskFrom
    // then pre-empt
    preEmpt(stmt)
  }

  /**
   * Preempting a task, leads to preempting all statements
   * coming from the same agent
   * from which that task came from
   * @param stmt the blocking statement that invoked the
   * preemption procedure.
   */
  private def preEmpt(stmt: Statement): Unit = {
    // this function is the filteration condition
    def cond(st: Statement) = st.a == stmt.a // yes, all statments from that agent are preEmpted
    var stmtsPreEmpted: Seq[Statement] = scheduler.consumeQ.filter { cond(_) } // take preempted ones
    scheduler.consumeQ = scheduler.consumeQ.filterNot(cond(_)) // keep only non-preempted ones 
    /*
     * - the stmt itself that caused the blocking should be "prepended" to the
     * front of the toExectute queue in the task it came from. This is
     * especially correct for the blocking Get statement kind, not
     * others (yes, not even TimedGet, since it schedules a timed
     * action to retry and/or skip)
     */
    var (part1, part2) = stmtsPreEmpted.splitAt(stmtsPreEmpted.indexWhere(_.taskFrom.id == stmt.taskFrom.id))

    /*
     * Not sure why the match statement doesn't work like stmt.instanceOf[...].
     * */

    stmt match {
      case x: TimedGet => // this should be TimedGet, the retry and will be in its timed-action
        scheduler.taskQ.unConsume(part1 ++ Seq(stmt) ++ part2) // unconsume preempted+blocking one

      case x: Get => // this is Blocking Get, it needs to execute again whenever it is unblocked
        stmt.taskFrom.action.executed = stmt.taskFrom.action.executed.dropRight(1) // dropping the GET statement since it blocked, though we did action.advancePC
        scheduler.taskQ.unConsume(part1 ++ Seq(stmt) ++ part2) // unconsume preempted+blocking one
      // case _ => // Some of the test-suites use the old way of creating Statements where we can't tell if it is a Get/TimedGet
      //   processItTheOldWay(stmt)
    }

    // based on this ====> I may have to re-write all the test-suites to use the NEW statement-kinds to be able to pattern match ... 

    // def processItTheOldWay(st: Statement) = {
    //   scheduler.taskQ.unConsume(part1 ++ part2) // unconsume preempted+blocking one
    // }

    /*
  * To understand more about this issue, please refer to  Issue #44     
  */
  }

  def traceCopy: BlockedTasksManager = {
    BlockedTasksManager.fromJson(toJson)
  }

  def toJson: JValue = {
    (getClass.getSimpleName ->
      ("blockedTasks" -> blockedTasks.map { x ⇒ x.toJson }))

    // we don't care about DS nor Scheduler in this stage as they are the "containing" objects.
    // this is to avoid dependency cycles too.
  }

  override def toString: String = {
    blockedTasks.mkString(",")
  }

  override def snapshot: BlockedTasksManagerState = BlockedTasksManagerState(this)

  override def restore(state: BlockedTasksManagerState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}

object BlockedTasksManager {
  def fromJson(js: JValue): BlockedTasksManager = {
    implicit val formats = DefaultFormats
    //    js.extract[BlockedTasksManager]

    // note that the DS and scheduler will be added by Scheduler.fromJson.
    val btm = new BlockedTasksManager()(null, null)

    val blockedTasks = js \\ "blockedTasks" match {
      case JArray(x) ⇒ x map { z ⇒ SuspendableTask.fromJson(z) }
      case _ ⇒ throw new Error("BlockedTasksManager.fromJson - can't extract 'blockedTasks'")
    }

    btm.blockedTasks = blockedTasks.toSet

    btm
  }
}
