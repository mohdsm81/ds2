package edu.utah.cs.gauss.ds2.core.schedulers

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.state.{Snapshot, TaskQState}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.collection.mutable.{Map => MMap}

object TaskQ {
  /**
   * Returns a new instance of the SchedulerMultiTaskQ associated with
   * the scheduler <code>sch</code>
   * @param sch the scheduler that is associated with this multi task queue
   */
  def apply(sch: Scheduler): TaskQ = {
    return new TaskQ(sch)
  }

  def fromJson(js: JValue): TaskQ = {

    val mtq = TaskQ(null)

    js \ "TaskQ" \ "queues" match {
      case JArray(x) ⇒
        x.map {
          case JField(a, JArray(q)) ⇒ mtq.queues += (a -> q.map {
            case z ⇒ SuspendableTask.fromJson(q)
          })
          case _ ⇒ throw new Error("SchedulerMultiTaskQ.fromJson - couldn't parse a pair of (Agent -> Seq[SuspendableTasks]) object")
        }
      case _ ⇒ throw new Error("SchedulerMultiTaskQ.fromJson - couldn't parse the SchedulerMultiTaskQ object")
    }

    mtq
  }
}

/**
 * This class implements a task queue per agent. It facilitates many
 * things including modeling task-level interleaving.
 */
class TaskQ(sch: Scheduler) extends Snapshot[TaskQState]{

  var queues = MMap[String, Seq[SuspendableTask]]()

  override def hashCode: Int = {
    queues.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }

  def update(a: Agent, q: Seq[SuspendableTask]): Unit = {
    queues(a.name) = q
  }

  def update(agentName: String, q: Seq[SuspendableTask]): Unit = {
    queues(agentName) = q
  }

  def apply(a: Agent): Seq[SuspendableTask] = {
    queues(a.name)
  }

  def apply(agentName: String): Seq[SuspendableTask] = {
    queues(agentName)
  }

  def +=(a: Agent, v: SuspendableTask): Unit = {
    if (queues.contains(a.name)) // append to its queue
      this(a) = this(a) :+ v
    else // update the map with a new agent
      this(a) = Seq(v)
  }

  def +=(agentName: String, v: SuspendableTask): Unit = {
    if (queues.contains(agentName)) // append to its queue
      this(agentName) = this(agentName) :+ v
    else // update the map with a new agent
      this(agentName) = Seq(v)
  }

  def size: Int = {
    queues.values.map { _.size }.sum
  }
  def countTasks(a: Agent): Int = {
    this(a).size
  }
  def countTasks(agentName: String): Int = {
    countTasks(sch.ds.get(agentName))
  }

  def countStmts(a: Agent): Int = {
    var cnt = 0
    this(a).map { x ⇒ cnt = cnt + x.action.toExecute.size }
    cnt
  }
  def countStmts(agentName: String): Int = {
    countStmts(sch.ds.get(agentName))
  }
  def countStmtsHeadTask(a: Agent): Int = {
    this(a).head.action.toExecute.size
  }
  def countStmtsHeadTask(agentName: String): Int = {
    countStmts(sch.ds.get(agentName))
  }

  def traceCopy: TaskQ = {
    TaskQ.fromJson(toJson)
  }

  def map(a: Agent)(op: (SuspendableTask) ⇒ SuspendableTask): Seq[SuspendableTask] = {
    queues(a.name) map { op(_) }
  }

  def toJson: JValue = {
    (getClass.getSimpleName ->
      ("queues" -> queues.map { case (k, v) ⇒ (k, v.map(_.toJson)) }))
  }

  def :+(task: SuspendableTask): TaskQ = {
    +=(task.action.a, task)
    this
  }

  def isEmpty: Boolean = {
    queues.isEmpty || queues.forall(_._2.isEmpty)
  }

  def isEmpty(a: Agent): Boolean = {
    this(a).isEmpty
  }

  def isEmpty(agentName: String): Boolean = {
    isEmpty(sch.ds.get(agentName))
  }

  def head(a: Agent): SuspendableTask = {
    queues(a.name).head
  }

  def tail(a: Agent): Seq[SuspendableTask] = {
    queues(a.name).tail
  }

  def consume(a: Agent): Statement = {
    require(!this(a).isEmpty, "SchedulerMultiTaskQ.consume(Agent) -- '" + a.name + "' queue is empty")

    val stmt = head(a).action.toExecute.head
    head(a).action.advancePC

    // removes the task if completely consumed
    if (!this(a).isEmpty &&
      !head(a).action.hasMore)
      this(a) = tail(a)
    stmt
  }

  private def unConsume(stmt: Statement): Unit = {
    // a per-statement unConsume(stmt)
    // it simplifies the operation of the more complex unConsume(stmts)

    val a = stmt.a
    val task = stmt.taskFrom

    // we only care about the head of the taskQ, we don't care about other task since naturally they didn't start being consumed
    this(a) match {
      case x :: tail if (x.id == task.id) ⇒
        // task exists, put the statement back to the front of its to-execute queue, and remove it from executed-queue
        x.action.toExecute = stmt +: x.action.toExecute
        x.action.executed = x.action.executed.dropRight(1) // drop the last stmt (same as the one added to the toExecute queue
      case _ ⇒
        // task isn't anymore in the taskQ, add it back, then do the same as above
        this(a) = task +: this(a)
        // Now that task exists, put the statement back to the front of its to-execute queue, and remove it from executed-queue
        head(a).action.toExecute = stmt +: task.action.toExecute
        head(a).action.executed = task.action.executed.dropRight(1) // drop the last stmt (same as the one added to the toExecute queue
    }
  }

  def unConsume(stmts: Seq[Statement]): Unit = {
    //    val cnt = stmts.size
    //    if ( cnt > 0 )
    stmts.reverse map (unConsume(_))

    /*
     * We reverse because we unConsume at the opposite order these statements were consumed.
     * It is like going back in-reverse from work to home :), take opposite of each turn at
     * reverse order of distances (before, instead of after) each turn. Everything should be
     * reversed, and hopefully you will reach home ;).
     */
  }

  /**
   * To enable ScalaTest to testing for emptiness of the taskQ
   * @param thing
   * @return
   */
  def isEmpty(thing: TaskQ): Boolean = {
    thing.isEmpty
  }

  override def toString: String = {
    queues.keys map { k ⇒
      k + "---> " + queues(k).map { x: SuspendableTask ⇒ x.action.toExecute.map(_.getClass.getSimpleName).mkString(",") }.mkString(";")
    } mkString ("\n")
  }

  override def snapshot: TaskQState = TaskQState(this)
  override def restore(state: TaskQState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}
