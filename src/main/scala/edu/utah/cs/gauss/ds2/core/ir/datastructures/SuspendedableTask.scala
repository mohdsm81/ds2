package edu.utah.cs.gauss.ds2.core.ir.datastructures

import java.util.UUID

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.Searchable
import edu.utah.cs.gauss.ds2.core.schedulers.BlockedTasksManager
import edu.utah.cs.gauss.ds2.core.state.{Snapshot, SuspendableTaskState}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/**
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *         This class, from its name, represents any agent with a task waiting
 *         for a future to be resolved. Waiting is blocking, and blocking means
 *         it has to be suspended till the future it is waiting it resolves, or
 *         it will deadlock for ever.
 *
 */

@SerialVersionUID(1000)
case class SuspendableTask(var action: Action) extends Serializable with Searchable with Snapshot[SuspendableTaskState] {

  override def snapshot: SuspendableTaskState = SuspendableTaskState(this)
  override def restore(state: SuspendableTaskState): Unit = {
    state.instanceToRestore = this
    state.restore
  }

  var suspended = false
  var isTimed = false
  //  var agentName = if (null == a) null else a.name
  //  
  var id: UUID = UUID.randomUUID

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    val aHash = action.a match {
      case null => 0
      case x: Agent => x.hashCode
    }

    aHash +
      //    action.hashCode +
      suspended.hashCode +
      isTimed.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }

  //==============================

  def suspend: Unit = {
    suspended = true
  }

  def isSuspended: Boolean = {
    suspended
  }

  def resume: Unit = {
    suspended = false
    action.a.blocked = false
  }

  def toJson: JValue = {

    val agentName = action.a match {
      case null => null
      case x: Agent => x.name
    }

    ("SuspendableTask" ->
      ("a" -> JNothing) ~
        ("action" -> action.toJson) ~
        ("suspended" -> suspended) ~
        ("isTimed" -> isTimed) ~
        ("agentName" -> agentName) ~
        ("id" -> id.toString))
  }

  def traceCopy: SuspendableTask = {
    SuspendableTask.fromJson(toJson)
  }

  def in(tasks: Set[SuspendableTask]): Boolean = {
    require(tasks != null, "SuspendableTask.in(Set[tasks]) method - doesn't accept null arguments")
    tasks.contains(this)
  }

  def in(tasks: Seq[SuspendableTask]): Boolean = {
    require(tasks != null, "SuspendableTask.in(Seq[tasks]) method - doesn't accept null arguments")
    tasks.contains(this)
  }

  def in(blockedMgr: BlockedTasksManager): Boolean = {
    require(blockedMgr != null, "SuspendableTask.in(BlockedTasksManager) method - doesn't accept null arguments")
    blockedMgr.blockedTasks.contains(this)
  }

  def is(that: SuspendableTask): Boolean = {
    val actCondition = if (null == action) null == that.action else action == that.action

    //    aCondition && 
    actCondition
  }

  override def toString: String = {
    val (agentName, msg) = action match {
      case null => (null, null)
      case act: Action =>
        val agent = act.a match {
          case null => null
          case a: Agent => a.name
        }
        val msg = action.m match {
          case null => null
          case m: Message => m.name
        }
        (agent,msg)
    }

    agentName + ":" + msg + s"[s:${suspended}-t:${isTimed}]"
  }

  /**
   * Searches the stmt that is unliked (provided as arg) and returnes
   * the linked version of it.
   *
   * @param unlinkedStmtToSearch the statement that is not linked,
   *                             which needs to be replaced by the returned statement
   * @return the linked statement that will replace the unlinked one.
   */
  override def search(unlinkedStmtToSearch: Statement): Option[Statement] = action.search(unlinkedStmtToSearch)
}

object SuspendableTask {
  def fromJson(js: JValue): SuspendableTask = {
    implicit val formats = DefaultFormats

    val action: Action = js \ "SuspendableTask" \ "action" match {
      case x: JObject ⇒ Action.fromJson(x)
      case _ ⇒ throw new Error("SuspendableTask.fromJson - can't extract 'action'")
    }

    val suspended = js \ "SuspendableTask" \ "suspended" match {
      case JBool(x) ⇒ x
      case _ ⇒ throw new Error("SuspendableTask.fromJson - can't extract 'suspended'")
    }
    val isTimed = js \ "SuspendableTask" \ "isTimed" match {
      case JBool(x) ⇒ x
      case _ ⇒ throw new Error("SuspendableTask.fromJson - can't extract 'isTimed'")
    }
    val id = js \ "SuspendableTask" \ "id" match {
      case JString(x) ⇒ x
      case _ ⇒ throw new Error("SuspendableTask.fromJson - can't extract 'id'")
    }

    val task = new SuspendableTask(action)
    task.suspended = suspended
    task.isTimed = isTimed
    task.id = UUID.fromString(id)
    task
  }
}
