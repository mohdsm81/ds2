package edu.utah.cs.gauss.ds2.core.ir.datastructures
import edu.utah.cs.gauss.ds2.core.schedulers.BlockedTasksManager
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import java.util.UUID

import edu.utah.cs.gauss.serialization.IO.{fromBytes, toBytes}
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.state.{DummyFutureState, Snapshot}

/**
 * This DummyFuture implementation is to be managed by the
 * BlockedAgentsManager, not directly by the scheduler.
 */

@SerialVersionUID(600)
case class DummyFuture(
                        var resolved: Boolean = false,
                        var submittedTime: BigInt,
                        var promisedBy: Agent,
                        var waitingFor: Agent) extends Serializable with Snapshot[DummyFutureState] {

  var value: Any = _
  var id = UUID.randomUUID

//  var promisedByName = if (null != promisedBy) promisedBy.name else ""
//  var waitingForName = if (null != waitingFor) waitingFor.name else ""

  var storedInVariable: String = ""

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    val valueHash = if (null == value) 0 else value.hashCode
    val promisedByyName = if (null == promisedBy) "" else promisedBy.name
    val waitingForrName = if (null == waitingFor) "" else waitingFor.name

    resolved.hashCode +
      submittedTime.hashCode +
      promisedByyName.hashCode +
      waitingForrName.hashCode +
      valueHash +
      id.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }
  //==============================

  def get: Option[Any] =
    {
      this.synchronized {
        if (resolved)
          Some(value)
        else
          None
      }
    }

  def resolve(value: Any): Unit = {

    this.value = value

    resolved = true
    promisedBy.futuresPromised -= id
    waitingFor.futuresWaitingFor -= id
    //      waitingFor.localState += (id.toString -> value)
    waitingFor.localState(storedInVariable) = value
    waitingFor.blocked = false

  }

  def toJson: JValue = {

    val promisedByyName = promisedBy match{
      case null => null
      case x: Agent => x.name
    }
    val waitingForrName = waitingFor match{
      case null => null
      case x: Agent => x.name
    }

    val thisVal: JValue = if (null == value) null else toBytes(value) map { x => JInt(x) }

    ("DummyFuture" ->
      ("resolved" -> resolved) ~
      ("submittedTime" -> submittedTime) ~
      ("promisedBy" -> promisedByyName) ~
      ("waitingFor" -> waitingForrName) ~
      ("value" -> thisVal) ~
      ("id" -> id.toString) ~
      ("storedInVariable" -> storedInVariable))
  }

  def traceCopy: DummyFuture = DummyFuture.fromJson(toJson)

  def in(futures: Map[UUID, DummyFuture]): Boolean = {
    require(futures != null, "DummyFuture.in() method - doesn't accept null arguments")
    futures.keySet.contains(id)
  }

  def in(blockingMGR: BlockedTasksManager): Boolean = {
    require(null != blockingMGR, "DummyFuture.in(BlockingManager) method - doesn't accept null arguments")
    blockingMGR.blocked.find { x => this is x.action.a.blockedOn.get } != None
  }

  def is(that: DummyFuture): Boolean = {
    id == that.id
  }

  override def toString: String = {
    //    s"resolved = ${resolved},\nsubmitted-time = ${submitedTime},\npromised-by = ${promisedBy},\nwaiting-for= ${waitingFor},\nvalue = ${value},\nid = ${id}"
    s"FUTURE: by = ${promisedBy}, for= ${waitingFor}, id = ${id}"
  }

  override def snapshot: DummyFutureState = DummyFutureState(this)

  override def restore(state: DummyFutureState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}

object DummyFuture {
  def fromJson(js: JValue): DummyFuture = {
    implicit val formats = DefaultFormats
    //    js.extract[DummyFuture]

    val rslvd = js \ "DummyFuture" \ "resolved" match {
      case JBool(x) => x
      case _        => throw new Error("DummyFuture - can't extract 'resolved'")
    }

    val subTime = js \ "DummyFuture" \ "submittedTime" match {
      case JInt(x) => x
      case _       => throw new Error("DummyFuture - can't extract 'submittedTime'")
    }

    val promiser = null


    val waiter = null

    val value = js \ "DummyFuture" \ "value" match {
      case null => null
      case JArray(x) => fromBytes(x map { x =>
        x match {
          case JInt(x) => x.toByte
          case _       => throw new Error("DummyFuture - can't be the case that a non JInt is in the seq!")
        }
      })
      case _ => throw new Error("DummyFuture - can't extract 'value'")
    }

    val id = js \ "DummyFuture" \ "id" match {
      case JString(x) => UUID.fromString(x)
      case _          => throw new Error("DummyFuture - can't extract 'id'")
    }

    val storedInVariable = js \ "DummyFuture" \ "id" match {
      case JString(x) => x
      case _          => throw new Error("DummyFuture - can't extract 'storedInVariable'")
    }

    val f = new DummyFuture(rslvd, subTime, promiser, waiter)
    f.id = id
    f.storedInVariable = storedInVariable

    f
  }
}
