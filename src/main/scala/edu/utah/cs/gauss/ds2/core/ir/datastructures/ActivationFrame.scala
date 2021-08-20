package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.{JsonDeSerializable, JsonSerializable, TraceCopying}
import edu.utah.cs.gauss.ds2.core.state.{ActivationFrameState, Snapshot}

import scala.collection.mutable.Stack
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
@SerialVersionUID(110)
class ActivationFrame extends JsonSerializable with TraceCopying[ActivationFrame] with Snapshot[ActivationFrameState] {

  // this stack always resides inside the task/process, in our case
  // the Agent. This is merely a pointer to that stack.
  var stack: Stack[ActivationFrame] = Stack()

  var localState = new LocalState
  var parameterList: Either[Seq[Any], Seq[String]] = Left(Seq())

  var parameterAccessPattern: Seq[Boolean] = Seq()
  // var parameterCachedValues: Seq[Any] = Seq()

  // stack-frame  state-variables
  var pushed: Boolean = false
  var popped: Boolean = false

  override def hashCode: Int = {
    localState.hashCode() +
    parameterAccessPattern.hashCode() +
    pushed.hashCode +
    popped.hashCode
  }

  // convenience methods
  def push: Unit = stack.push(this)
  def pop: ActivationFrame = stack.pop

  override def toJson: JValue = {

    var paramListAnnotation: String = ""
    val paramList: JValue = parameterList match {
      case Left(x) =>
        paramListAnnotation = "Left"
        x.map { y => LocalState.serialize(y) }
      case Right(x) => paramListAnnotation = "Right"; x.map { y => y }
    }

    ("ActivationFrame" ->
       ("localState" -> localState.toJson) ~
       ("parameterList" -> paramList) ~
       ("paramListAnnotation" -> paramListAnnotation) ~
       ("parameterAccessPattern" -> parameterAccessPattern) ~
       // ("parameterCachedValues" -> parameterCachedValues.map(LocalState.serialize(_))) ~
       ("pushed" -> pushed) ~
       ("popped" -> popped)
    )
  }

  override def traceCopy: ActivationFrame = {
    ActivationFrame.fromJson(toJson)
  }

  override def snapshot: ActivationFrameState = ActivationFrameState(this)
  override def restore(state: ActivationFrameState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}

object ActivationFrame extends JsonDeSerializable[ActivationFrame] {

  override def fromJson(js: JValue): ActivationFrame = {
    val newOne = new ActivationFrame

    newOne.localState = LocalState.fromJson(js \ "ActivationFrame" \ "localState")

    val paramList = js \ "ActivationFrame" \ "parameterList" match {
      case JArray(x) => x.map(LocalState.deSerialize(_))
      case _ => throw new Error("ActivationFrame.fromJson -- can't extract parameterList")
    }

    js \ "ActivationFrame" \ "paramListAnnotation" match {
      case JString("Right") => newOne.parameterList = Right(paramList.map(_.toString))
      case JString("Left") => newOne.parameterList = Left(paramList)
      case _ => throw new Error("ActivationFrame.fromJson -- can't extract paramListAnnotation")
    }

    js \ "ActivationFrame" \ "parameterAccessPattern" match {
      case JArray(x) => x.map{
        case JBool(z) => newOne.parameterAccessPattern = newOne.parameterAccessPattern :+ z
        case _ => "ActivationFrame.fromJson -- can't extract a parameterAccessPattern value"
      }
      case _ => throw new Error("ActivationFrame.fromJson -- can't extract parameterAccessPattern")
    }


    js \ "ActivationFrame" \ "pushed" match {
      case JBool(x) => newOne.pushed = x
      case _ => throw new Error("ActivationFrame.fromJson -- can't extract pushed")
    }

    js \ "ActivationFrame" \ "popped" match {
      case JBool(x) => newOne.popped = x
      case _ => throw new Error("ActivationFrame.fromJson -- can't extract popped")
    }

    newOne
  }
}
