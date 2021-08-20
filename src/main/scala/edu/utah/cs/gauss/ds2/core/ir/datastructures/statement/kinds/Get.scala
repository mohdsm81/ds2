package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.features.Printable
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
@SerialVersionUID(1129)
class Get extends Statement with
  Blocking with JsonSerializable with
  TraceCopying[Get] with
  Printable {

  override def hashCode: Int = {
    super.hashCode // +
      // future.hashCode +
      // futureVar.hashCode +
      // dstVariableName.hashCode
  }

  override def toJson: JValue = {
    val futureJVal = if (null != future) future.toJson else null

    ("Get" ->
      ("future" -> futureJVal) ~ // may/not lead to infinite loop, keep an eye
      ("futureVar" -> futureVar) ~
      ("dstVariableName" -> dstVariableName) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Get = {
    future = localStateForRead(futureVar).asInstanceOf[DummyFuture]
    generateStatic
  }

  override def generateStatic: Get = {
    val dstVar = if (dstVariableName.isEmpty) None else Some(dstVariableName)
    code = (m: Message, a: Agent) => { ds.get(future, dstVar, action) }
    this
  }

  override def apply = {
    super.apply

    if (!isDynamic && null != future && future.resolved)
      localStateForWrite(dstVariableName) = future.value

    if (isDynamic) {
      future = localStateForRead(futureVar).asInstanceOf[DummyFuture]
      if (null != future && future.resolved)
        localStateForWrite(dstVariableName) = future.value
    }
  }


  override def traceCopy: Get = {
    Get.fromJson(toJson)
  }

  override def toString: String = {
    val agentNme = a match{
      case null => null
      case x:Agent => x.name
    }
    val futureID = future match {
      case null => -1
      case x:DummyFuture => future.id

    }

    s"GET --> by ${agentNme} for future id ${futureID}"
  }

  def assignAttributesTo(dstStmt: Get): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.future = future
    dstStmt.futureVar = futureVar
  }

}

object Get extends JsonDeSerializable[Get] {

  def apply(future: DummyFuture, dstVariableName: String): Get = {
    val newOne = new Get
    newOne.future = future
    newOne.dstVariableName = dstVariableName

    newOne
  }

  def apply(futureVar: String, dstVariableName: String): Get = {
    LocalState.validateVariable(futureVar)
    LocalState.validateVariable(dstVariableName)

    val newOne = new Get
    newOne.isDynamic = true
    newOne.futureVar = futureVar
    newOne.dstVariableName = dstVariableName

    newOne
  }

  def apply(futureVar: String, dstVariableName: String, agentStateHoldsFutureVar: Boolean, agentStateToStoreValue: Boolean): Get = {
    val newOne = Get(futureVar, dstVariableName)
    newOne.agentStateRead1 = agentStateHoldsFutureVar
    newOne.agentStateWrite = agentStateToStoreValue
    newOne
  }

  def fromJson(js: JValue): Get = {
    val newOne = new Get

    Statement.fromJson(js \ "Get" \ "Statement").assignAttributesTo(newOne)

    js \ "Get" \ "future" match {
      case null       => newOne.future = null
      case x: JObject => newOne.future = DummyFuture.fromJson(js \ "Get" \ "future")
      case _          => throw new Error("Get.fromJson -- can't extract future")
    }

    js \ "Get" \ "futureVar" match {
      case JString(x) => newOne.futureVar = x
      case _          => throw new Error("Get.fromJson -- can't extract futureVar")
    }

    js \ "Get" \ "dstVariableName" match {
      case JString(x) => newOne.dstVariableName = x
      case _          => throw new Error("Get.fromJson -- can't extract dstVariableName")
    }

    newOne
  }
}
