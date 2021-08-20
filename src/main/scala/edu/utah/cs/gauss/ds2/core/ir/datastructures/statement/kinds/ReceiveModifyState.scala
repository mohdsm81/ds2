package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.features.Printable
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/**
  * @author <br>
  *         Mohammed S. Al-Mahfoudh <br/>
  *         mahfoudh@cs.utah.edu <br/>
  *         SoC - Gauss Group <br/>
  */
@SerialVersionUID(1123)
class ReceiveModifyState extends Statement with
  ReceiveModifyingState with JsonSerializable with
  TraceCopying[ReceiveModifyState] with
  Printable {

  override def hashCode: Int = {
    // val valueHash = if (null != value) value.hashCode else 0

    super.hashCode // +
    // variable.hashCode +
    // variableVar.hashCode +
    // valueHash +
    // valueVar.hashCode

  }

  override def toJson: JValue = {
    val valueStr = if (null != value(m, a)) value(m, a).toString else "nothing"

    ("ReceiveModifyState" ->
      ("variable" -> variable) ~
        ("variableVar" -> variableVar) ~
        ("value" -> valueStr) ~
        ("valueVar" -> valueVar) ~
        ("Statement" -> super.toJson))
  }

  override def generateDynamic: ReceiveModifyState = {
    LocalState.validateVariable(variableVar)
    LocalState.validateVariable(valueVar)

    variable = localStateForRead(variableVar).asInstanceOf[String]
    value = localStateForWrite(valueVar).asInstanceOf[Function2[Message, Agent, Any]]
    generateStatic
  }

  override def generateStatic: ReceiveModifyState = {
    code = (m: Message, a: Agent) => {
      localStateForWrite(variable) = value(m, a)
    }
    this
  }

  override def traceCopy: ReceiveModifyState = {
    ReceiveModifyState.fromJson(toJson)
  }

  override def toString: String = {
//    s"RECEIVE_MODIFY_STATE --> ${variable} to be assigned ${value(m, a)}"
    s"RECEIVE_MODIFY_STATE --> ${variable} to be assigned something"
  }

  private def assignOnlyTo(dstStmt: ReceiveModifyingState): Unit = {
    dstStmt.variable = variable
    dstStmt.variableVar = variableVar
    dstStmt.value = value
    dstStmt.valueVar = valueVar
  }

  def assignAttributesTo(dstStmt: ReceiveModifyState): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
    dstStmt.isDynamic = isDynamic
  }
}

object ReceiveModifyState extends JsonDeSerializable[ReceiveModifyState] {

  def apply(variable: String, value: (Message, Agent) => Any): ReceiveModifyState = {
    LocalState.validateVariable(variable)
    val newOne = new ReceiveModifyState
    newOne.variable = variable
    newOne.isFunctional = true
    newOne.value = value
    newOne
  }

  def apply(variableVar: String, valueVar: String): ReceiveModifyState = {
    LocalState.validateVariable(variableVar)
    LocalState.validateVariable(valueVar)

    val newOne = new ReceiveModifyState
    newOne.variableVar = variableVar
    newOne.valueVar = valueVar
    newOne.isDynamic = true
    newOne
  }


  def apply(variableVar: String, valueVar: String, agentStateHoldsVariable: Boolean, agentStateHoldsValue: Boolean): ReceiveModifyState = {
    val newOne = ReceiveModifyState(variableVar, valueVar)
    newOne.agentStateRead1 = agentStateHoldsVariable
    newOne.agentStateWrite = agentStateHoldsValue
    newOne
  }

  def fromJson(js: JValue): ReceiveModifyState = {
    val newOne = new ReceiveModifyState
    Statement.fromJson(js \ "ReceiveModifyState" \ "Statement").assignAttributesTo(newOne)

    js \ "ReceiveModifyState" \ "variable" match {
      case JString(x) => newOne.variable = x
      case _ => throw new Error("ReceiveModifyState.fromJson -- can't extract variable")
    }

    js \ "ReceiveModifyState" \ "variableVar" match {
      case JString(x) => newOne.variableVar = x
      case _ => throw new Error("ReceiveModifyState.fromJson -- can't extract variableVar")
    }

    js \ "ReceiveModifyState" \ "value" match {
      case JString(x) => newOne.value = (m: Message, a: Agent) => x
      case _ => throw new Error("ReceiveModifyState.fromJson -- can't extract value")
    }

    js \ "ReceiveModifyState" \ "valueVar" match {
      case JString(x) => newOne.valueVar = x
      case _ => throw new Error("ReceiveModifyState.fromJson -- can't extract valueVar")
    }

    newOne
  }
}
