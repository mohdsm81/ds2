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
@SerialVersionUID(1125)
class ModifyState extends Statement with
  ModifyingState with JsonSerializable with
  TraceCopying[ModifyState] with
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
    val valueStr = if (null != value) value.toString else "nothing"

    ("ModifyState" ->
      ("variable" -> variable) ~
        ("variableVar" -> variableVar) ~
        ("value" -> valueStr) ~
        ("valueVar" -> valueVar) ~
        ("Statement" -> super.toJson))
  }

  override def generateDynamic: ModifyState = {
    variable = localStateForRead(variableVar)
    LocalState.validateVariable(variable)
    value = localStateForWrite(valueVar)
    generateStatic
  }

  override def generateStatic: ModifyState = {
    code = (m: Message, a: Agent) => {
      if (!isFunctional)
        localStateForWrite(variable) = value
      else
        localStateForWrite(variable) = valueFunc(m, a)
    }
    this
  }

  override def traceCopy: ModifyState = {
    ModifyState.fromJson(toJson)
  }

  override def toString: String = {
    val valStr =
      if (null != value) value.toString
      else if (isFunctional) "DON'T MESS with the state!" //valueFunc(m,a) // this one just in case it messes up with the state
      else "nothing"

    s"MODIFY_STATE --> ${variable} to be assigned ${valStr}"
  }

  private def assignOnlyTo(dstStmt: ModifyingState): Unit = {
    dstStmt.variable = variable
    dstStmt.variableVar = variableVar
    dstStmt.value = value
    dstStmt.valueVar = valueVar
    dstStmt.valueFunc = valueFunc
  }

  def assignAttributesTo(dstStmt: ModifyState): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
    dstStmt.isDynamic = isDynamic
  }
}

object ModifyState extends JsonDeSerializable[ModifyState] {

  def apply(variable: String, valueFunc: (Message, Agent) => Any): ModifyState = {
    LocalState.validateVariable(variable)
    val newOne = new ModifyState
    newOne.variable = variable
    newOne.isFunctional = true
    newOne.valueFunc = valueFunc
    newOne
  }

  def apply(variable: String, value: Any): ModifyState = {
    LocalState.validateVariable(variable)
    val newOne = new ModifyState
    newOne.variable = variable
    newOne.value = value
    newOne
  }

  def apply(variableVar: String, valueVar: String): ModifyState = {
    LocalState.validateVariable(variableVar)
    LocalState.validateVariable(valueVar)

    val newOne = new ModifyState
    newOne.variableVar = variableVar
    newOne.valueVar = valueVar
    newOne.isDynamic = true
    newOne
  }


  def apply(variableVar: String, valueVar: String, agentStateHoldsVariable: Boolean, agentStateHoldsValue: Boolean): ModifyState = {
    val newOne = ModifyState(variableVar, valueVar)
    newOne.agentStateRead1 = agentStateHoldsVariable
    newOne.agentStateWrite = agentStateHoldsValue
    newOne
  }

  def fromJson(js: JValue): ModifyState = {
    val newOne = new ModifyState
    Statement.fromJson(js \ "ModifyState" \ "Statement").assignAttributesTo(newOne)

    js \ "ModifyState" \ "variable" match {
      case JString(x) => newOne.variable = x
      case _ => throw new Error("ModifyState.fromJson -- can't extract variable")
    }

    js \ "ModifyState" \ "variableVar" match {
      case JString(x) => newOne.variableVar = x
      case _ => throw new Error("ModifyState.fromJson -- can't extract variableVar")
    }

    js \ "ModifyState" \ "value" match {
      case JString(x) => newOne.value = x
      case _ => throw new Error("ModifyState.fromJson -- can't extract value")
    }

    js \ "ModifyState" \ "valueVar" match {
      case JString(x) => newOne.valueVar = x
      case _ => throw new Error("ModifyState.fromJson -- can't extract valueVar")
    }

    newOne
  }
}
