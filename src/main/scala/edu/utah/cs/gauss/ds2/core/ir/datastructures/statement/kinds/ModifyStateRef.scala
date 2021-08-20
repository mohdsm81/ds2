package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
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
@SerialVersionUID(1124)
class ModifyStateRef extends ModifyState with
  ModifyingStateRef with JsonSerializable with
  TraceCopying[ModifyStateRef] with
  Printable {

  override def toJson: JValue = {
    ("ModifyStateRef" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: ModifyStateRef = {
    ModifyStateRef.fromJson(toJson)
  }

  override def generateStatic: ModifyStateRef = {
    // super.generateStatic // modify state calls different method on localstate object
    code = (m: Message, a: Agent) => { localStateForWrite.setRef(variable, value.toString)}
    this
  }

  override def generateDynamic: ModifyStateRef = {
    // super.generateDynamic // this is a source of a problem, generate dynamic should do mod-state-ref specific logic
    variable = variableVar
    LocalState.validateVariable(variable)
    value = valueVar
    LocalState.validateVariable(value.toString)
    generateStatic
  }

  override def toString: String = {
    val valStr = if (null != value) value.toString else "nothing"
    s"MODIFY_STATE_REF --> ${variable} to be assigned ${valStr}"
  }
}

object ModifyStateRef extends JsonDeSerializable[ModifyStateRef] {
  def apply(variable1: String, variable2: String): ModifyStateRef = {
    LocalState.validateVariable(variable1)
    LocalState.validateVariable(variable2)

    val newOne = new ModifyStateRef
    newOne.variable = variable1
    newOne.value = variable2 // yes, this is completely dynamic anyways
    newOne.isDynamic = true
    newOne
  }

  def apply(variableVar: String, valueVar: String, agentStateHoldsVariable: Boolean, agentStateHoldsValueVar: Boolean): ModifyStateRef = {
    val newOne = ModifyStateRef(variableVar, valueVar)
    newOne.variableVar = variableVar
    newOne.valueVar = valueVar
    newOne.agentStateRead1 = agentStateHoldsVariable
    newOne.agentStateWrite = agentStateHoldsValueVar
    newOne
  }

  def fromJson(js: JValue): ModifyStateRef = {
    val newOne = new ModifyStateRef
    ModifyState.fromJson(js \ "ModifyStateRef" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
