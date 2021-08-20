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
@SerialVersionUID(1122)
class ReceiveModifyStateRef extends ReceiveModifyState with
  ReceiveModifyingStateRef with JsonSerializable with
  TraceCopying[ReceiveModifyStateRef] with
  Printable {

  override def toJson: JValue = {
    ("ReceiveModifyStateRef" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: ReceiveModifyStateRef = {
    ReceiveModifyStateRef.fromJson(toJson)
  }

  override def generateStatic: ReceiveModifyStateRef = {
    // super.generateStatic // modify state calls different method on localstate object    
    code = (m: Message, a: Agent) => {localStateForWrite.setRef(variable, value(m,a).toString)}
    this
  }

  override def generateDynamic: ReceiveModifyStateRef = {
    variable = variableVar
    LocalState.validateVariable(variable)
    value = (m:Message, a:Agent) => valueVar
//    value = (m:Message, a:Agent) => valueVar
//    LocalState.validateVariable(value(m,a).toString)
    generateStatic
  }

  override def toString: String = {
    val valStr = if (null != value(m,a)) value(m,a).toString else "nothing"
    s"RECEIVE_MODIFY_STATE_REF --> ${variable} to be assigned ${valStr}"
  }
}

object ReceiveModifyStateRef extends JsonDeSerializable[ReceiveModifyStateRef] {
  def apply(variable1: String, variable2: String): ReceiveModifyStateRef = {
    LocalState.validateVariable(variable1)
    LocalState.validateVariable(variable2)

    val newOne = new ReceiveModifyStateRef
    newOne.variableVar = variable1
    newOne.valueVar = variable2 // yes, this is completely dynamic anyways
    newOne.isDynamic = true
    newOne
  }

  def apply(variableVar: String, valueVar: String, agentStateHoldsVariable: Boolean, agentStateHoldsValueVar: Boolean): ReceiveModifyStateRef = {
    val newOne = ReceiveModifyStateRef(variableVar, valueVar)
    newOne.variableVar = variableVar
    newOne.valueVar = valueVar
    newOne.agentStateRead1 = agentStateHoldsVariable
    newOne.agentStateWrite = agentStateHoldsValueVar
    newOne
  }

  def fromJson(js: JValue): ReceiveModifyStateRef = {
    val newOne = new ReceiveModifyStateRef
    ReceiveModifyState.fromJson(js \ "ReceiveModifyStateRef" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
