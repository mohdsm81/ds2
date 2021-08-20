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
@SerialVersionUID(1133)
class Else extends ElseIf
  with Conditional with JsonSerializable
  with TraceCopying[Else] with
  Printable {

  override def toJson: JValue = {
    ("Else" ->
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Else = generateStatic // yup, it doesn't care about any conditions!

  override def generateStatic: Else = {
    code = (m: Message, a: Agent) => { action.toExecute = body ++ action.toExecute }
    this
  }

  override def traceCopy: Else = Else.fromJson(toJson)

  override def toString: String = "ELSE"

}

object Else extends JsonDeSerializable[Else] {

  def apply(stmts: Statement*)(associatedWith: Statement): Else = {
    // sanity check
    require(null != stmts && null != associatedWith, "ElseIf.apply -- can't have any argument as null")
    require(associatedWith.isInstanceOf[If] || associatedWith.isInstanceOf[ElseIf] ,
      "ElseIf.apply -- can't associate an ElseIf with other than an If or ElseIf")

    val newOne = new Else
    ElseIf((_, _) => false)(stmts:_*)(associatedWith).assignAttributesTo(newOne)
    newOne.associatedWith = associatedWith
    newOne
  }

  def apply(condVar: String)(stmts: Statement*)(associatedWith: Statement): Else = {
    LocalState.validateVariable(condVar)
    // sanity check
    require(null != condVar && null != stmts && null != associatedWith, "ElseIf.apply -- can't have any argument as null")
    require(associatedWith.isInstanceOf[If] || associatedWith.isInstanceOf[ElseIf], "ElseIf.apply -- can't associate an ElseIf with other than an If or ElseIf")

    val newOne = new Else
    ElseIf(condVar)(stmts:_*)(associatedWith).assignAttributesTo(newOne)
    newOne
  }

  def apply(condVar: String, agentStateHoldsCondVar: Boolean)(stmts: Statement*)(associatedWith: Statement): Else = {
    val newOne = Else(condVar)(stmts:_*)(associatedWith)
    newOne.agentStateRead1 = agentStateHoldsCondVar
    newOne
  }

  def fromJson(js: JValue): Else = {
    val newOne = new Else
    ElseIf.fromJson(js \ "Else" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
