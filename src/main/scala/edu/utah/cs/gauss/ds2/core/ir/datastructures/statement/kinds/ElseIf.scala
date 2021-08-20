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
@SerialVersionUID(1132)
class ElseIf extends If with
  Conditional with JsonSerializable with
  TraceCopying[ElseIf] with
  Printable {

  override def toJson: JValue = {
    ("ElseIf" ->
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: ElseIf = {
    super.generateDynamic
    generateStatic
  }

  override def generateStatic: ElseIf = {
    super.generateStatic
    this
  }

  override def traceCopy: ElseIf = {
    ElseIf.fromJson(toJson)
  }

  override def toString: String = {
    s"ELSE_IF --> ${conditionEvaluation}"
  }
}

object ElseIf extends JsonDeSerializable[ElseIf] {
  def apply(cond: (Message, Agent) => Boolean)(stmts: Statement*)(associatedWith: Statement): ElseIf = {
    // sanity check
    require(null != cond && null!= stmts && null != associatedWith, "ElseIf.apply -- can't have any argument as null")
    require(associatedWith.isInstanceOf[If] || associatedWith.isInstanceOf[ElseIf], "ElseIf.apply -- can't associate an ElseIf with other than an If")


    val newOne = new ElseIf
    If.apply(cond)(stmts:_*).assignAttributesTo(newOne)
    newOne.associatedWith = associatedWith
    newOne
  }

  def apply(condVar: String)(stmts: Statement*)(associatedWith: Statement): ElseIf = {
    LocalState.validateVariable(condVar)
    // sanity check
    require(null != condVar && null!= stmts && null != associatedWith, "ElseIf.apply -- can't have any argument as null")
    require(associatedWith.isInstanceOf[If] || associatedWith.isInstanceOf[ElseIf] , "ElseIf.apply -- can't associate an ElseIf with other than an If")

    val newOne = new ElseIf
    If.apply(condVar)(stmts:_*).assignAttributesTo(newOne)
    newOne.associatedWith = associatedWith
    newOne
  }

  def apply(condVar: String, agentStateHoldsCondVar: Boolean)(stmts: Statement*)(associatedWith: Statement): ElseIf = {
    val newOne = ElseIf(condVar)(stmts:_*)(associatedWith)
    newOne.agentStateRead1 = agentStateHoldsCondVar
    newOne
  }

  def fromJson(js: JValue): ElseIf = {
    val newOne = new ElseIf
    If.fromJson(js \ "ElseIf" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
