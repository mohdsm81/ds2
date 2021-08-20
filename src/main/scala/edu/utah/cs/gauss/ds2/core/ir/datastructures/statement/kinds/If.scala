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
@SerialVersionUID(1128)
class If extends Statement with
  Conditional with JsonSerializable with
  TraceCopying[If] with
  Printable with
  SettingParams {

  override def search(unlinkedStmtToSearch: Statement): Option[Statement] = {
    if(id == unlinkedStmtToSearch.id) Some(this)
    else {
      val found = body.find(_.id == unlinkedStmtToSearch.id)

      if(!found.isDefined) None
      else Some(found.head)
    }
  }

  override def setAgent(a: Agent) = {
    super.setAgent(a)
    body.map(_.setAgent(a))
  }

  override def setMessage(m: Message) = {
    super.setMessage(m)
    body.map(_.setMessage(m))
  }

  override def setAction(act: Action) = {
    super.setAction(act)
    body.map(_.setAction(action)) // yes, pointing to the same parent action
  }


  override def hashCode: Int = {
    super.hashCode
//    +
//      body.hashCode
  }

  override def toJson: JValue = {
    ("If" ->
      ("body" -> body.map { _.toJson }) ~
      ("conditionEvaluation" -> conditionEvaluation))
  }

  override def generateDynamic: If = {
    condition = localStateForRead[(Message, Agent) => Boolean](conditionVar) // function taking zero args and returning boolean
    generateStatic
  }

  override def generateStatic: If = {
    code = (m: Message, a: Agent) => {
      conditionEvaluation = condition(m,a)
      if (conditionEvaluation) {
        body.map { x => x.setAgent(a); x.setMessage(m); x.setAction(action) }
        if(action != null) // during testing some one is lazy to put it in an action
          action.toExecute = body ++ action.toExecute
//        conditionEvaluation = true
      } // if
    } // code
    this
  }

  override def traceCopy: If = {
    If.fromJson(toJson)
  }

  override def toString: String = {
    s"IF --> s{condition()}"
  }

  def assignAttributesTo(dstStmt: If): Unit = {
    super.assignAttributesTo(dstStmt)

    dstStmt.condition = condition
    dstStmt.conditionVar = conditionVar
    dstStmt.conditionEvaluation = conditionEvaluation
    dstStmt.associatedWith = associatedWith

    dstStmt.body = body
  }
}

object If extends JsonDeSerializable[If] {

  def apply(cond: (Message, Agent) => Boolean)(stmts: Statement*): If = {
    val newOne = new If
    newOne.condition = cond
    newOne.body = stmts map { x => x.associatedWith = newOne; x }
    newOne
  }

  def apply(condVar: String)(stmts: Statement*): If = {
    LocalState.validateVariable(condVar)

    val newOne = new If
    newOne.conditionVar = condVar
    newOne.body = stmts map { x => x.associatedWith = newOne; x }
    newOne.isDynamic = true
    newOne
  }

  def apply(condVar: String, agentStateHoldsCondVar: Boolean)(stmts: Statement*): If = {
    val newOne = If(condVar)(stmts:_*)
    newOne.agentStateRead1 = agentStateHoldsCondVar
    newOne
  }

  // def apply(condVar: String, returnValueVar: String)(stmts: Statement*): If = {
  //   LocalState.validateVariable(condVar)
  //   LocalState.validateVariable(returnValueVar)

  //   val newOne = new If
  //   newOne.conditionVar = condVar
  //   newOne.returnValueVar = returnValueVar
  //   newOne.body = stmts
  //   newOne
  // }


  def fromJson(js: JValue): If = {
    val newOne = new If
    Statement.fromJson(js \ "Statement").assignAttributesTo(newOne)
    js \ "If" \ "body" match {
      case JArray(x) => newOne.body = x.map { x => Statement.fromJson(x) }
      case _ => throw new Error("If.fromJson -- can't extract body statement(s)")
    }

    js \ "If" \ "conditionEvaluation" match {
      case JBool(x) => newOne.conditionEvaluation = x
      case _ => throw new Error("If.fromJson -- can't extract conditionEvaluation")
    }

    js \ "If" \ "conditionVar" match {
      case JString(x) => newOne.conditionVar = x
      case _ => throw new Error("If.fromJson -- can't extract conditionVar")
    }

    // ignore the condition, it is by default set to () => true for If

    newOne
  }
}
