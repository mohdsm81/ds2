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
@SerialVersionUID(1110)
class While extends If with
  Looping with JsonSerializable
  with TraceCopying[While] with
  Printable {

  override def toJson: JValue = {
    body = body.dropRight(1) // drop the final while-stmt at the end

    ("While" ->
      ("count" -> count) ~
      ("Statement" -> super.toJson))

  }

  override def generateDynamic: While = {
    super.generateDynamic
    generateStatic
  }

  override def generateStatic: While = {
    code = (m: Message, a: Agent) => {
      conditionEvaluation = condition(m, a)
      if (conditionEvaluation) {
        body.map { x => x.setAgent(a); x.setMessage(m); x.setAction(action) }
        if (action != null) // during testing some one is lazy to put it in an action
          action.toExecute = body ++ action.toExecute
        //        conditionEvaluation = true
      } // if
//      else skip
    }
    this
  }
  
  override def setAction(act: Action): Unit = {
    this.action = act
    body = body.dropRight(1) // drop the source if infinite loop (this statement)
    body.map(_.setAction(act)) // set the agent
    body = body :+ this
  }

  override def setAgent(a: Agent): Unit = {
    this.a = a
    body = body.dropRight(1) // drop the source if infinite loop (this statement)
    body.map(_.setAgent(a)) // set the agent
    body = body :+ this
  }
  
  override def setMessage(m: Message): Unit = {
    this.m = m
    body = body.dropRight(1) // drop the source if infinite loop (this statement)
    body.map(_.setMessage(m)) // set the agent
    body = body :+ this
  }

  /*override def setMessage(m: Message): Unit = {
    this.m = m
    body = body.dropRight(1) // drop the source if infinite loop (this statement)
    body.map(_.setMessage(m)) // set the agent
    body = body :+ this
  }*/

  override def traceCopy: While = {
    While.fromJson(toJson)
  }

  override def toString: String = {
    s"WHILE --> ${conditionEvaluation}"
  }

  def assignAttributesTo(dstStmt: While): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.count = count
  }

  override def apply: Unit = {
    // first skip this while, then apply it: so that no While-accumulation at each body-prepend to Action.toExecute
    if(!action.toExecute.isEmpty && action.toExecute.head == this)
      skip
    super.apply
    if (conditionEvaluation) count += 1
  }

}

object While extends JsonDeSerializable[While]{

  def apply(cond: (Message, Agent) => Boolean)(stmts: Statement*): While = {
    val newOne = new While
    newOne.condition = cond
    newOne.body = stmts.map { x => x.associatedWith = newOne; x }
    newOne.body = newOne.body ++ Seq(newOne) // you know, looping
//    newOne.body = Seq(Statement((_,_) => {})) ++ newOne.body // for some reason this is needed! not to skip the first statement of a While.body
    newOne
  }

  def apply(condVar: String)(stmts: Statement*): While = {
    LocalState.validateVariable(condVar)

    val newOne = new While
    newOne.conditionVar = condVar
    newOne.body = stmts.map { x => x.associatedWith = newOne; x }
    newOne.body = newOne.body ++ Seq(newOne) // you know, looping
//    newOne.body = Seq(Statement((_,_) => {})) ++ newOne.body // for some reason this is needed! not to skip the first statement of a While.body
    newOne.isDynamic = true
    newOne
  }

  def apply(condVar: String, agentStateHoldsCondVar: Boolean)(stmts: Statement*): While = {
    val newOne = While(condVar)(stmts:_*)
    newOne.agentStateRead1 = agentStateHoldsCondVar
    newOne
  }


  def fromJson(js: JValue): While = {
    val newOne = new While
    If.fromJson(js \ "While" \ "Statement").assignAttributesTo(newOne)

    js \ "While" \ "count" match {
      case JInt(x) => newOne.count = x.toInt
      case _ => throw new Error("While.fromJson -- couldn't extract count")
    }

    newOne.body = newOne.body ++ Seq(newOne)

    newOne
  }
}
