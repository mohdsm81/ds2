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
@SerialVersionUID(1127)
class Kill extends Statement with
  Killing with JsonSerializable with
  TraceCopying[Kill] with
  Printable {

  override def hashCode: Int = {
//    val agentHash = if (null == dstAgent) 0 else dstAgent.hashCode

    super.hashCode // +
      // agentHash +
      // dstAgent.hashCode +
      // dstAgentVar.hashCode
  }

  override def toJson: JValue = {
    ("Kill" ->
      ("dstAgentVar" -> dstAgentVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Kill = {
    dstAgent = localStateForRead(dstAgentVar).asInstanceOf[Agent]
    generateStatic
  }

  override def generateStatic: Kill = {
    code = (m: Message, a: Agent) => { ds.kill(a, dstAgent) }
    this
  }

  override def traceCopy: Kill = {
    Kill.fromJson(toJson)
  }


  override def toString: String = {
    val dstAgentNme = dstAgent match{
      case null => null
      case x:Agent => x.name
    }

    val agentNme = a match{
      case null => null
      case x:Agent => x.name
    }

    s"KILL --> ${agentNme} --> ${dstAgentNme}"
  }

  private def assignOnlyTo(dstStmt: Kill): Unit = {
    dstStmt.dstAgent = dstAgent
    dstStmt.dstAgentVar = dstAgentVar
  }
  def assignAttributesTo(dstStmt: Kill): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
  }

}

object Kill extends JsonDeSerializable[Kill] {

  def apply(dstAgent: Agent): Kill = {
    require(null != dstAgent, "Kill.apply(dstAgent) -- argument can't be null")

    val newOne = new Kill
    newOne.dstAgent = dstAgent
    newOne
  }

  def apply(dstAgentVar: String): Kill = {
    LocalState.validateVariable(dstAgentVar)

    val newOne = new Kill
    newOne.dstAgentVar = dstAgentVar
    newOne.isDynamic = true

    newOne
  }

  def apply(dstAgentVar: String, agentStateHoldsDstAgentVar : Boolean): Kill = {
    val newOne = Kill(dstAgentVar)
    newOne.agentStateRead1 = agentStateHoldsDstAgentVar
    newOne
  }

  def fromJson(js: JValue): Kill = {
    val newOne = new Kill
    Statement.fromJson(js \ "Kill" \ "Statement").assignAttributesTo(newOne)

    js \ "Kill" \ "dstAgentVar" match {
      case JString(x) => newOne.dstAgentVar = x
      case _          => throw new Error("Kill.fromJson -- can't extract dstAgentVar")
    }

    newOne
  }
}
