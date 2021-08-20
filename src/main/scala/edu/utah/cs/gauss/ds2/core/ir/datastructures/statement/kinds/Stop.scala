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
@SerialVersionUID(1117)
class Stop extends Statement with Stopping with JsonSerializable with TraceCopying[Stop] with Printable {

  override def hashCode: Int = {
    // val dstAgentHash = if (null != dstAgent) dstAgent.hashCode else 0

    super.hashCode // +
      // dstAgentHash +
      // dstAgentName.hashCode +
      // dstAgentVar.hashCode
  }

  override def toJson: JValue = {
    val dstAgentNme = dstAgent match{
      case null => null
      case x:Agent => x.name
    }

    ("Stop" ->
      ("dstAgentName" -> dstAgentNme) ~
      ("dstAgentVar" -> dstAgentVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Stop = {
    dstAgent = localStateForRead(dstAgentVar).asInstanceOf[Agent]
    generateStatic
    this
  }

  override def generateStatic: Stop = {
    code = (m: Message, a: Agent) => { ds.stop(a, dstAgent) }
    this
  }

  override def traceCopy: Stop = {
    Stop.fromJson(toJson)
  }

  override def toString: String = {
    val agntName = dstAgent match{
      case null => null
      case x:Agent => x.name
    }

    s"STOP --> ${agntName}"
  }

  private def assignOnlyTo(dstStmt: Stop): Unit = {
    dstStmt.dstAgent = dstAgent
    dstStmt.dstAgentVar = dstAgentVar
  }

  def assignAttributesTo(dstStmt: Stop): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
  }

}

object Stop extends JsonDeSerializable[Stop] {
  
  def apply(dstAgent: Agent): Stop = {
    val newOne = new Stop
    newOne.dstAgent = dstAgent

    newOne
  }

  def apply(dstAgentVar: String): Stop = {
    LocalState.validateVariable(dstAgentVar)

    val newOne = new Stop
    newOne.dstAgentVar = dstAgentVar

    newOne.isDynamic = true
    newOne
  }

  def apply(dstAgentVar: String, agentStateHoldsDstAgentVar: Boolean): Stop = {
    val newOne = Stop(dstAgentVar)
    newOne.agentStateRead1 = agentStateHoldsDstAgentVar
    newOne
  }

  def fromJson(js: JValue): Stop = {
    val newOne = new Stop
    Statement.fromJson(js \ "Stop" \ "Statement").assignAttributesTo(newOne)
    
    js \ "Stop" \ "dstAgentVar" match {
      case JString(x) => newOne.dstAgentVar = x
      case _          => throw new Error("Stop.fromJson -- can't extract dstAgentVar")
    }

    newOne
  }

}
