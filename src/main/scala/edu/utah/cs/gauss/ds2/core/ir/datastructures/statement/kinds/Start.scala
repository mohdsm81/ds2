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
@SerialVersionUID(1118)
class Start extends Statement with
  Starting with JsonSerializable with
  TraceCopying[Start] with
  Printable {

  override def hashCode: Int = {
    // val dstAgentHash = if (null != dstAgent) dstAgent.hashCode else 0

    super.hashCode // +
    // dstAgentHash +
    // dstAgentName.hashCode +
    // dstAgentVar.hashCode +
    // args.hashCode() +
    // argsVar.hashCode()
  }

  override def toJson: JValue = {
    ("Start" ->
      ("dstAgentVar" -> dstAgentVar) ~
      ("args" -> args) ~
      ("argsVar" -> argsVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Start = {
    dstAgent = localStateForRead(dstAgentVar).asInstanceOf[Agent]
    args = localStateForAnotherRead(argsVar).asInstanceOf[Seq[String]]
    generateStatic
  }

  override def generateStatic: Start = {
    code = (m: Message, a: Agent) => { ds.start(a, dstAgent, args) }
    this
  }

  override def traceCopy: Start = {
    Start.fromJson(toJson)
  }

  override def toString: String = {
    val dstAgentNme = dstAgent match{
      case null => null
      case x:Agent => x.name
    }

    val starter = a.name

    s"START --> ${starter} starts ${dstAgentNme}"
  }

  private def assignOnlyTo(dstStmt: Start): Unit = {
    dstStmt.dstAgent = dstAgent
    dstStmt.dstAgentVar = dstAgentVar
    dstStmt.args = args
    dstStmt.argsVar = argsVar
  }

  def assignAttributesTo(dstStmt: Start): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
  }

}

object Start extends JsonDeSerializable[Start] {
  def apply(dstAgent: Agent, args: Seq[String] = Seq()): Start = {
    val newOne = new Start
    newOne.dstAgent = dstAgent
    newOne.args = args
    newOne
  }

  def apply(dstAgentVar: String, argsVar: String): Start = {
    LocalState.validateVariable(dstAgentVar)
    LocalState.validateVariable(argsVar)

    val newOne = new Start
    newOne.dstAgentVar = dstAgentVar
    newOne.argsVar = argsVar
    newOne.isDynamic = true
    newOne
  }

  def apply(dstAgentVar: String, argsVar: String, agentStateHoldsDstAgentVar: Boolean, agentStateHoldsArgsVar: Boolean): Start = {
    val newOne = Start(dstAgentVar,argsVar)
    newOne.agentStateRead1 = agentStateHoldsDstAgentVar
    newOne.agentStateRead2 = agentStateHoldsArgsVar
    newOne
  }


  def fromJson(js: JValue): Start = {
    val newOne = new Start
    Statement.fromJson(js \ "Start" \ "Statement").assignAttributesTo(newOne)

    js \ "Start" \ "dstAgentVar" match {
      case JString(x) => newOne.dstAgentVar = x
      case _          => throw new Error("Start.fromJson -- can't extract dstAgentVar")
    }
    js \ "Start" \ "args" match {
      case JArray(x) => newOne.args = x.map {
        case JString(x) => x
        case _          => throw new Error("Start.fromJson -- can't extract an arg")
      }
      case _ => throw new Error("Start.fromJson -- can't extract args")
    }
    js \ "Start" \ "argsVar" match {
      case JString(x) => newOne.argsVar = x
      case _          => throw new Error("Start.fromJson -- can't extract argsVar")
    }

    newOne
  }
}
