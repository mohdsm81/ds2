package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.ir.features._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
@SerialVersionUID(1137)
class Ask extends Send with
  Asking with JsonSerializable with
  TraceCopying[Ask] with
  Printable {

  override def hashCode: Int = {
    super.hashCode() // +
      // future.hashCode() +
      // futureVar.hashCode()
  }

  override def toJson: JValue = {
    ("Ask" ->
      ("future" -> future.toJson) ~
      ("futureVar" -> futureVar) ~
      ("Statement" -> super.toJson))
  }
  override def generateDynamic: Ask = {
    super.generateDynamic
    generateStatic
  }

  override def apply = {

    super.apply

    if (isDynamic) {
      future.storedInVariable = futureVar
      localStateForWrite(futureVar) = future
    }
  }

  override def generateStatic: Ask = {
    code = (m: Message, a: Agent) => {
      future = ds.ask(a, msgOut, dstAgent)
    }
    this
  }


  override def traceCopy: Ask = Ask.fromJson(toJson)

  override def toString: String = {
    // an ask encapsulating an ask of such and such attributes.
    s"ASK --> (${super.toString})"
  }

  def assignAttributesTo(dstStmt: Ask): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.future = future
  }
}

object Ask extends JsonDeSerializable[Ask] {

  def apply(m: Message, dst: Agent): Ask = {
    // note the future is returned inside this statement instead, use the dynamic version better
    val newOne = new Ask
    Send(m, dst).assignAttributesTo(newOne)

    newOne
  }

  def apply(msgOutVar: String, dstAgentVar: String, futureVar: String): Ask = {
    LocalState.validateVariable(msgOutVar)
    LocalState.validateVariable(dstAgentVar)
    LocalState.validateVariable(futureVar)

    val newOne = new Ask
    Send(msgOutVar, dstAgentVar).assignAttributesTo(newOne)
    newOne.futureVar = futureVar
    newOne.isDynamic = true

    newOne
  }

  def apply(msgOutVar: String, dstAgentVar: String, futureVar: String, agentStateHoldsMsg: Boolean, agentStateHoldsDstAgent: Boolean, agentStateHoldsFuture: Boolean): Ask = {
    val newOne = Ask(msgOutVar, dstAgentVar, futureVar)
    newOne.agentStateRead1 = agentStateHoldsMsg
    newOne.agentStateRead2 = agentStateHoldsDstAgent
    newOne.agentStateWrite = agentStateHoldsFuture
    newOne
  }

  def fromJson(js: JValue): Ask = {
    val newOne = new Ask
    Send.fromJson(js \ "Ask" \ "Statement").assignAttributesTo(newOne)

    js \ "Ask" \ "future" match {
      case null       => newOne.future = null
      case x: JObject => newOne.future = DummyFuture.fromJson(x)
      case _          => throw new Error("Ask.fromJson -- can't extract a future object")
    }
    js \ "Ask" \ "futureVar" match {
      case JString(x) => newOne.futureVar = x
      case _          => throw new Error("Ask.fromJson -- can't extract a futureVar string")
    }

    js \ "Ask" \ "agentStateWrite" match {
      case JBool(x) => newOne.agentStateWrite = x
      case _        => throw new Error("Ask.fromJson -- can't extract a agentStateWrite")
    }

    newOne
  }
}
