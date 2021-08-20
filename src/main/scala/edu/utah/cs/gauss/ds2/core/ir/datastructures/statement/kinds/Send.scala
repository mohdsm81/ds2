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
@SerialVersionUID(1119)
class Send extends
  Statement with Sending with
  JsonSerializable with
  TraceCopying[Send] with
  Printable with
  AgentStateAccessing {

  agentStateWrite = false // send can never write something to localstate

  override def hashCode: Int = {
    // val msgOutHash = if (null != msgOut) msgOut.hashCode else 0

    super.hashCode() // +
      // //      dstAgent.hashCode() +
      // dstAgentName.hashCode() +
      // dstAgentVar.hashCode() +
      // msgOutHash +
      // msgOutVar.hashCode()
  }

  override def generateStatic: Send = {
    // the reason why we use ds.get(agent.name) is it will get the agent from the "current" distributed system  snapshot
    code = (m: Message, a: Agent) => { ds.send(ds.get(a.name), msgOut, ds.get(dstAgent.name)) }
    this
  }

  override def generateDynamic: Send = {
    // stored in agentStateRead1
    msgOut = localStateForRead(msgOutVar).asInstanceOf[Message]
    // stored in agentStateRead2
    dstAgent = localStateForAnotherRead(dstAgentVar).asInstanceOf[Agent]

    generateStatic
  }

  override def toJson: JValue = {

    val mOut = if (null == msgOut) null else msgOut.toJson
    val dstAgntName = dstAgent.name

    ("Send" ->
      ("dstAgentName" -> dstAgntName) ~
      ("dstAgentVar" -> dstAgentVar) ~
      ("msgOut" -> mOut) ~
      ("msgOutVar" -> msgOutVar) ~
      ("Statement" -> super.toJson))
  }

  override def traceCopy: Send = {
    Send.fromJson(toJson)
  }

  override def toString: String = {
    val src = a match{
      case null => null
      case x:Agent => x.name
    }
    val dst = dstAgent match{
      case null => null
      case x:Agent => x.name
    }
    val mName = msgOut match{
      case null => null
      case m:Message => m.name
    }

      s"SEND: ${src} --${mName}--> ${dst}"
  }

  def assignAttributesTo(dstStmt: Send): Unit = {
    super.assignAttributesTo(dstStmt)

    dstStmt.dstAgent = dstAgent
    dstStmt.dstAgentVar = dstAgentVar

    dstStmt.msgOut = msgOut
    dstStmt.msgOutVar = msgOutVar
  }
}

object Send extends JsonDeSerializable[Send] {

  /**
   * This method constructs a send statements
   * @param m
   * @param dst
   * @return the constructed and instrumented statement
   */
  def apply(m: Message, dst: Agent): Send = {
    require(null != dst, "dst can't be null")
    require(null != m, "msgOut can't be null")

    val stmt = new Send
    stmt.instrumented = true

    stmt.msgOut = m
    stmt.dstAgent = dst

    stmt
  }

  /**
   * This statement constructs a dynamic send statements
   * @param m
   * @param dst
   * @return the constructed and instrumented statement
   */
  def apply(m: String, dst: String): Send = {
    LocalState.validateVariable(m)
    LocalState.validateVariable(dst)

    val stmt = new Send
    stmt.instrumented = true
    stmt.isDynamic = true

    stmt.msgOutVar = m
    stmt.dstAgentVar = dst // stmt.dstAgent.name

    stmt
  }

  /**
   * This statement constructs a dynamic send statements, with option
   * if getting its arguments from agent's local state
   * @param m
   * @param dst
   * @param agentStateHoldsMsg local state of the agent holds the message, if true. Top of the stack local state otherwise.
   * @param agentStateHoldsDstAgent local state of the agent holds the destination agent, if true. Top of the stack local state otherwise.
   * @return the constructed and instrumented statement
   */
  def apply(m: String, dst: String, agentStateHoldsMsg: Boolean, agentStateHoldsDstAgent: Boolean): Send = {
    val newOne = Send.apply(m, dst)
    newOne.agentStateRead1 = agentStateHoldsMsg
    newOne.agentStateRead2 = agentStateHoldsDstAgent
    newOne
  }

  def fromJson(js: JValue): Send = {
    val stmt = new Send
    Statement.fromJson(js \ "Send" \ "Statement").assignAttributesTo(stmt)


    js \ "Send" \ "dstAgentVar" match {
      case JString(x) => stmt.dstAgentVar = x
      case _          => throw new Error("Send.fromJson -- can't extract the dstAgentVar")
    }

    js \ "Send" \ "msgOut" match {
      case null       ⇒ stmt.msgOut = null
      case x: JObject ⇒ stmt.msgOut = Message.fromJson(x)
      case _ ⇒
        println(js \ "Send" \ "msgOut")
        throw new Error("Send.fromJson - can't extract 'msgOut'")
    }

    js \ "Send" \ "msgOutVar" match {
      case JString(x) => stmt.msgOutVar = x
      case _ =>
        println(js \ "Send" \ "msgOutVar")
        throw new Error("Send.fromJson -- can't extract the msgOutVar")
    }

    stmt
  }
}
