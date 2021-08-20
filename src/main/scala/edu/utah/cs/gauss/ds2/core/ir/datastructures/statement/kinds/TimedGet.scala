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
@SerialVersionUID(1115)
class TimedGet extends Get with
  Timing with JsonSerializable with
  TraceCopying[TimedGet] with
  Printable {

  override def hashCode: Int = {
    super.hashCode // +
      // timeout.hashCode
  }

  override def toJson: JValue = {
    ("TimedGet" ->
      ("timeout" -> timeout) ~
      ("timeoutVar" -> timeoutVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: TimedGet = {
    super.generateDynamic

    if (LocalState.isValidVarName(timeoutVar) && timeout > 0)
      localStateForWrite(timeoutVar) = timeout

    timeout = localStateForRead(timeoutVar).asInstanceOf[BigInt]
    generateStatic
    this
  }

  override def generateStatic: TimedGet = {
    super.generateStatic
    val dstVar = if (dstVariableName.isEmpty) None else Some(dstVariableName)
    code = (m: Message, a: Agent) => { ds.getTimed(future, dstVar, timeout, action) }
    this
  }

  override def traceCopy: TimedGet = {
    TimedGet.fromJson(toJson)
  }

  override def toString: String = {
    val agentNme = a match{
      case null => null
      case x:Agent => x.name
    }

    val to = if (isDynamic) a.getVariable(timeoutVar).asInstanceOf[BigInt] else timeout
    val futureID = if (null != future) future.id else 0

    s"TIMED_GET --> by ${agentNme} for future id ${futureID} for ${to} ticks"
  }

  def assignAttributesTo(dstStmt: TimedGet): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.timeout = timeout
    dstStmt.timeoutVar = timeoutVar
  }

}

object TimedGet extends JsonDeSerializable[TimedGet] {

  def apply(future: DummyFuture, dstVariableName: String, timeout: BigInt): TimedGet = {
    val newOne = new TimedGet
    newOne.future = future
    newOne.dstVariableName = dstVariableName
    newOne.timeout = timeout
    newOne
  }

  def apply(futureVar: String, dstVariableName: String, timeoutVar: String): TimedGet = {
    LocalState.validateVariable(futureVar)
    LocalState.validateVariable(dstVariableName)
    LocalState.validateVariable(timeoutVar)

    val newOne = new TimedGet
    newOne.isDynamic = true
    newOne.futureVar = futureVar
    newOne.dstVariableName = dstVariableName
    newOne.timeoutVar = timeoutVar
    newOne
  }

  def apply(futureVar: String, dstVariableName: String, timeout: BigInt, timeoutVar: String): TimedGet = {
    LocalState.validateVariable(futureVar)
    LocalState.validateVariable(dstVariableName)
    LocalState.validateVariable(timeoutVar)
    val newOne = new TimedGet
    newOne.isDynamic = true
    newOne.futureVar = futureVar
    newOne.dstVariableName = dstVariableName
    newOne.timeoutVar = timeoutVar
    newOne.timeout = timeout // this timeout gets inserted into the agent for you every time the statement is generated, only in dynamic mode

    newOne
  }

  def apply(futureVar: String, dstVariableName: String, timeout: BigInt = 0, timeoutVar: String, agentStateHoldsFutureVar: Boolean, agentStateHoldsDstVar: Boolean): TimedGet = {
    val newOne = TimedGet(futureVar, dstVariableName, timeout, timeoutVar)
    newOne.agentStateRead1 = agentStateHoldsFutureVar
    newOne.agentStateWrite = agentStateHoldsDstVar
    newOne
  }

  def fromJson(js: JValue): TimedGet = {
    val newOne = new TimedGet
    Get.fromJson(js \ "TimedGet" \ "Statement").assignAttributesTo(newOne)

    js \ "TimedGet" \ "timeout" match {
      case JInt(x) => newOne.timeout = x
      case _       => throw new Error("TimedGet.fromJson -- can't extract timeout")
    }

    js \ "TimedGet" \ "timeoutVar" match {
      case JString(x) => newOne.timeoutVar = x
      case _          => throw new Error("TimedGet.fromJson -- can't extract timeoutVar")
    }

    newOne
  }
}
