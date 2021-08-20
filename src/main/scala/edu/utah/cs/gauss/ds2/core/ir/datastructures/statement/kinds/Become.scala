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
@SerialVersionUID(1136)
class Become extends Statement with
  Becoming with JsonSerializable with
  TraceCopying[Become] with
  Printable {

  override def hashCode: Int = super.hashCode

  override def toJson: JValue = {
    ("Become" ->
      ("behaviorName" -> behaviorName) ~
      ("behaviorNameVar" -> behaviorNameVar) ~
      ("remember" -> remember) ~
      ("rememberVar" -> rememberVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Become = {
    behaviorName = localStateForRead(behaviorNameVar).asInstanceOf[String]
    remember = localStateForAnotherRead(rememberVar).asInstanceOf[Boolean]
    generateStatic
  }

  override def generateStatic: Become = {
    code = (m: Message, a: Agent) => { ds.become(a, behaviorName, remember) }
    this
  }

  override def traceCopy: Become =  Become.fromJson(toJson)

  override def toString: String = {
    val agentNme = a.name

    s"BECOME --> ${agentNme} --> ${behaviorName}, remember: ${remember}"
  }

  private def assignOnlyTo(dstStmt: Become) = {
    dstStmt.behaviorName = behaviorName
    dstStmt.behaviorNameVar = behaviorNameVar
    dstStmt.remember = remember
    dstStmt.rememberVar = rememberVar
  }
  
  def assignAttributesTo(dstStmt: Become): Unit = {
    super.assignAttributesTo(dstStmt)
    assignOnlyTo(dstStmt)
  }
}

object Become extends JsonDeSerializable[Become] {

  def apply(behaviorName: String, remember: Boolean = false): Become = {
    val newOne = new Become

    newOne.behaviorName = behaviorName
    newOne.remember = remember

    newOne
  }

  def apply(behaviorName: String, rememberVar: String): Become = {
    //LocalState.validateVariable(behaviorNameVar)
    LocalState.validateVariable(rememberVar)

    val newOne = new Become

    //newOne.behaviorNameVar = behaviorNameVar
    newOne.behaviorName = behaviorName
    newOne.rememberVar = rememberVar
    newOne.isDynamic = true

    newOne
  }

  def apply(behaviorNameVar: String, rememberVar: String, agentStateholdsBehaviorName: Boolean, agentStateHoldsRemember: Boolean): Become = {
    val newOne = new Become
    newOne.isDynamic = true
    newOne.behaviorNameVar = behaviorNameVar
    newOne.rememberVar = rememberVar
    newOne.agentStateRead1 = agentStateholdsBehaviorName
    newOne.agentStateRead2 = agentStateHoldsRemember
    newOne
  }

  def fromJson(js: JValue): Become = {
    val newOne = new Become
    Statement.fromJson(js \ "Become" \ "Statement").assignAttributesTo(newOne)

    js \ "Become" \ "behaviorName" match {
      case JString(x) => newOne.behaviorName = x
      case _          => throw new Error("Become.fromJson -- can't extract behaviorName")
    }

    js \ "Become" \ "behaviorNameVar" match {
      case JString(x) => newOne.behaviorNameVar = x
      case _          => throw new Error("Become.fromJson -- can't extract behaviorNameVar")
    }

    js \ "Become" \ "remember" match {
      case JBool(x) => newOne.remember = x
      case _        => throw new Error("Become.fromJson -- can't extract remember")
    }

    js \ "Become" \ "rememberVar" match {
      case JString(x) => newOne.rememberVar = x
      case _          => throw new Error("Become.fromJson -- can't extract rememberVar")
    }

    newOne
  }
}
