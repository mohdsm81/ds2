package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.features.Printable
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.language.postfixOps
/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */
@SerialVersionUID(1135)
class BootStrap extends Statement with
  BootStrapping with JsonSerializable with
  TraceCopying[BootStrap] with
  Printable {

  // it never writes to localstate
  agentStateWrite = false

  override def hashCode: Int = {
    super.hashCode // +
      // bootStrapped.hashCode
  }

  override def toJson: JValue = {
    ("BootStrap" ->
      ("bootStrappedVar" -> bootStrappedVar) ~
      ("bootStrapped" -> bootStrapped.map { x => (x._1 -> x._2) }) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: BootStrap = {
    bootStrapped = localStateForRead(bootStrappedVar).asInstanceOf[Set[(String, String)]]
    generateStatic
  }

  override def generateStatic: BootStrap = {
    code = (m: Message, a: Agent) => { bootStrapped map { x => ds.bootStrap(ds.get(x._1), x._2) } }
    this
  }

  override def traceCopy: BootStrap = BootStrap.fromJson(toJson)

  override def toString: String = {
    s"BOOTSTRAP --> ${bootStrapped.map { x => s"(${x._1},${x._2})" }.mkString(",")}"
  }

  def assignAttributesTo(dstStmt: BootStrap): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.bootStrapped = bootStrapped
    dstStmt.bootStrappedVar = bootStrappedVar
  }
}

object BootStrap extends JsonDeSerializable[BootStrap] {

  def apply(bootStrapped: Set[(String, String)]): BootStrap = {
    val newOne = new BootStrap
    newOne.bootStrapped = bootStrapped
    newOne
  }

  def apply(bootStrappedVar: String): BootStrap = {
    LocalState.validateVariable(bootStrappedVar)

    val newOne = new BootStrap
    newOne.bootStrappedVar = bootStrappedVar
    newOne.isDynamic = true
    newOne
  }

  def apply(bootStrappedVar: String, agentStateHoldsBootStrappedVar: Boolean): BootStrap = {
    val newOne = BootStrap(bootStrappedVar)
    newOne.agentStateRead1 = agentStateHoldsBootStrappedVar
    newOne
  }

  def fromJson(js: JValue): BootStrap = {
    val newOne = new BootStrap
    Statement.fromJson(js \ "BootStrap" \ "Statement").assignAttributesTo(newOne)

    js \ "BootStrap" \ "bootStrapped" match {
      case JArray(x) => newOne.bootStrapped = x map {
        case JField(x, JString(y)) => (x, y)
        case _                     => throw new Error("BootStrap.fromJson -- can't extract a boot strapped tuple")
      } toSet
      case _ => throw new Error("BootStrap.fromJson -- can't extract bootStrapped")
    }

    js \ "BootStrapped" \ "bootStrappedVar" match {
      case JString(x) => newOne.bootStrappedVar = x
      case _          => throw new Error("BootStrap.fromJson -- can't extract bootStrappedVar")
    }

    newOne
  }
}
