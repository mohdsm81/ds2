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
@SerialVersionUID(1134)
class Create extends Statement with
  Creating with JsonSerializable with
  TraceCopying[Create] with
  Printable {

  override def hashCode: Int = {
    super.hashCode // +
      // childAgentName.hashCode +
      // childAgentNameVar.hashCode
  }

  override def toJson: JValue = {
    ("Create" ->
      ("childAgentName" -> childAgentName) ~
      ("childAgentNameVar" -> childAgentNameVar) ~
      ("Statement" -> super.toJson))
  }

  override def generateDynamic: Create = {
    childAgentName = localStateForRead(childAgentNameVar).asInstanceOf[String]
    generateStatic
  }

  override def generateStatic: Create = {
    super.generateStatic

    code = (m: Message, a: Agent) => { ds.create(a, childAgentName) }

    this
  }

  override def traceCopy: Create = Create.fromJson(toJson)

  override def toString: String = {
    val agentNme = a match{
      case null => null
      case x:Agent => x.name
    }
    
    s"CREATE --> ${agentNme} creates ${childAgentName}"
  }

  def assignAttributesTo(dstStmt: Create): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.childAgentName = childAgentName
    dstStmt.childAgentNameVar = childAgentNameVar
  }

}

object Create extends JsonDeSerializable[Create] {
  
  def apply(childAgentOrVar: String): Create = {
    val newOne = new Create
    
    // only one apply(String) method is allowed, this is why the following
    LocalState.isValidVarName(childAgentOrVar) match {
      case true => newOne.childAgentNameVar = childAgentOrVar; newOne.isDynamic = true
      case false => newOne.childAgentName = childAgentOrVar
    }
    newOne
  }

  def apply(childAgentOrVar: String, agentStateHoldsChildAgentVar: Boolean): Create  = {
    val newOne = Create(childAgentOrVar)
    newOne.agentStateRead1 = agentStateHoldsChildAgentVar
    newOne
  }
  
  def fromJson(js: JValue): Create = {
    val newOne = new Create

    Statement.fromJson(js \ "Create" \ "Statement").assignAttributesTo(newOne)

    js \ "Create" \ "childAgentName" match {
      case JString(x) => newOne.childAgentName = x
      case _          => throw new Error("Create.fromJson -- can't extract childAgentName")
    }

    js \ "Create" \ "childAgentNameVar" match {
      case JString(x) => newOne.childAgentNameVar = x
      case _          => throw new Error("Create.fromJson -- can't extract childAgentNameVar")
    }

    newOne
  }
}
