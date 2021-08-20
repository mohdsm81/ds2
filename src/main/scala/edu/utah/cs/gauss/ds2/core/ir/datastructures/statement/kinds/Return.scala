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
@SerialVersionUID(1120)
class Return extends Statement with
  Returning with JsonSerializable with
  TraceCopying[Return] with
  Printable {
  override def hashCode: Int = {
    super.hashCode() // +
      // returnValueVariable.hashCode() +
      // returnVariable.hashCode()
    // yes, i skipped returnValue in purpose
  }

  override def traceCopy: Return = {
    Return.fromJson(toJson)
  }

  override def generateDynamic: Return = {
    returnValue = localState(returnValueVariable)
    generateStatic
  }

  override def generateStatic: Return = {
    // to determine which type of constructor was used (if false means return from frame, not returning a value)
    if (LocalState.isValidVarName(returnVariable) &&
          (returnValue != None ||
             LocalState.isValidVarName(returnValueVariable)))
      code = (m: Message, a: Agent) => { callerLocalState(returnVariable) = returnValue }
    else
      code = (m: Message, a: Agent) => {
        // removing the current running frame
        a.stack.pop
        // now removing all statements that belong to that frame from the action.toExecute
        associatedWith.skip
      }
    this
  }

  override def toString: String = {
    s"RETURN --> ${returnVariable}"
  }

  def assignAttributesTo(dstStmt: Return): Unit = {
    super.assignAttributesTo(dstStmt)
    // dstStmt.returnValue = returnValue
    dstStmt.returnVariable = returnVariable
    dstStmt.returnValueVariable = returnValueVariable
  }

  override def toJson: JValue = {
    ("Return" ->
      ("returnVariable" -> returnVariable) ~
      ("returnValueVariable" -> returnValueVariable) ~
      ("returnValue" -> LocalState.serialize(returnValue)) ~
      ("Statement" -> super.toJson))
  }
}

object Return extends JsonDeSerializable[Return] {

  def apply(returnVariable: String, returnValue: Any)(associateWith: Statement): Return = {
    LocalState.validateVariable(returnVariable)
    val newOne = new Return
    newOne.returnValue = returnValue
    newOne.returnVariable = returnVariable
    newOne.associatedWith = associateWith
    newOne
  }

  def apply(returnVariable: String, returnValueVariable: String)(associatedWith: Statement): Return = {
    LocalState.validateVariable(returnVariable)
    LocalState.validateVariable(returnValueVariable)

    val newOne = new Return
    newOne.returnValueVariable = returnValueVariable
    newOne.returnVariable = returnVariable
    newOne.associatedWith = associatedWith
    newOne.isDynamic = true
    newOne
  }

  // This is a lexury and won't be coded till absolutely needed. That is, returning "from a funcation/scope" in contrast to returning a value.
  def apply(associatedWith: Statement): Return = {
    val newOne = new Return
    newOne.returnVariable = ""
    newOne.returnValueVariable = ""
    newOne.returnValue = None
    newOne.associatedWith = associatedWith
    newOne
  }

  def fromJson(js: JValue): Return = {
    val json = js \ "Return"
    val newOne = new Return
    Statement.fromJson(json \ "Statement").assignAttributesTo(newOne)

    json \ "returnVariable" match {
      case JString(x) => newOne.returnVariable = x
      case _ => throw new Error("Return.fromJson -- can't extract returnVariable")
    }

    json \ "returnValueVariable" match {
      case JString(x) => newOne.returnValueVariable = x
      case _ => throw new Error("Return.fromJson -- can't extract returnValueVariable")
    }

    json \ "returnValue" match {
      case null => newOne.returnValue = null
      case x: JObject => newOne.returnValue = LocalState.deSerialize(x)
      case _ => throw new Error("Return.fromJson -- can't extract returnValue")
    }

    newOne
  }
}
