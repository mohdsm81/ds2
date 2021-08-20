package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Action, Agent, Message}
import edu.utah.cs.gauss.ds2.core.ir.features.Printable
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
@SerialVersionUID(1130)
class FunctionRecurse extends Function with
  TraceCopying[FunctionRecurse] with
  Printable with SettingParams{


  override def setAgent(a: Agent) = this.a = a

  override def setMessage(m: Message) = this.m = m

  override def setAction(act: Action) = action = act

  override def toJson: JValue = {
    ("FunctionRevurse" ->
      ("function" -> super.toJson))
  }

  override def traceCopy: FunctionRecurse = {
    FunctionRecurse.fromJson(toJson)
  }

  override def generateStatic: FunctionRecurse = {
    // same statements as associatedWith (associatedwith is definitely
    // a function as it is illegal to call FunctionRecurse from outside a
    // function)
    associatedWith match {
      case x:Function =>
        body = body // no need for linking as we are not migrating to a new copy of a function here
        body.map{
          case x:FunctionRecurse => ; // do nothing
          case x => x.associatedWith = this
        }
      case _ => throw new Error("FunctionRecurse.generateStatic -- you can't call FunctionRecurse from outside a function's body")
    }
    super.generateStatic
    this
  }

  override def generateDynamic: FunctionRecurse = {
    super.generateDynamic
    generateStatic
  }

  override def toString: String = {
    s"FUNCTION_RECURSE --> ${functionName}"
  }
}

object FunctionRecurse extends JsonDeSerializable[FunctionRecurse] {

  def apply(funcName: Option[String])(parameterList: Either[Seq[Any], Seq[String]])// (function: Function)
      : FunctionRecurse = {
    val newOne = new FunctionRecurse
    Function(funcName)(parameterList)().assignAttributesTo(newOne)
    newOne.frame.parameterList = parameterList
    // newOne.associatedWith = function // this is taken care if by the front end
    // later in generate static and dynamic, stmts and funcName are assigned from associatedWith field.
    newOne
  }

  override def fromJson(js: JValue): FunctionRecurse = {
    val newOne = new FunctionRecurse
    Function.fromJson(js \ "FunctionRecurse" \ "function").assignAttributesTo(newOne)
    newOne
  }
}
