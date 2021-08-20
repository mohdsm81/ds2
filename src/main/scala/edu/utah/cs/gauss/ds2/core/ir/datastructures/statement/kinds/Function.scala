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
@SerialVersionUID(1131)
class Function extends Statement with
  FunctionCalling with JsonSerializable with
  TraceCopying[Function] with
  Printable with
  SettingParams{

  var functionName: Option[String] = None // assume anonymous function


  override def setAgent(a: Agent) = {
    super.setAgent(a)
    body.map(_.setAgent(a))
  }

  override def setMessage(m: Message) = {
    super.setMessage(m)
    body.map(_.setMessage(m))
  }

  override def setAction(act: Action) = {
    super.setAction(act)
    body.map(_.setAction(action)) // yes, pointing to the same parent action
  }

  override def hashCode: Int = {
    super.hashCode +
    // frame.hashCode +
    // functionName.hashCode +
    body.hashCode
  }

  def localStateAccessPattern: Seq[LocalState] = {
    frame.parameterAccessPattern map{
      case true => a.localState
      case false => localState
    }
  }

  def cacheParametersValues: Unit = {

    val paramsVars = frame.parameterList match {
      case Right(x) => x
      case _ => throw new Error("Function.cacheParametersValues -- can't extract dynamic parameters list")}

    require(paramsVars.size == frame.parameterAccessPattern.size, "Function.chechParametersValues -- size of parameterList and parameterListAccessPattern should be the same")

    var parameterCachedValues = Seq[Any]()

    // update the frame.cacheparametersvalues list
    (localStateAccessPattern zip paramsVars) map {
      case (x,y) => parameterCachedValues = parameterCachedValues :+ x(y)
      case _ => throw new Error("Function.chechParametersValues -- encountered a strange pattern")}

    // update the function's local state with up todate params/values
    (paramsVars zip parameterCachedValues) map { x => frame.localState(x._1) = x._2}

  }

  override def toJson: JValue = {

    ("Function" ->
       ("functionName" -> functionName) ~
       ("frame" -> frame.toJson ) ~
       ("body" -> body.map{_.toJson}) ~
       ("Statement" -> super.toJson))
  }

  override def generateDynamic: Function = {
    cacheParametersValues 
    generateStatic
  }

  override def generateStatic: Function = {
    super.generateStatic
    code = (m:Message, a:Agent) => {
      body = body.dropRight(1) // removing the last pop-statement

      // and adding pop that will act on this statement
      body = body :+ Statement((m:Message, a:Agent) => pop)
      body.map{ x => x.a = a; x.m = m}
      frame.stack = a.stack
      action.toExecute = body ++ action.toExecute
    }
    this
  }

  override def apply: Unit = {
    // push the frame
    if(!pushed)
      push

    super.apply

    // popping the frame is done only after the "last" statement executes (which is a pop...)
    // the possible statement to be before that last one could be Return
  }

  override def traceCopy: Function = {
    Function.fromJson(toJson)
  }

  override def toString: String = {
    // this looks weird, but can't tell if what was used is either one or the other in a more direct way...
    val paramList = frame.parameterList match {
      case Left(x) => x.mkString(",")
      case Right(x) => x.mkString(",")
    }

    s"FUNCTION(${functionName}) --> ${paramList}"
  }

  def assignAttributesTo(dstStmt: Function): Unit = {
    super.assignAttributesTo(dstStmt)
    dstStmt.frame = frame
    dstStmt.body = body
    dstStmt.functionName = functionName
  }

}

object Function extends JsonDeSerializable[Function] {

  def apply(funcName: Option[String] = None)( parameterList: Either[Seq[Any],Seq[String]])(stmts: Statement*): Function = {
    val newOne = new Function
    newOne.functionName = funcName
    newOne.frame.parameterList = parameterList
    newOne.body = stmts.map{ x => x.associatedWith = newOne; x}
    newOne.body = newOne.body :+ Statement((m:Message, a:Agent) => newOne.pop)
    parameterList match {
      case Right(x) =>
        x.map(LocalState.validateVariable(_))
        newOne.isDynamic = true
      case _ => // do nothing
    }

    newOne
  }


  def apply(funcName: Option[String],parameterList: Either[Seq[Any],Seq[String]], agentLocalStateAccess: Seq[Boolean])(stmts: Statement*): Function = {
    val newOne = Function(funcName)(parameterList)(stmts:_*)
    newOne.frame.parameterAccessPattern = agentLocalStateAccess
    newOne
  }

  def fromJson(js: JValue): Function = {
    val newOne = new Function
    Statement.fromJson(js \ "Function" \ "Statement").assignAttributesTo(newOne)

    js \ "Function" \ "functionName" match {
      case JString(x) => newOne.functionName = Some(x)
      case _ => throw new Error("Function.fromJson -- can't extract functionName")
    }

    js \ "Function" \ "frame" match {
      case x:JObject => newOne.frame = ActivationFrame.fromJson(x)
      case _ => throw new Error("Function.fromJson -- can't extract frame")      
    }

    js \ "Function" \ "body" match {
      case JArray(x) => newOne.body = x.map(Statement.fromJson(_))
      case _ => throw new Error("Function.fromJson -- can't extract body (stmts)")        
    }

    newOne
  }
}
