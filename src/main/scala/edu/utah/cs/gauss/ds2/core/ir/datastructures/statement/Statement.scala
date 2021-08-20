package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement

import java.io._
import java.util.UUID

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{Start => StartStmt, Stop => StopStmt, _}
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.features.{Applicable, Printable}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.language.postfixOps

/**
  * @author <br>
  *         Mohammed S. Al-Mahfoudh <br/>
  *         mahfoudh@cs.utah.edu <br/>
  *         SoC - Gauss Group <br/>
  *
  *         The user needs to override <code>code,m and a</code> of this class to place the body of the statement
  */
@SerialVersionUID(900)
class Statement extends Serializable with
  StaticallyGeneratable[Statement] with
  DynamicallyGeneratable[Statement] with
  Applicable with Printable with // this is not really needed, it is just to tell that toString is to be implemented properly
//  CopyLink[Statement] with
  TraceCopying[Statement] with
  JsonSerializable with AgentStateAccessing with
  SettingParams with Searchable {

  // File info from which this statement came
  var startLineNo: BigInt = -1
  var endLineNo: BigInt = -1
  var fileName = ""

  // invoker message, and agent executing this statement
  var m: Message = _ // scheduler
  var a: Agent = _ // static

  // statement specific identifiers
  var id = UUID.randomUUID // static

  // code executed by the statement
  var code: (Message, Agent) => Unit = (msg: Message, agnt: Agent) ⇒ {}

  def ds: DistributedSystem = a.ds // TAKEN-CARE_OF

  var taskFrom: SuspendableTask = _

  var action: Action = _ // the action containing this statement

  // by default, code is considered not instrumented till one of the instrumentation code is called
  var instrumented = true // static
  var isDynamic = false // set it to true only when generating the dynamic statement variant(s)
  var isFunctional = false // set to true only when generating the function-value dynamic stmt variant(s)

  var associatedWith: Statement = this // normally each statement is lonely, unless it is inside a control flow statement
  var associatedWithForSkipping: Statement = this

  var readVariables: Seq[String] = Seq()

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    startLineNo.hashCode() +
      endLineNo.hashCode() +
      fileName.hashCode() +
      id.hashCode() +
      isDynamic.hashCode() // +
  }

  override def equals(that: Any): Boolean = {
    if (null != that)
      hashCode == that.hashCode
    else
      false
  }

  //==============================
  // Utilities
  //==============================
  def isReadStatement: Boolean = if (readVariables.isEmpty) false else true

  // to be overriden by If and Function (all other control flow will inherit those)
  override def search(unlinkedStmtToSearch: Statement): Option[Statement] = {
    if (id == unlinkedStmtToSearch.id) Some(this)
    else None
  }

  def assignAttributesTo(dstStmt: Statement): Unit = {
    // it looks silly, but it is extremely helpful at copy-ing subclasses
    dstStmt.a = a
    dstStmt.m = m
    dstStmt.action = action
    dstStmt.code = code
    dstStmt.endLineNo = endLineNo
    dstStmt.fileName = fileName
    dstStmt.id = id
    dstStmt.instrumented = instrumented
    dstStmt.isDynamic = isDynamic
    dstStmt.isFunctional = isFunctional
    dstStmt.startLineNo = startLineNo
    dstStmt.taskFrom = taskFrom
    dstStmt.agentStateRead1 = agentStateRead1
    dstStmt.agentStateRead2 = agentStateRead2
    dstStmt.agentStateWrite = agentStateWrite
    dstStmt.readVariables = readVariables
  }

  def localState: LocalState = a.stack.top.localState

  def agentLocalState: LocalState = a.stack.last.localState

  def localStateForRead: LocalState = {
    agentStateRead1 match {
      case true => a.localState
      case false => localState
    }
  }

  def localStateForAnotherRead: LocalState = {
    agentStateRead2 match {
      case true => agentLocalState
      case false => localState
    }
  }

  def localStateForYetAnotherRead: LocalState = {
    agentStateRead3 match {
      case true => agentLocalState
      case false => localState
    }
  }

  def localStateForWrite: LocalState = {
    agentStateWrite match {
      case true => agentLocalState
      case false => localState
    }
  }

  def callerLocalState: LocalState = {
    if (a.stack.size >= 2) // if it is <= 1 then it is called by the agent
      a.stack.tail.top.localState
    else
    // a.localState
      throw new Error("There is no caller, this is the agent talking here")
  }

  def skip: Unit = {
    while (action.hasMore && action.toExecute.head.associatedWithForSkipping == this)
      action.advancePC
  }

  //==============================
  def apply: Unit = {
    if (instrumented)
      isDynamic match {
        case true => generateDynamic
        case false => generateStatic
      }
    code(m, a)
  }

  override def generateDynamic = {
    this
  }

  override def generateStatic = {
    this
  }

  override def toString(): String = "A Generic Statement"

  def setAction(act: Action): Unit = {
    action = act
    setAgent(action.a)
  }

  override def setAgent(a: Agent) = this.a = a

  override def setMessage(m: Message) = this.m = m

  override def traceCopy: Statement = Statement.fromJson(toJson)

  override def toJson: JValue = {
    val mField = if (null == m) null else m.toJson
    val associatedWithStmt = if (associatedWith.id == id) JNothing
    else associatedWith.toJson
    val associatedWithStmtForSkipping = if (associatedWithForSkipping.id == id) JNothing
    else associatedWithForSkipping.toJson

    ("Statement" ->
      ("m" -> mField) ~
        ("startLineNo" -> JInt(startLineNo)) ~
        ("endLineNo" -> JInt(endLineNo)) ~
        ("fileName" -> fileName) ~
        ("id" -> id.toString) ~
        ("instrumented" -> instrumented) ~
        ("isDynamic" -> isDynamic) ~
        ("isFunctional" -> isFunctional) ~
        ("associatedWith" -> associatedWithStmt) ~
        ("associatedWithStmtForSkipping" -> associatedWithStmtForSkipping) ~
        ("agentStateRead1" -> agentStateRead1) ~
        ("agentStateRead2" -> agentStateRead2) ~
        ("agentStateWrite" -> agentStateWrite) ~
        ("readVariables" -> readVariables)
      )
  }

  def is(that: Statement): Boolean = id == that.id

}

object Statement {

  /**
    * Generic non-instrumented statement that is NOT one of our
    * distributed system's specification methods.
    *
    * @param code the code to execute, must NOT contain any DistributedSystem methods listed in Kind object.
    * @return
    */
  def apply(code: (Message, Agent) ⇒ Unit): Statement = {
    val stmt = new Statement
    stmt.code = code
    stmt.instrumented = false
    stmt.isDynamic = true
    stmt.isFunctional = true
    stmt
  }

  private def statementFromJson(json: JValue): Statement = {

    // STATEMENT
    implicit val formats = DefaultFormats
    //    val stmt = Statement(code)
    val stmt = Statement((m: Message, a: Agent) ⇒ {})

    //MESSAGE
    json \ "Statement" \ "m" match {
      case x: JObject ⇒ stmt.m = Message.fromJson(x)
      case null ⇒ stmt.m = null
      case _ ⇒
        println(json \ "Statement" \ "m");
        throw new Error("Statement.fromJson - Couldn't extract message")
    }

    // AGENT
    stmt.a = null

    // start Line No
    json \ "Statement" \ "startLineNo" match {
      case JInt(x) ⇒ stmt.startLineNo = x
      case _ ⇒
        println(json \ "Statement" \ "startLineNo")
        throw new Error("Statement.fromJson - Couldn't extract startLineNo")
    }

    // end line no
    json \ "Statement" \ "endLineNo" match {
      case JInt(x) ⇒ stmt.endLineNo = x
      case _ ⇒ throw new Error("Statement.fromJson - Couldn't extract endLineNo")
    }

    json \ "Statement" \ "fileName" match {
      case JString(x) ⇒ stmt.fileName = x
      case _ ⇒
        println(json \ "Statement" \ "fileName")
        throw new Error("Statement.fromJson - Couldn't extract fileName")
    }
    // ID
    json \ "Statement" \ "id" match {
      case JString(x) ⇒ stmt.id = UUID.fromString(x)
      case _ ⇒ throw new Error("Statement.fromJson - there can't be a statement without a UUID")
    }

    json \ "Statement" \ "instrumented" match {
      case JBool(x) ⇒ stmt.instrumented = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'instrumented'")
    }

    json \ "Statement" \ "isDynamic" match {
      case JBool(x) ⇒ stmt.isDynamic = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'isDynamic'")
    }

    json \ "Statement" \ "isFunctional" match {
      case JBool(x) ⇒ stmt.isFunctional = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'isFunctional'")
    }

    json \ "Statement" \ "associatedWith" match {
      case JNothing => stmt.associatedWith = stmt
      case x: JObject => Statement.fromJson(x)
      case _ => throw new Error("Statement.fromJson - couldn't extract the associatedWith field")
    }

    stmt.associatedWithForSkipping = stmt

    json \ "Statement" \ "agentStateRead1" match {
      case JBool(x) ⇒ stmt.agentStateRead1 = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'agentStateRead1'")
    }

    json \ "Statement" \ "agentStateRead2" match {
      case JBool(x) ⇒ stmt.agentStateRead2 = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'agentStateRead2'")
    }

    json \ "Statement" \ "agentStateWrite" match {
      case JBool(x) ⇒ stmt.agentStateWrite = x
      case _ ⇒ throw new Error("Statement.fromJson - can't extract 'agentStateWrite'")
    }

    json \ "Statement" \ "readVariables" match {
      case JArray(x) => stmt.readVariables = x.map {
        case JString(x) => x
        case _ => throw new Error("Statement.fromJSon - extracting 'readVariables' encountered a type error")
      }
      case _ => throw new Error("Statement.fromJson - can't extract 'readVariables'")
    }

    stmt
  }

  def fromJson(js: JValue): Statement = {
    var className = js match {
      case JObject(List(JField(x, _))) => x
      case _ =>
        //        println(js)
        throw new Error("Statement.fromJson -- can't determine the specific subclass name")
    }

    className match {
      case "Ask" => Ask.fromJson(js)
      case "Become" => Become.fromJson(js)
      case "BootStrap" => BootStrap.fromJson(js)
      case "Create" => Create.fromJson(js)
      case "Else" => Else.fromJson(js)
      case "ElseIf" => ElseIf.fromJson(js)
      case "Function" => Function.fromJson(js)
      case "Get" => Get.fromJson(js)
      case "If" => If.fromJson(js)
      case "Kill" => Kill.fromJson(js)
      case "Lock" => Lock.fromJson(js)
      case "ModifyState" => ModifyState.fromJson(js)
      case "ModifyStateRef" => ModifyStateRef.fromJson(js)
      case "ResumeConsume" => ResumeConsume.fromJson(js)
      case "Send" => Send.fromJson(js)
      case "Start" => StartStmt.fromJson(js)
      case "Stop" => StopStmt.fromJson(js)
      case "StopConsume" => StopConsume.fromJson(js)
      case "TimedGet" => TimedGet.fromJson(js)
      case "UnBecome" => UnBecome.fromJson(js)
      case "UnLock" => UnLock.fromJson(js)
      case "UnStash" => UnStash.fromJson(js)
      case "UnStashAll" => UnStashAll.fromJson(js)
      case "While" => While.fromJson(js)
      case "Return" => Return.fromJson(js)
      case "FunctionRecurse" => FunctionRecurse.fromJson(js)
      case _ => statementFromJson(js)
    }
  }
}
