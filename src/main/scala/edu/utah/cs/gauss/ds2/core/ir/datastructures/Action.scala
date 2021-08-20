package edu.utah.cs.gauss.ds2.core.ir.datastructures

import java.util.UUID

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.Searchable
import net.liftweb.json.{DefaultFormats, JArray, JString, JValue}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue, seq2jvalue, string2jvalue}

import scala.language.postfixOps
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 *
 * Note that this Action gets pupolated by the front-end with
 * send/ask calls as well as control flow statements.
 *
 * 
 */
@SerialVersionUID(100)
case class Action() extends Serializable with Searchable { //with CopyLink[Action] with ControlFlowLinking{

  // read only payload and message to act upon, includes the sender
  var m: Message = new Message("dummy TWO VR Sets please")

  // the receiver, the agent whose state may change upon executing this action
  var a: Agent = _

  // original stmts seq non-modifiable
  var stmts = Seq[Statement]()

  // mutable sequences of statements to represent program counter

  var executed = Seq[Statement]()
  var toExecute = Seq[Statement]()

  var id = UUID.randomUUID

  reset

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {

    val mHash = m match {
      case null ⇒ 0
      case _ ⇒ m.hashCode
    }

    //    agent isn't terminal, so I will avoid cyclic calls if removed from hashcode
    //    val aHash = if(null == a) 0 else a.hashCode

    mHash +
      stmts.hashCode +
      executed.hashCode +
      toExecute.hashCode +
      id.hashCode
  }

  override def equals(that: Any): Boolean = hashCode == that.hashCode

  def execute: Unit = {
    // all statements as one transaction
    while (hasMore)
      executeOne
  }

  /**
   * If action has more statements to execute, returns <code>true</code>. Otherwise, returns <code>false</code>
   *
   * @return <code>true</code> if there are more statements to execute, <code>false</code> otherwise
   */
  def hasMore: Boolean = {
    !toExecute.isEmpty
  }

  /**
   * Executes one statement of the action only, then stops. This method
   * needs to be called as many times as there are statements in the
   * Action.
   */
  def executeOne: Unit = {
    // no more than ONE stmt from this action to execute at a time
    /*
     * 1- call the stmt function on the parameters
     * 2- advance the program counter
     */
    toExecute.head.apply
    advancePC
  }

  /**
   * Careful NOT to use this one in any situation. This is only made public
   * to enable the basic scheduler's implementation.
   *
   * Before using this one, make sure you REALLY need to use it and there is
   * no way to "skip" the statement execution sitting in front of the <code>toExecute</code>
   * queue.
   */
  def advancePC: Unit = {
    if (!toExecute.isEmpty) {
      executed = executed :+ toExecute.head
      toExecute = toExecute.tail
    }
  }

  /**
   * Resets the <code>executed</code> and <code>toExecute</code>, but not <code>stmts</code>.
   *
   * This is a runtime reset, not a a compile time one. Good for simulating an agent crashing
   * in the middle of processing things, then restarting and executing again.
   */
  def reset: Unit = {
    executed = Seq[Statement]()
    toExecute = stmts // map { x ⇒ x }
    setAgent(a)
    setMessage(m)
    setAction
  }

  def +(stmt: Statement): Action = {
    stmts = stmts :+ stmt
    reset
    this
  }

  def ++(stmts: Seq[Statement]): Action = {
    stmts map { x ⇒ this + x }
    this
  }

  /**
   * Removes the last statement if there is any from the action.
   *
   * @return the action without the last statement.
   */
  def -(): Action = {
    if (!stmts.isEmpty)
      stmts = stmts.slice(0, stmts.size - 1)

    this
  }

  def current: Statement = {
    toExecute.head
  }

  //=============================
  // runtime copy is the only one remaining
  //=============================
  /**
   * Used to get a copy of the "prototypes" defined in reactions. (please see the prototype
   * object-oriented design pattern to learn more). This method is critical for
   * suspending(blocking) then resuming an action. Without it, it is not possible to block
   * then resume an action.
   *
   * @return the copy of the current action along with its current state
   */
  def runtimeCopy: Action = {

    val newAc = new Action
    // Message is a constant anyways but will copy
    newAc.m = m

    // referenced since doesn't change since creation
    newAc.executed = executed //.map { x ⇒ x }
    newAc.toExecute = toExecute //.map { x ⇒ x }
    newAc.stmts = stmts //.map { x ⇒ x }
    newAc.id = id
    newAc.setAction
    newAc.setMessage(m)
    newAc.setAgent(a)

    newAc
  }

  //=============================
  // Tracing support
  //=============================

  def traceCopy: Action = Action.fromJson(toJson)

  def toJson: JValue = {
    val mField = m match {
      case null ⇒ null
      case _ ⇒ m.toJson
    }
    //    val mField = if (null == m) null else m.toJson
    //    val aName = if (null == a) null else a.name
    val aName = a match {
      case null ⇒ null
      case _ ⇒ a.name
    }

    ("Action" ->
      ("id" -> id.toString) ~
        ("m" -> mField) ~
        ("a" -> null) ~
        ("stmts" -> stmts.map { x ⇒ x.toJson }) ~
        ("executed" -> executed.map { x ⇒ x.toJson }) ~
        ("toExecute" -> toExecute.map { x ⇒ x.toJson }))
  }

  //=============================
  // Utility methods
  //=============================


  def is(that: Action): Boolean = id.equals(that.id)


  def setAgent(a: Agent): Unit = {
    this.a = a
    stmts.map { x ⇒ x.setAgent(a) } // toExecute, executed both reference this one
  }

  def setMessage(m: Message): Unit = {
    this.m = m
    stmts.map { x ⇒ x.setMessage(m) }
  }

  def setAction: Unit = stmts.map { x ⇒ x.setAction(this) }

  override def toString: String = {
    s"ACTION: ${toExecute.size} stmts, id = ${id}"
  }

  /**
   * Searches the stmt that is unliked (provided as arg) and returnes
   * the linked version of it.
   *
   * @param unlinkedStmtToSearch the statement that is not linked,
   *                             which needs to be replaced by the returned statement
   * @return the linked statement that will replace the unlinked one.
   */
  override def search(unlinkedStmtToSearch: Statement): Option[Statement] = stmts.find { x => x.search(unlinkedStmtToSearch).isDefined }
}

object Action {

  //  def apply: Action = {
  //    new Action
  //  }

  def fromJson(js: JValue): Action = {
    implicit val formats = DefaultFormats
    //    val act = js.extract[Action]
    val act = new Action

    act.id = js \ "Action" \ "id" match {
      case JString(x) ⇒ UUID.fromString(x)
      case _ ⇒ throw new Error("Action.fromJson - can't extract 'id'")
    }

    js \ "Action" \ "m" match {
      case null ⇒ act.m = null
      case x ⇒ act.m = Message.fromJson(x)
    }

    act.a = null // we can restore it from the completely restored DS by using its name

    js \ "Action" \ "stmts" match {
      case JArray(x) ⇒ act.stmts = x map { x ⇒ Statement.fromJson(x) }
      case _ ⇒ throw new Error("Action.fromJson - can't extract 'stmts'")
    }

    js \ "Action" \ "toExecute" match {
      case JArray(x) ⇒ act.toExecute = x map { x ⇒ Statement.fromJson(x) }
      case _ ⇒ throw new Error("Action.fromJson - can't extract 'stmts'")
    }

    js \ "Action" \ "executed" match {
      case JArray(x) ⇒ act.executed = x map { x ⇒ Statement.fromJson(x) }
      case _ ⇒ throw new Error("Action.fromJson - can't extract 'stmts'")
    }

    act
  }
}
