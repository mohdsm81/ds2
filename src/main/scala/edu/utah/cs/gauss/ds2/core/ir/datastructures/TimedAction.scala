package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.Searchable
import edu.utah.cs.gauss.ds2.core.schedulers.TimedActionsTracker
import edu.utah.cs.gauss.ds2.core.state.{Snapshot, TimedActionState}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/**
 * @author <br>
 * Mohammed S. Al-Mahfoudh <br/>
 * mahfoudh@cs.utah.edu <br/>
 * SoC - Gauss Group <br/>
 *
 * While TimedActions are perfectly good for compile-time representation,
 * we need more information about TimedActions above during runtime. e.g. if
 * they timed out w.r.t. when they were started, ...etc.
 *
 * This is the class that the scheduler use to actually act on timed actions.
 */

@SerialVersionUID(800)
case class TimedAction(
    var startLimit: BigInt,
    var endLimit: BigInt,
    var action: Action,
//    var agent: Agent,
    var howManyTimes: BigInt,
    var submittedClock: BigInt) extends Serializable with Searchable with Snapshot[TimedActionState]{
  var countDown = startLimit // every how many ticks this action is triggered
  var runtimeHowManyTimes: BigInt = howManyTimes // how many times remaining this action runs

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {

    startLimit.hashCode +
      endLimit.hashCode +
      action.hashCode +
      howManyTimes.hashCode +
      submittedClock.hashCode +
      countDown.hashCode +
      runtimeHowManyTimes.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }
  //==============================

  def reset: Unit = {
      countDown = startLimit
  }

  def toJson: JValue = {
//      agentName = if (null == agent) null else agent.name

      ("RuntimeTimedAction" ->
        ("startLimit" -> startLimit) ~
        ("endLimit" -> endLimit) ~
        ("action" -> action.toJson) ~
        ("agent" -> null) ~
        ("howManyTimes" -> howManyTimes) ~
        ("submittedClock" -> submittedClock) ~
        ("countDown" -> countDown) ~
        ("runtimeHowManyTimes" -> runtimeHowManyTimes))
  }

  def copy: TimedAction = {

    val ta = new TimedAction(
      startLimit,
      endLimit,
      action, // statements inside this action will link themselves just before executing (based on the 'ds' field)
      howManyTimes,
      submittedClock)
    ta.countDown = countDown
    ta.runtimeHowManyTimes = runtimeHowManyTimes
    ta
  }

  def link(ds: DistributedSystem): Unit = {} // just like SuspendableTask, nothing to do

  def traceCopy: TimedAction = {
    TimedAction.fromJson(toJson)
  }

  def in(rttas: Set[TimedAction]): Boolean = {
    require(rttas != null, "RuntimeTimedAction.in(rttActions) method - doesn't accept null arguments")
    rttas.exists(x => x == this)
  }

  def in(tat: TimedActionsTracker): Boolean = {
    require(null != tat, "RuntimeTimedAction.in(TimedActionsTracker) method - doesn't accept null arguments")

    // why the circus? to convert from parallel collection to sequential one.
    this.in(tat.timedActions.toList.toSet)
  }

  def is(that: TimedAction): Boolean = {

//    val agentCondition = if (null == agent) null == that.agent else agent is that.agent

    startLimit == that.startLimit &&
      endLimit == that.endLimit &&
      (action is that.action) &&
//      agentCondition &&
      howManyTimes == that.howManyTimes

    // the rest of attributes are just runtime attributes.
  }

  override def toString: String = {

      s"TimedAction ---> from ${action.a.name}"

    //    val agentStr = if(null == agent) null else agent.name
//    agentName = if (null == agent) null else agent.name
//    "start limit = " + startLimit + "\n" +
//      "end limit = " + endLimit + "\n" +
//      "action = " + action.toExecute.size + " Statement(s)" + "\n" +
//      "agent = " + agentName + "\n" +
//      "how many times = " + howManyTimes + "\n" +
//      "time submitted = " + submittedClock + "\n" +
//      "count down = " + countDown + "\n" +
//      "runtime how many = " + runtimeHowManyTimes + "\n"
  }

  override def snapshot: TimedActionState = TimedActionState(this)

  override def restore(state: TimedActionState): Unit = {
    state.instanceToRestore = this
    state.restore
  }

  /**
   * Searches the stmt that is unliked (provided as arg) and returnes
   * the linked version of it.
   *
   * @param unlinkedStmtToSearch the statement that is not linked,
   *                             which needs to be replaced by the returned statement
   * @return the linked statement that will replace the unlinked one.
   */
  override def search(unlinkedStmtToSearch: Statement): Option[Statement] = action.search(unlinkedStmtToSearch)
}

object TimedAction {
  def fromJson(js: JValue): TimedAction = {
    implicit val formats = DefaultFormats
    //    js.extract[RuntimeTimedAction]

    val tajs = js \ "RuntimeTimedAction"

    val startLimit = tajs \ "startLimit" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'startLimit'")
    }

    val endLimit = tajs \ "endLimit" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'endLimit'")
    }

    val action = tajs \ "action" match {
      case x: JObject => Action.fromJson(x)
      case _          => throw new Error("RuntimeTimedAction.fromJson - can't extract 'action'")
    }

    val howManyTimes = tajs \ "howManyTimes" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'howManyTimes'")
    }

    val submittedClock = tajs \ "submittedClock" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'submittedClock'")
    }

    val countdown = tajs \ "countDown" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'countDown'")
    }

    val runtimeHowManyTimes = tajs \ "runtimeHowManyTimes" match {
      case JInt(x) => x
      case _       => throw new Error("RuntimeTimedAction.fromJson - can't extract 'runtimeHowManyTimes'")
    }

    // set the values in the returned action
    val ta = new TimedAction(
      startLimit,
      endLimit,
      action,
      howManyTimes,
      submittedClock)
    ta.countDown = countdown
    ta.runtimeHowManyTimes = runtimeHowManyTimes

    ta
  }
}
