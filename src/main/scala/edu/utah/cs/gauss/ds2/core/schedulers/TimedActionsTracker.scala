package edu.utah.cs.gauss.ds2.core.schedulers

import scala.collection.parallel.ParSeq
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.state.{Snapshot, TimedActionsTrackerState}

import scala.collection.parallel.ParSet
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

/**
 * @author  	Mohammed S. Al-Mahfoudh <br/>
 * 		   	mahfoudh@cs.utah.edu <br/>
 * 		   	Gauss Group - SoC <br/>
 * 		   	The University of Utah <br/>
 *
 * Each scheduler should at least have one instance of this class to keep track of
 * periodically scheduled/timed actions, e.g. a heart beat.
 *
 * It provides facilities to simplify managing a bench of them at bulk not to bloat
 * schedulers' implementation(s)
 *
 * @param <TimedActionType> the type of the timed-action (see SimpleTypes, Two of
 * them so far and both are abstract representation of time using BigInt).
 */

@SerialVersionUID(1500)
class TimedActionsTracker extends Serializable with Snapshot[TimedActionsTrackerState]{

  var timedActions: ParSet[TimedAction] = ParSet()

  //==============================
  // Equality handling
  //==============================
  override def hashCode: Int = {
    timedActions.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }

  /**
   * Adds a syntactic timed action with the supporting runtime attributes.
   * @param ta the syntactic timed action
   * @param clock the scheduler's time at which this TimedAction was added.
   */
  def +(ta: TimedAction)(implicit clock: () => BigInt): TimedActionsTracker = {
    timedActions.synchronized {
      timedActions += ta
      this
    }
  }

  /**
   * Removes a syntactic TimedAction from the active Runtime-TimedActions
   * set, discarding all additional runtime attributes in the process.
   * @param ta the syntactic TimedAction whose runtime info to be discarded.
   */
  def -(ta: TimedAction): TimedActionsTracker = {
    timedActions.synchronized {
      timedActions = timedActions filterNot { x => x is ta }
      this
      // we don't care about other runtime attributes.
    }
  }

  /**
   * Advances the time for all TimedActions (by decrementing the count down),
   * returns a set of all timed actions whose Action needs be executed before next tick.
   * @return A set of all triggered timed actions (i.e. their count down reached 0)
   * for executing the Action associated with them on the agent's state, also
   * associated with them. None, otherwise.
   */
  def tick: Option[Set[TimedAction]] = {
    tick(1)
  }

  private def maintainHowManyTimes(x: TimedAction): Unit = {

    // dealing with how many times remaining to execute the action
    val y = x.runtimeHowManyTimes

    if (y > 0) // decrement and add to triggered
      x.runtimeHowManyTimes -= 1
    else if (y == 0) // remove from here
      this - x
    // else (y < 0) run forever do nothing
  }

  /**
   * Same as tick() with the addition of how many clock ticks
   * the scheduler has advanced.
   * @param n the number of ticks the scheduler time has already
   *  advanced
   * @return The set of all triggered timed action due to this. That is,
   * whose count down is greater than or equal to the startLimit of
   * their timed action.
   *
   */
  def tick(n: BigInt): Option[Set[TimedAction]] = {
    var triggered = Set[TimedAction]()
    timedActions.foreach { x =>
      val y = x.countDown

      if (y > 0 && x.runtimeHowManyTimes > 0) { // keep counting 
        x.countDown -= n
      }
      else if (y <= 0 && x.runtimeHowManyTimes > 0) // triggered
      {
        triggered = triggered + x
        x.reset // reset x's count down
        // Dealing with how many times
        maintainHowManyTimes(x)
      }
      else if (x.runtimeHowManyTimes == 0) {
        maintainHowManyTimes(x)
      }

      // else nothing happens
    }

    // dealing with return values
    if (triggered.isEmpty)
      None
    else
      Some(triggered)
  }

  def willTimeoutIn(numOfTicks: BigInt): Set[TimedAction] = {
    timedActions.filter { x => x.countDown <= numOfTicks }.toList.toSet
  }

  private def resetAll(): Unit = {
    timedActions.synchronized {
      timedActions map { x => x.reset }
    }
  }

  def traceCopy: TimedActionsTracker = {
    TimedActionsTracker.fromJson(toJson)
  }

  def toJson: JValue = {
    this.synchronized {
      (getClass.getSimpleName ->
        ("timedActions" -> timedActions.toList.map { x => x.toJson }.toSet))
    }
  }

  override def toString: String = {
    timedActions map { x => x.toString } mkString (",\n")
  }

  override def snapshot: TimedActionsTrackerState = TimedActionsTrackerState(this)
  override def restore(state: TimedActionsTrackerState): Unit = {
    state.instanceToRestore = this
    state.restore
  }
}

object TimedActionsTracker {
  def fromJson(js: JValue): TimedActionsTracker = {
    implicit val formats = DefaultFormats
    js.extract[TimedActionsTracker]
  }
}
