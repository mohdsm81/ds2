package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */
class WGL2(override val history: Event, override val debug: Boolean = false, override val graphFilePathToEmit: String = "") extends WGL(history, debug, graphFilePathToEmit) {

  override protected def simulate(entry: Event): (Boolean, Boolean, State) = {
    require(entry.isInvocation, "Can't simulate a Response entry.")

    var isLinearizable = false

    var postStateClone = spec.clone
    val resp = entry.getMatch
    entry match {
      case x: Invocation =>

        if (resp != null) {
          if (resp.isInstanceOf[Response]) {
            val (isLin, _ , postState) = super.simulate(entry)
            isLinearizable = isLin
            postStateClone = postState
          }
          else x.getOperation.toLowerCase match { // handling a WildCardResponse

            case "add" | ":add" => // add to a Set
              postStateClone += (entry.getArgs.head -> entry.getArgs.head)
              isLinearizable = true

            case "remove" | ":remove" => // remove from a Set
              postStateClone -= entry.getArgs.head
              isLinearizable = true

            case "contains" | ":contains" => isLinearizable = true

            case "read" | ":read" => isLinearizable = true

            case "get" | ":get" => isLinearizable = true

            case "write" | ":write" =>
              postStateClone(entry.getKey) = entry.getArgs.head
              isLinearizable = true

            case "put" | ":put" =>
              postStateClone(entry.getKey) = entry.getArgs.head
              isLinearizable = true

            case "append" | ":append" =>
              if (!postStateClone.contains(entry.getKey)) postStateClone(entry.getKey) = entry.getArgs.head
              else postStateClone(entry.getKey) = postStateClone(entry.getKey).toString + entry.getArgs.mkString

              isLinearizable = true

            case "cas" | ":cas" if (resp.getArgs == entry.getArgs) =>
              val (isLin, _, post ) = super.simulate(entry)
              postStateClone = post
              isLinearizable = isLin

            case DUMMY_HEAD_NAME => throw new Error(s"WGL2.simulate - can't simulate a $DUMMY_HEAD_NAME")

            case _ =>
          } // opname match
        }

      //-------------------------------------------------------------------------------------------
      case _ => throw new Error("WGL2.simulate -- can't simulate but Invocations(calls)")
    } // entry match

    (isLinearizable, false, postStateClone)
  }
}
