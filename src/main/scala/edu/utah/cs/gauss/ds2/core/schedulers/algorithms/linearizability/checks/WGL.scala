package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import edu.utah.cs.gauss.serialization.IO._
import edu.utah.cs.gauss.serialization._

import scala.collection.mutable
import scala.collection.mutable.{Map => MMap, Set => MSet}

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */
trait WGLState {


  type State = MMap[Any, Any]
  //  type CacheEntry = (BitSet, State)
  type CacheEntry = (mutable.BitSet, State) // hash of state instead of the state-copy!
  var spec: State = MMap[Any, Any]()
  val modificationAPI: Set[String] = Set(":append", ":write", ":put", ":cas") union Set("add", "remove")

  // check: Boolean method state
  // I could have made it specific to Invocation (that is calls), but we need it to track CausalChains too
  val calls: mutable.Stack[(History, State)] = mutable.Stack[(Event, State)]() // initially empty
  val cache: MSet[(mutable.BitSet, State)] = MSet[CacheEntry]() // initially empty
  val linearized: mutable.BitSet = mutable.BitSet()
}

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */
//case class PCompositionalityCheck(history: Event, dataStructure: DataStruct, debug: Boolean = false) extends PCompoAlgState {
case class WGL(history: Event, debug: Boolean = false, graphFilePathToEmit: String = "") extends WGLState {
    require(history != null && history.size >= 2, "The history provided is shorter than to be checked by this algorithm")
  history.reNumber // re-assigning in order IDs

  val padding: HistoryTypes.History = getPadding
  insertBefore(history, padding)
  var headEntry: History = padding

  // DEBUG support
  implicit var count: Int = 0
  var startIteration: Int = 0
  var endIteration: Int = Int.MaxValue
  var fileToOutput: String = ""


  //  def trimQoutes(str: String): String = {
  //    var result = ""
  //    if(str == null || str.isEmpty) return result
  //    else result = str.filterNot(_ == '\"')
  //    result
  //  }

  //----------------------------------------
  // For optimized version
  //----------------------------------------
  val map: MMap[Int, flat.HistoryTypes.History] = computeMatchMap

  def matches(event: Event): Event = {

    if (event.isInvocation && map.contains(event.getID))
      map(event.getID)
    else
      null
  }

  def computeMatchMap: MMap[Int, Event] = {

    var event: Event = headEntry.getNext
    val map = MMap[Int, Event]()
    //    map(-1) = null // match of anything else is null (

    while (event != null) {
      if (event.isInvocation) {
        val theMatch: Event = matchOf(event)
        map(event.getID) = theMatch
      }
      event = event.getNext
    }

    map
  }


  /**
   * This is the "apply(entry,s): (is_linearized,s')" in the paper (the wgl alg specifically)
   *
   * Except that the second argument is removed as it is made a global mutable
   * state called 'spec' and naturally no need to return the new state too.
   *
   * The extra argument is called "shortCircuite" is specific to CALC and how it linearizes
   * causal chains. It is not used here.
   */
  protected def simulate(entry: Event): (Boolean, Boolean, State) = {
    require(entry.isInvocation, "Can't simulate a Response entry.")

    var isLinearizable = false
    // val resp = matches(entry.getID)
    //    val resp = matches(entry)
    val postStateClone = spec.clone
    val resp = entry.getMatch
    entry match {
      case x: Invocation =>

        if (resp != null) {
          x.getOperation.toLowerCase match {

            case "add" | ":add" => // add to a Set
              postStateClone += (entry.getArgs.head -> entry.getArgs.head)
              isLinearizable = true

            case "remove" | ":remove" => // remove from a Set
              postStateClone -= entry.getArgs.head
              isLinearizable = true

            case "contains" | ":contains" =>
              isLinearizable = resp.getArgs.head == "true" &&
                postStateClone.contains(entry.getArgs.head) &&
                postStateClone(entry.getArgs.head).toString == resp.getArgs.head

            case "read" | ":read" =>
              if (!postStateClone.contains(entry.getKey) && resp.getArgs.head == "") {
                isLinearizable = true
              }
              else {
                val respArgs = resp.getArgs.head
                val stateContent = if (postStateClone.contains(entry.getKey)) postStateClone(entry.getKey).toString else ""
                isLinearizable = stateContent.equals(respArgs)
              }

            case "get" | ":get" =>
              if (!postStateClone.contains(entry.getKey) && resp.getArgs.head == "") {
                isLinearizable = true
              }
              else {
                val respArgs = resp.getArgs.head
                val stateContent = if (postStateClone.contains(entry.getKey)) postStateClone(entry.getKey).toString else ""
                isLinearizable = stateContent == respArgs
              }

            case "write" | ":write" =>
              postStateClone(entry.getKey) = resp.getArgs.head
              isLinearizable = true

            case "put" | ":put" =>
              postStateClone(entry.getKey) = resp.getArgs.head
              isLinearizable = true

            case "append" | ":append" =>
              if (!postStateClone.contains(entry.getKey) && entry.getArgs.head == resp.getArgs.head)
                postStateClone(entry.getKey) = resp.getArgs.head
               else
                postStateClone(entry.getKey) = postStateClone(entry.getKey).toString + resp.getArgs.mkString
              isLinearizable = true

            case "cas" | ":cas" if resp.getArgs == entry.getArgs =>

              val expected = entry.getArgs.head
              val intended = entry.getArgs.tail.head
              val responseVal = resp.getArgs.head

              val k = entry.getKey match {
                case None => None // FIXME is it None or null? check this otherwise it will mark some lin histories as non-lin
                case Some(x) => x
              }

              // retVal -> outcome of this invocation on the current state of the model.
              // respVal -> out of this invocation in the real system that was recorded in the response in history for this invocation
              // CAS
              var retVal = false
              if (postStateClone.contains()) {
                val current = postStateClone(k)
                if (current == expected) {
                  retVal = true
                  postStateClone(k) = intended
                }
              }

              isLinearizable = retVal.toString == responseVal

            case DUMMY_HEAD_NAME => throw new Error(s"WGL.simulate - can't simulate a $DUMMY_HEAD_NAME")

            case _ =>
          } // opname match
        }

      //-------------------------------------------------------------------------------------------
      case _ => throw new Error("WGL.simulate -- can't simulate but Invocations(calls)")
    } // entry match

    (isLinearizable, false, postStateClone)
  }

  def debugFilter(implicit count: Int): Boolean = {
    if (debug || fileToOutput != "")
      count >= startIteration && count < endIteration
    else
      false
  }

  def preProcessKeyString(tuple: (Any, Any)): String = {
    tuple._2.toString
  }

  //========================================
  // check (the algorithm)
  //========================================
  def check: Boolean = {

    if (fileToOutput != "") deleteFile(fileToOutput)

    val histEntriesNum = 16
    // GRAPH support
    var microCount = 0
    var transitions = MSet[MyEdge]()

    // val lowerLimit = 1000 // starts graphing here. Good one: 800 (wrong appends).
    // val upperLimit = lowerLimit + 35 //+ 32 // + 12 // stops graphing here: 100 (wrong appends).
    def graphFilter = count >= startIteration && graphFilePathToEmit != ""

    if (debug)
      println(s"History original size = ${headEntry.size}")
    var skipped = 0
    var advanced = 0
    var backtracked = 0

    var entry: Event = headEntry.getNext

    while (headEntry.getNext != null) { // i.e. while there are still entries in the history other than the handel
      count += 1
      //      if (debugFilter) // I disabled it for now :)
      //        println("count = " + count)

      if (debug)
        println(s"History remaining Size = ${entry.size}")

      //      // --------------- Graph CODE ------------------
      //      val label = MyLabel(entry.copy)
      //      val node1 = MyNode(spec.clone)
      //      // --------------- DEBUG CODE ------------------
      //      if (debugFilter && count >= endIteration)
      //        System.exit(0)
      //      // --------------- END DEBUG CODE ---------------

      if (entry.isInvocation) { // is call entry?

        val (isLinearizable, shortCircuit, postStateClone) = simulate(entry) // simulate entry's operation

        if (shortCircuit) return false

        //        label.isLinearizable = isLinearizable

        if (debugFilter) {
          if (fileToOutput != "")
            toFile(entry)
          else
            toStdOut(entry, postStateClone, isLinearizable)
        }

        val cachePrime = cache.clone

        if (isLinearizable) {
          //          val linearizedPrime = linearized.clone // copy bitset
          //          linearizedPrime += entry.getID // insert entry_id(entry) into bitset
          cache += ((linearized.clone += entry.getID) -> postStateClone) // Update Config Cache
        }

        if (cachePrime != cache) {
          // note, this doesn't need to be cloned, because the post-state is a clone of it (reducing Garbage Collection pressure).
          calls.push((entry, spec)) // Provisionally linearize call entry and state
          spec = postStateClone
          linearized += entry.getID // keep track of linearized entries
          lift(entry) // Provisionally remove the entry from the history
          entry = headEntry.getNext
          skipped = 0

          if (debug) {
            println("cache changed")
            println(s"Start from head, ${headEntry.size} entries to explore")
          }
        }
        else // cache didn't change (non linearizable entry!)
        {
          if (debug)
            println("no cache change")
          advanced += 1
          entry = entry.getNext
        }

      } // end of if-Inv
      else { // handle "return entry"
        if (calls.isEmpty) {
          // if(debugFilter) appendToFile(fileToOutput, "Not Linearizable")

          return false // can not linearize calls in history (because it started with a response
        }

        // revert to earlier state (backtracking)
        val (e, s) = calls.top
        entry = e
        spec = s

        linearized -= entry.getID // resetting entry.id bit

        calls.pop()

        unlift(entry) // Undo provisional linearization

        // if (debugFilter) appendToFile(fileToOutput, "Reverted entry: " + entry)

        entry = entry.getNext

        if (debug) {
          advanced += 1
          println(s"Skipped = $skipped")
          println(s"Advanced = $advanced")
          println(s"State became = \n${spec.mkString("\n")}")
          println(s"Entry to explore = ${entry.toPorcupineEntry}")
        }
      } // top-else

      //      // GRAPH support
      //      var node2: MyNode = null
      //      if (count >= startIteration && graphFilePathToEmit != "") {
      //        // println("===========> HERE!")
      //        microCount = microCount + 1
      //        label.optionalSeqNo = microCount
      //
      //        node2 = MyNode(spec.clone)
      //        transitions.add(MyEdge(node1, label, node2))
      //      }
      //
      //      if (count == endIteration && graphFilePathToEmit != "") {
      //        println("Finally! Graph is output")
      //        val buffer: Buffer[MyEdge] = Buffer()
      //        buffer ++= transitions
      //        printGraphToFile(makeGraph(buffer), graphFilePathToEmit)
      //        compileDotToPDF(graphFilePathToEmit)
      //      }
    } // while

    // if (debugFilter) appendToFile(fileToOutput, "Linearizable")
    true
  }

  //noinspection AccessorLikeMethodIsUnit
  private def toFile(entry: Event): Unit = {
    // appendToFile(fileToOutput, s"\n\n\nEntry = $entry")
    // appendToFile(fileToOutput, s"spec = ${spec.mkString("\n")}")
    appendToFile(fileToOutput, spec.map {
      preProcessKeyString
    }.mkString)
  }

  //noinspection AccessorLikeMethodIsUnit
  private def toStdOut(entry: Event, state: MMap[Any, Any], isLin: Boolean): Unit = {
    println("-----------------------------------------------------------------------------------------------------------")
    println(s"Entry(${entry.getID}) = ${entry.toPorcupineEntry}")
    println(s"Match(${entry.getID}) = ${matchOf(entry).toPorcupineEntry}")
    println(s"isLinearizable = $isLin\n")
    println(s"State = \n${state.mkString("\n")}\n")
    //    println(s"TYPES ${(entry.getClass.getSimpleName,entry.getMatch.getClass.getSimpleName)}")
    //    println(s"OPERATIONs ${(entry.getOperation,entry.getMatch.getOperation)}")
    //    println(s"KEYs ${(entry.getKey,entry.getMatch.getKey)}")
    //    println(s"ARGs ${(entry.getArgs.head,entry.getMatch.getArgs.head)}")
    //    println(s"PIDs ${(entry.getPID,entry.getMatch.getPID)}")
    //    println(s"IDs ${(entry.getID,entry.getMatch.getID)}")
    //    println(s"headEntry.take(few) \n${if(headEntry.getNext != null) headEntry.getNext.take(7) else "EMPTY"}")
    println(s"To explore next (if not lifted): \n${if (entry.getNext != null) entry.getNext.take(12) else "EMPTY"}")
    println("-----------------------------------------------------------------------------------------------------------")
  }

  //========================================
  // Optimized check
  //========================================
}
