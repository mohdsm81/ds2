package edu.utah.cs.gauss.ds2.core.tracing
import scala.collection.mutable.{ Seq => MSeq }
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 *
 * Keeps track of multiple traces and
 *
 */

@SerialVersionUID(2000)
class TraceManager extends Serializable {
  var traces = Seq[Trace]() 
  
  // a trace manager should at least have ONE trace, otherwise no point
  this + new Trace
  
  var curr = 0

  def atBeginning: Boolean = {
    curr == 0
  }

  def hasNext: Boolean = {
    curr < traces.size
  }

  def current: Trace = {
    require(!traces.isEmpty && curr < traces.size, "Current method - current is out of bounds")
    traces(curr)
  }

  def next: Unit = {
    require(!traces.isEmpty, "Next method - traces is empty")
    curr = curr + 1
  }

  def previous: Trace = {
    require(!traces.isEmpty, "Previous method - traces is empty")
    if (curr - 1 >= 0)
      traces(curr - 1)
    else
      traces(0)
  }

  def +(tr: Trace): Unit = {
    traces = traces :+ tr
  }

  def reset: Unit = {
    curr = 0
  }

  def clear: Unit = {
    reset
    traces = MSeq[Trace](new Trace)
  }
  def toJson: JValue = {
    (getClass.getSimpleName ->
      ("traces" -> traces.map { x => x.toJson }) ~
      ("curr" -> curr))
  }
  def diffs: Seq[Diff] = {
    require(traces.size >= 2, "TraceManager.diffs - can't find differences for less than 2 traces!")
    var diffs = Seq[Diff]()
    var start = 0
    for (trace <- traces.tail) {
      if (start + 1 < traces.size) {
        val next = start + 1
        diffs = diffs :+ diffTwo(start, next)
        start = next
      }
    }
    diffs
  }

  def toJsonOne(whichTrace: Int): JValue = {
    traces(whichTrace).toJson
  }

  def diffTwo(first: Int, second: Int): Diff = {
    require(0 <= first && first < traces.size, "TraceManager.diffTwo - first index is out of range")
    require(0 <= second && second < traces.size, "TraceManager.diffTwo - second index is out of range")
    require(first != second, "TraceManager.diffTwo - there is diffinately no difference in the same trace!")
    val f = traces(first).toJson
    val s = traces(second).toJson
    f diff s
  }

}

object TraceManager {
  def fromJson(js: JValue): TraceManager = {
    implicit val formats = DefaultFormats
    //    js.extract[TraceManager]

    val traces = js \ "TraceManager" \ "traces" match {
      case JArray(x) => x map { y => Trace.fromJson(y) }
      case _         => throw new Error("TraceManager.fromJson - can't extract 'traces', js = " + js \ "TraceManager" \ "traces")
    }

    val curr = js \ "TraceManager" \ "curr" match {
      case JInt(x) => x
      case _       => throw new Error("TraceManager.fromJson - can't extract 'curr', js = " + js \ "TraceManager" \ "curr")
    }

    val tm = new TraceManager
    tm.traces = MSeq(traces: _*)
    tm.curr = curr.toInt
    tm

  }
}
