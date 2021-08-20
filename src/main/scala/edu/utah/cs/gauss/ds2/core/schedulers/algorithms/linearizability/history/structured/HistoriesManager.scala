package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured

import edu.utah.cs.gauss.ds2.core.tracing.TraceManager
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


class HistoriesManager extends TraceManager {
  
  // a trace manager should at least have ONE trace, otherwise no point
  this + new History
  
  override def toJson: JValue =  {
    (getClass.getSimpleName ->
      ("histories" -> traces.map { x => x.toJson }) ~
      ("curr" -> curr))
  }

  override def toJsonOne(whichTrace: Int): JValue = traces(whichTrace).toJson

  //----------------------------------------
  // Linearizability Specific methods
  //----------------------------------------

  //TODO implement linearizability-specific methods



}

object HistoriesManager {

  def fromJson(js: JValue): HistoriesManager = {
    implicit val formats = DefaultFormats
    //    js.extract[TraceManager]

    // println(pretty(render(js)))

    val traces = js \ "HistoriesManager" \ "histories" match {
      case JArray(x) => x map { y => History.fromJson(y) }
      case _         => throw new Error("HistoriesManager.fromJson - can't extract 'histories', js = " + js \ "HistoriesManager" \ "histories")
    }

    val curr = js \ "HistoriesManager" \ "curr" match {
      case JInt(x) => x
      case _       => throw new Error("HistoriesManager.fromJson - can't extract 'curr', js = " + js \ "HistoriesManager" \ "TraceManager" \ "curr")
    }

    val newOne = new HistoriesManager
    newOne.curr = curr.toInt
    newOne.traces = MSeq(traces: _*)
    newOne
  }


}

