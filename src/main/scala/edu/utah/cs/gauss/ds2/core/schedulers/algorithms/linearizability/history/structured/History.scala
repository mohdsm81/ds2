package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured

import edu.utah.cs.gauss.ds2.core.tracing.Trace
import net.liftweb.json._
import net.liftweb.json.JsonDSL._


/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

class History extends Trace {

  override def toJson: JValue = ("History" -> entries.map { x => x.toJson})

  override def copy: History = History.fromJson(toJson)

  //----------------------------------------
  // Linearizability-specific operations
  //----------------------------------------

  // TODO implement linearizability-specific methods

}

object History {
  def fromJson(js: JValue): History = {

    def auxClassName(js: JValue): String = {
      var auxClassName = ""

      js match {
        case JObject(List(JField("LinearizabilityInvocation", _))) =>
          auxClassName = "LinearizabilityInvocation"
        case JObject(List(JField("LinearizabilityResponse", _))) => 
          auxClassName = "LinearizabilityResponse"
        case _ => throw new Error("History.fromJson - can't extract 'auxClassName'")
      }
      auxClassName
    }

    val entries = js \ "History" match {
      case JArray(x) => x map {
        y => auxClassName(y) match {
          case "LinearizabilityInvocation" => LinearizabilityInvocation.fromJson(y)
          case "LinearizabilityResponse" => LinearizabilityResponse.fromJson(y)
          case _ => throw new Error("History.fromJson -- can't decide what 'auxClassName' fromJson method to call -- auxClassName = " + auxClassName(y))
        }
      }

      case _ => throw new Error("History.fromJson - can't extract 'entries'")
    }

    val history = new History
    history.entries = entries
    history
  }
}
