package edu.utah.cs.gauss.ds2.core.tracing

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import java.util.UUID

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 *
 * Pre, event, post states of a distributed system is ONE TraceEntry.
 *
 */

@SerialVersionUID(1700)
class TraceEntry extends Serializable {

  var id = UUID.randomUUID

  var prior: DistributedSystem = _
  var event: TraceEvent = _
  var posterior: DistributedSystem = _

  def toJson: JValue = {

    val pri = if (null == prior) null else prior.toJson
    val post = if (null == posterior) null else posterior.toJson

    (getClass.getSimpleName ->
      ("id" -> id.toString) ~
      ("prior" -> pri) ~
      ("event" -> event.toJson) ~
      ("posterior" -> post))
  }

  def is(that: TraceEntry): Boolean = {
    if (null != that) id == that.id else false
  }
  
  def copy: TraceEntry = {
    // no need to cross reference, it is a trace thing frozen in time now.
    TraceEntry.fromJson(toJson)
  }

}

object TraceEntry {

  def fromJson(js: JValue): TraceEntry =
    {
      var auxClassName = ""
      
      val te = js match {
        case JObject(List(JField("TraceEntry", _))) =>
          auxClassName = "TraceEntry"; new TraceEntry
        case JObject(List(JField("TraceEntryEnd", _))) => 
          auxClassName = "TraceEntryEnd"; new TraceEntryEnd
        case _ => throw new Error("TraceEntry.fromJson - can't extract 'auxClassName'")
      }


      val priJson = js \ auxClassName \ "prior"
      
      val postJson = js \ auxClassName \ "posterior"
      
      val eventJson = js \ auxClassName \ "event"
      
      val idJson = js \ auxClassName \ "id"

      val prior = priJson match {
        case null       => null
        case JNothing   => null
        case x: JObject => DistributedSystem.fromJson(x)
        case _          => throw new Error("TraceEntry.fromJson - can't extract 'prior'")
      }

      val posterior = postJson match {
        case null       => null
        case JNothing   => null
        case x: JObject => DistributedSystem.fromJson(x)
        case _          => throw new Error("TraceEntry.fromJson - can't extract 'posterior'")
      }

      val event = eventJson match {
        case x: JObject => TraceEvent.fromJson(eventJson)
        case _          => throw new Error("TraceEntry.fromJson - can't extract 'event'")
      }

      val id = idJson match {
        case JString(x) => UUID.fromString(x)
        case _          => throw new Error("TraceEntry.fromJson - can't extract 'id'")
      }

      auxClassName match {
        case "TraceEntryEnd" =>
          te.event = event
          te.id = id
          te
        case "TraceEntry" =>
          te.prior = prior
          te.posterior = posterior
          te.event = event
          te.id = id
          te
        case _ => throw new Error("TraceEntry.fromJson - unknown 'auxClassName [" + auxClassName + "]")
      }

    }

}