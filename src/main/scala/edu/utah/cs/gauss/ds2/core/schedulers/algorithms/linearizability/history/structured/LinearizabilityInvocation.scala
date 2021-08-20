package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.tracing.{TraceEntry, TraceEvent}
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import java.util.UUID
import scala.math.BigInt.int2bigInt

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 *
 * Pre, event, post states of a distributed system is ONE TraceEntry.
 *
 */

class LinearizabilityInvocation(var requestNo: BigInt = -1) extends TraceEntry {

  override def toJson: JValue = {
    val pri = if (null == prior) null else prior.toJson
    val post = if (null == posterior) null else posterior.toJson

    ("LinearizabilityInvocation" ->
      ("requestNo" -> requestNo) ~
      ("id" -> id.toString) ~
      ("prior" -> pri) ~
      ("event" -> event.toJson) ~
      ("posterior" -> post))
  }

  override def copy: LinearizabilityInvocation = LinearizabilityInvocation.fromJson(toJson)

}

object LinearizabilityInvocation {
  def apply(te: TraceEvent): LinearizabilityInvocation = {
    val newOne = new LinearizabilityInvocation
    newOne.event = te
    newOne
  }

  def fromJson(js: JValue): LinearizabilityInvocation = {

    // DEBUG
    // println(pretty(render(js \ "LinearizabilityInvocation" \ "Entry" \ "LinearizabilityInvocation" \ "id" )))

    val newOne = new LinearizabilityInvocation

    js \ "LinearizabilityInvocation" \ "requestNo" match {
      case JInt(x) => newOne.requestNo = x
      case _ => throw new Error("LinearizabilityInvocation.fromJson -- can't extract 'requestNo' field")
    }

    js \ "LinearizabilityInvocation" \ "id" match {
      case JString(x) => newOne.id = UUID.fromString(x)
      case _ => throw new Error("LinearizabilityInvocation.fromJson -- can't extract 'id' field")
    }
    js \ "LinearizabilityInvocation" \ "prior" match {
      case null => newOne.prior = null
      case x: JObject => newOne.prior = DistributedSystem.fromJson(x)
      case _ => throw new Error("LinearizabilityInvocation.fromJson -- can't extract 'prior' field")
    }

    js \ "LinearizabilityInvocation" \ "event" match {
      case null => newOne.event = null
      case x: JObject => newOne.event = TraceEvent.fromJson(x)
      case _ => throw new Error("LinearizabilityInvocation.fromJson -- can't extract 'event' field")
    }

    js \ "LinearizabilityInvocation"  \ "posterior" match {
      case null => newOne.posterior = null
      case x: JObject => newOne.posterior = DistributedSystem.fromJson(x)
      case _ => throw new Error("LinearizabilityInvocation.fromJson -- can't extract 'posterior' field")
    }

    newOne
  }
}
