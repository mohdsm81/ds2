package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.structured
import edu.utah.cs.gauss.ds2.core.tracing.TraceEvent
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.math.BigInt.int2bigInt


/**
 * @author  	Mohammed S. Al-Mahfoudh <br/>
 *		   	mahfoudh@cs.utah.edu <br/>
 *		   	Gauss Group - SoC <br/>
 *		   	The University of Utah <br/>
 * 
 * Marker event entry to mark the end of a lengthy trace entry with nested entries.
 */
class LinearizabilityResponse(var requestNum: BigInt) extends LinearizabilityInvocation(requestNum)
{
  override def toJson: JValue = super.toJson
}

object LinearizabilityResponse {
  def apply(invocation: LinearizabilityInvocation): LinearizabilityResponse = {
    val response = new LinearizabilityResponse(invocation.requestNo)
    response.event = invocation.event
    response
  }
  
  def apply(invocationEvent: TraceEvent): LinearizabilityResponse = {
    val tee = new LinearizabilityResponse(-1)
    tee.event = invocationEvent
    tee
  }

  def fromJson(js: JValue): LinearizabilityResponse  = {

    val newOne = new LinearizabilityResponse(-1)

    js \ "LinearizabilityResponse" \ "requestNo" match {
      case JInt(x) => newOne.requestNo = x
      case _ => throw new Error("LinearizabilityResponse.fromJson -- can't extract 'requestNo' field")
    }

    js \ "LinearizabilityResponse" \ "Entry" \ "LinearizabilityResponse" \ "event" match {
      case null => newOne.event = null
      case x:JObject => newOne.event = TraceEvent.fromJson(x)
      case _ => throw new Error("LinearizabilityResponse.fromJson -- can't extract 'event' field")        
    }    
    newOne
  }

}
