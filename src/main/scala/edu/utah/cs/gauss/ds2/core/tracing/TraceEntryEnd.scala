package edu.utah.cs.gauss.ds2.core.tracing
import net.liftweb.json._
import net.liftweb.json.JsonDSL._



/**
 * @author  	Mohammed S. Al-Mahfoudh <br/>
 *		   	mahfoudh@cs.utah.edu <br/>
 *		   	Gauss Group - SoC <br/>
 *		   	The University of Utah <br/>
 * 
 * Marker event entry to mark the end of a lengthy trace entry with nested entries.
 */
@SerialVersionUID(1800)
class TraceEntryEnd extends TraceEntry
//{
//  override def toJson: JValue = {
//     ("TraceEntryEnd" ->
//    ("Entry" -> super.toJson))
//  }
//}

object TraceEntryEnd {
  def apply(te: TraceEntry): TraceEntryEnd = {
    val tee = new TraceEntryEnd
    tee.event = te.event
    tee
  }
  
  def apply(ev: TraceEvent): TraceEntryEnd = {
    val tee = new TraceEntryEnd
    tee.event = ev
    tee
  }

//  def fromJson(js: JValue): TraceEntryEnd  = {
//    val newOne = new TraceEntryEnd
//    val parent = TraceEntry.fromJson(js \ "TraceEntryEnd" \ "Entry")
//
//    newOne.id = parent.id
//    newOne.prior = parent.prior
//    newOne.event = parent.event
//    newOne.posterior = parent.posterior
//    
//    newOne
//  }
}
