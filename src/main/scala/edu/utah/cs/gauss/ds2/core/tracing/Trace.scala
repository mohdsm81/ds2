package edu.utah.cs.gauss.ds2.core.tracing
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import java.util.UUID


/**
 * @author <br>
 *	Mohammed S. Al-Mahfoudh <br/>
 *	mahfoudh@cs.utah.edu <br/>
 *	SoC - Gauss Group <br/>
 */

@SerialVersionUID(1600)
class Trace  extends Serializable{
  
  var entries = Seq[TraceEntry]()
  
  def apply(n: Int): TraceEntry = {
    entries(n)
  }
  
  
  def +=(te: TraceEntry): Unit = {
    entries = entries :+ te
  }
  
  def toJson: JValue = {
    (getClass.getSimpleName -> entries.map { x => x.toJson})
  }
  
  def in(traces: Seq[Trace]): Boolean = {
    require(traces != null, "Trace.in(traces) method - doesn't accept null arguments")
    traces.contains(this)
  }
  def in(traceMgr: TraceManager): Boolean = {
    require(traceMgr != null, "Trace.in(traces) method - doesn't accept null arguments")
    traceMgr.traces.contains(this)
  }
  
  def is(that: Trace): Boolean = {
    if(entries.size != that.entries.size)
      false
    else
    {
      val entriesTogether = entries.zip(that.entries)
      entriesTogether.forall { x => x._1 == x._2 }
    }
  }
  
  def copy: Trace = {
    Trace.fromJson(toJson)
  }
}

object Trace{
  def fromJson(js:JValue): Trace = {
    
    val entries = js \ "Trace" match {
      case JArray(x) => x map { y => TraceEntry.fromJson(y) } 
      case _ => throw new Error("Trace.fromJson - can't extract 'entries'")
    }
    
    val trace = new Trace
    trace.entries = entries
    trace
  }
}
