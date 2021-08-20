package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers

import java.io.File
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import edu.utah.cs.gauss.ds2.core.time.versionvectors.{DottedVersionVector => DVV}
import edu.utah.cs.gauss.serialization.IO

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */



object Parser{
  private val casArgsRegex = raw"\[(\d+)\s+(\d+)\]".r

  val kvRegex = raw"(INFO\s+jepsen.util\s+-\s+)?(\d+)\s+(:invoke|:ok|:fail|:info)\s+(:read|:write|:cas)\s+(\d+|nil|:timed-out|${casArgsRegex})".r.unanchored
  val setInvocationRegex = raw"(\d+)\s+(invokes)\s+(\badd\b|\bremove\b|\bcontains\b)\s+(\d+)".r.unanchored // works, checked it
  val setResponseRegex = raw"(\d+)\s+\breturns\b\s+(\btrue\b|\bfalse\b|\bTRUE\b|\bFALSE\b)".r.unanchored // works, checked it

  def parseKVLog(lines: Seq[String]): Seq[Event] = {
    var result= Seq[Event]()
    lines.map {
      case kvRegex(_,pid,inv,opname,arg,expected,intended) =>
        opname match{
          case ":cas" if (inv == ":invoke") => // Invocation()
            result = result :+ Invocation(None, pid, opname, Seq(expected, intended))
          case ":cas" if (inv == ":info") => // timed-out Info()
            result = result :+ Info(None, pid, opname, Seq(arg))
          case ":cas" if (inv == ":ok") => // Response()
            result = result :+ new Response(None, pid, opname, Seq(expected, intended))
          case ":cas" if (inv == ":fail") => // Fail()
            result = result :+ Fail(None, pid, opname, Seq(intended,expected))
          case x =>
            inv match{
              case ":invoke" => result = result :+ Invocation(None, pid, opname, Seq(arg))
              case ":ok" => result = result :+ new Response(None, pid, opname, Seq(arg))
              case ":fail" => result = result :+ Fail(None, pid, opname, Seq(arg))
              case ":info" => result = result :+ Info(None, pid, opname, Seq(arg))
            }
        }        
      case _ => // ignore anything else
    }
    // DEBUG
    // println(result.mkString("\n"))
    result
  }
  def parseSetLog(lines: Seq[String]): Seq[Event] = {
    var result = Seq[Event]()
    lines map{
      case setInvocationRegex(pid,inv,opname,arg) => result = result :+ Invocation(None, pid, opname, Seq(arg)) 
      case setResponseRegex(pid,arg) => result = result :+ new Response(None, pid, ":returns", Seq(arg)) // arg is true/false returned
      case _ => // ignore anything else
    }
    result
  }

  def parseKVLog(file: File): Seq[Event] = {
    val content = IO.readLinesFromFile(file)
    // val idx = content.indexWhere( s => s.contains("INFO  knossos.core") || s.startsWith("------"), 0)
    // if(idx > 0)
    //   parseKVLog(content.slice(0,idx -1))
    // else
      parseKVLog(content)
  }
  def parseSetLog(file: File): Seq[Event] = {
    val content = IO.readLinesFromFile(file)
    // val idx = content.indexWhere(s => s.contains("INFO  knossos.core") || s.startsWith("------"), 0)
    // if(idx > 0)
    //   parseSetLog(content.slice(0,idx -1))
    // else
      parseSetLog(content)
  }

}
