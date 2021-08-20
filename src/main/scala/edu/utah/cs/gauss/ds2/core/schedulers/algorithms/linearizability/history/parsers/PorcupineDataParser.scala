package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.parsers

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVector

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
object PorcupineData {
  val writeOp = ":append"
  val readOp = ":get"
  val partRegex = """nil|"(\w\s\d+\s\d+\s\w)+"|\d+|\w+""".r.unanchored
  //  val regex = (""":process\s+(\d+),\s+:type\s+(:ok|:invoke),\s+:f\s+(:put|:append|:get),\s+:key\s+"(\d+)\",\s+:value\s+""").r.unanchored

  // new one
  val regex = ("""\{\s*:process\s+(\d+)\s*,\s*:type\s+(:ok|:invoke)\s*,\s*:f\s+(:write|:put|:append|:cas|:read|:get)\s*,\s*:key\s+"(\d+|\w+|\p{Alnum}+)"\s*,\s*:value\s+(nil|"(\w\s\d+\s\d+\w)+"|".*")\s*\}""").r.anchored

  /*
   Example matching entries in the log:
   {:process 0, :type :ok, :f :get, :key "1", :value "x 0 10 yx 0 5 y"}
   {:process 0, :type :invoke, :f :append, :key "7", :value "x 0 11 y"}
   */

  def parseKV(data: Seq[String]): Seq[Event] = {
    var result = Seq[Event]()
    data.map { line =>
      line match {
        case regex(pid, invResp, opname, key, value, _) if (invResp == ":invoke") =>
          result = result :+ Invocation(Some(key), pid, opname, Seq(trimQoutes(value)))

        case regex(pid, invResp, opname, key, value, _) if (invResp == ":ok") =>
          result = result :+ new Response(Some(key), pid, opname, Seq(trimQoutes(value)))

        case _ => // ignore if not matched
      }
    }
    result
  }


  def trimQoutes(str: String): String = {
    var result = ""
    if (str == null || str.isEmpty) return result
    else result = str.filterNot(_ == '\"')
    result
  }


  def parseKV2(data: Seq[String]): Seq[Event] = {
    // Manual parsing using string operations jsut to make sure the above Regex-based parser actually works as intended


    // tiny regexes
    object Delimeter {
      val PROCESS = ":process "
      val TYPE = ":type "
      val OPERATION = ":f "
      val KEY = ":key "
      val VALUE = ":value "
      val DOUBLE_QUOTE = "\""
      val SPACE = " "
      val COMMA_SPACE = ", "
      val CLOSING_BRACE = "}"
    }

    import Delimeter._

    // """{:process 4, :type :invoke, :f :append, :key "1", :value "x 4 0 y"}"""

      for (line <- data) yield {
        var str = line
        str = line.split(PROCESS)(1) // remains of string
        val procID = str.split(COMMA_SPACE)(0).trim // process id
        str = str.split(TYPE)(1) // remains
        val typ = str.split(COMMA_SPACE)(0).trim
        str = str.split(OPERATION)(1) // remains
        val operation = str.split(COMMA_SPACE)(0).trim
        str = str.split(KEY)(1) // remains
        val key = str.split(COMMA_SPACE)(0).split(DOUBLE_QUOTE)(1).trim // key string w/o quotes
        str = str.split(VALUE)(1) // remains
        var value = str.split(CLOSING_BRACE)(0).trim
        if(value.startsWith("nil")) value = "nil"
        else if (value.trim == "\"\"" ) value = ""
        else value = value.split(DOUBLE_QUOTE)(1).trim
        typ match{
          case ":invoke" => Invocation(Some(key),procID,operation,Seq(value))
          case ":ok" => Response(Some(key),procID,operation,Seq(value))
        }
      }
  }
}
