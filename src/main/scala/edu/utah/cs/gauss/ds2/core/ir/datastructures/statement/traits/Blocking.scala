package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import java.util.UUID

trait Blocking {
 
  var future: DummyFuture = _
  var futureVar = ""
  
  var dstVariableName: String = "" // yes if it is resolved the agent may very well ignore/discard the value
}
