package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._

trait Becoming {

  var behaviorName: String = "" // why not ""? because this HAS to be specified in the statement
  var behaviorNameVar: String = ""
  
  var remember = false
  var rememberVar: String = ""
}
