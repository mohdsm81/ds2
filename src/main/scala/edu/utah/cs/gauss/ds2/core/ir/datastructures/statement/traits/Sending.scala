package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._

trait Sending {

  var msgOut: Message = _ // dynamic
  var msgOutVar: String = ""

  var dstAgent: Agent = _ // dynamic
  var dstAgentVar = ""

}
