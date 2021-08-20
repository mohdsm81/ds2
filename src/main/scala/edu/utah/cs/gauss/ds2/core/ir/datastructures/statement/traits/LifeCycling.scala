package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent

trait Starting {
  var dstAgent: Agent = _
  var dstAgentVar: String = ""
  
  var args = Seq[String]()
  var argsVar: String = ""
}

trait Stopping extends Starting
trait Killing extends Starting
