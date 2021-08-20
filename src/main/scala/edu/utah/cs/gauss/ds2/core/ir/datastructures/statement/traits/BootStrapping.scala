package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

trait BootStrapping {
  // set of agent names bootstrapped with args (agentName,argsString)
  var bootStrapped: Set[(String, String)] = Set()
  var bootStrappedVar: String = "" // where the booStrapped set sits
}
