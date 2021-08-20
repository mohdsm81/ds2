package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

trait Timing extends Blocking {
  var timeout: BigInt = 0
  var timeoutVar: String = ""
}
