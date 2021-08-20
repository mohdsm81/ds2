package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits
import edu.utah.cs.gauss.ds2.core.ir.datastructures._

trait Asking extends Sending {
  var future: DummyFuture = _ // handled by scheduler
  var futureVar: String = ""
}
