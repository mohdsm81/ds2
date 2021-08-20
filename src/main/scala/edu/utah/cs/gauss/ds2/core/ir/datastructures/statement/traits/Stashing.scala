package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._

trait Stashing {

  // the default is to stash the message at the head of the queue
  var stashed: Either[Message, Int ]= Right(0)
  var stashedVar: String = ""
}
