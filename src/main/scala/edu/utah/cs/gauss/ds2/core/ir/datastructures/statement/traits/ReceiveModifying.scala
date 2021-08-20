package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, Message }

trait ReceiveModifyingState {
  var variable: String = ""
  var variableVar: String = "" // I know reads weird
  
  var value: (Message, Agent) => Any = {(m: Message, a:Agent) => null}
  var valueVar: String = ""
}

trait ReceiveModifyingStateRef extends ReceiveModifyingState

// This is really not needed, Any can accomodate for String
// {
//   override var value: (Message, Agent) => String = _
// }
