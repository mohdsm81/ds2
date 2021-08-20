package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, Message}

trait ModifyingState {
  var variable: String = ""
  var variableVar: String = "" // I know reads weird
  
  var value: Any = _
  var valueVar: String = ""
  var valueFunc: (Message, Agent) => Any = _
}

trait ModifyingStateRef extends ModifyingState
