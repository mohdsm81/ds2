package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Action, Agent, Message }

trait SettingParams {
  def setAction(act: Action): Unit
  def setAgent(a:Agent): Unit
  def setMessage(m:Message): Unit 
}
