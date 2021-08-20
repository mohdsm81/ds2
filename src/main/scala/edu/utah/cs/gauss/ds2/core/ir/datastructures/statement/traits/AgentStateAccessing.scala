package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

trait AgentStateAccessing {
  // by default it is false till enabled by e.g. modify-state/get statement kinds
  // or to access local state somehow
  var agentStateWrite: Boolean = false
  var agentStateRead1: Boolean = false
  var agentStateRead2: Boolean = false
  var agentStateRead3: Boolean = false

  def hashCodeValue: Int = {
    agentStateRead1.hashCode() +
    agentStateRead2.hashCode() +
    agentStateRead3.hashCode() +
    agentStateWrite.hashCode() 
  }
}
