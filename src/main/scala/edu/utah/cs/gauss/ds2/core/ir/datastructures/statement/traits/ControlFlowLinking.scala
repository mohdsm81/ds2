package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

/**
 Action needs to implement this trait to link control-flow correctly
 */
trait ControlFlowLinking {

  def linkControlFlow: Unit

  /**
   Finds the replacement from the action's template statements (taht
   are already linked) to reconstruct the executed and toExecute
   queues.
   */
  def findReplacement(stmtToReplace: Statement): Option[Statement]
}

trait Searchable {

  /**
   Searches the stmt that is unliked (provided as arg) and returnes
   the linked version of it.
   @param unlinkedStmtToSearch the statement that is not linked, 
          which needs to be replaced by the returned statement
   @return the linked statement that will replace the unlinked one.
   */
  def search(unlinkedStmtToSearch: Statement): Option[Statement]
}
