package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import scala.collection.mutable.{Stack => MStack}
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
trait Backtracking {
  var backtrack: MStack[DFSState] = MStack()

  // user has to be careful to do the necessary checks before using these methods
  def currentState: DFSState = backtrack.top

  def push(state: DFSState): Unit = {
    backtrack.push(state)
  }
  def pop: DFSState = {
    backtrack.pop
  }
}
