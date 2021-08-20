package edu.utah.cs.gauss.ds2.core.state

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

trait Snapshot[T <: State[_,_]] {
  def snapshot: T
  def restore(state: T): Unit
}
