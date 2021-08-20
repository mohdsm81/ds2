package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DummyFuture

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class DummyFutureState(df: DummyFuture) extends State[DummyFuture,DummyFutureState]{
  override var instanceToRestore: DummyFuture = df

  // df.id doesn't change
  val resolved = instanceToRestore.resolved
  val value = instanceToRestore.value
  val promisedBy = instanceToRestore.promisedBy
  val waitingFor = instanceToRestore.waitingFor

  override def restore: Unit = {
    instanceToRestore.resolved = resolved
    instanceToRestore.value = value
    instanceToRestore.promisedBy = promisedBy
    instanceToRestore.waitingFor = waitingFor
  }
}
