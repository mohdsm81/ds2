/**
  *
  */
package edu.utah.cs.gauss.ds2.core.schedulers.composable.message.duplication

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem, Message }


/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  * Override the methods in this trait to suite the algorithm-specific
  * criteria implementing the scheduler. Default implentation is just
  * basic one.
  */
trait MessageDuplication {
  def duplicate(ds: DistributedSystem)(a:Agent, sourceIdx: Int, dstIdx: Int): Unit = ds.duplicate(a, sourceIdx, dstIdx)
  def duplicate(ds: DistributedSystem)(a:Agent, sourceMsg: Message, dstIdx: Int): Unit = ds.duplicate(a, sourceMsg, dstIdx)
}
