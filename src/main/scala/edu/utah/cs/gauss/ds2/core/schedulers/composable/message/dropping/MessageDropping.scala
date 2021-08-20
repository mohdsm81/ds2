package edu.utah.cs.gauss.ds2.core.schedulers.composable.message.dropping

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{DistributedSystem, Message}
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 *
 * This is the top-level trait for any Message-Dropping traits to be mixed in with custom schedulers.
 *
 *
 */
trait MessageDropping extends Scheduler {

  /**
   * Whatever dropped message due to partitioning is stored here.
   */
  var sink: Map[String, Seq[Message]] = Map()

  /**
   * specifies algorithm specific predicate of when to drop a message to target a bug
   */
  def canDropFrom(ds: DistributedSystem)(agentName: String): Int

}
