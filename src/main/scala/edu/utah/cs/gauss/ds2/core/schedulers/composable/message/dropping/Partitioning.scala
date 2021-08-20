/**
 *
 */
package edu.utah.cs.gauss.ds2.core.schedulers.composable.message.dropping

import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
trait Partitioning extends MessageDropping {

  /**
   * Returns the same distributed system after partitioning
   */
  def partition(parted: Set[Agent]): BigInt = ds.partition(parted)

  /**
   * Returns the same distributed system after unpartitioning.
   */
  def unpartition(part1: Set[Agent])(part2: Set[Agent]): BigInt = ds.unpartition(part1, part2)

}
