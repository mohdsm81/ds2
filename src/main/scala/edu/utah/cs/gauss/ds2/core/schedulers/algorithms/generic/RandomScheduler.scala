package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem}

import scala.util.Random

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class RandomScheduler(override val harnFilePath: String, override val distSys: DistributedSystem, iterationsLimit: Int = Int.MaxValue, benchmark: Boolean = false, numSchedulesLimit: Int = 1) extends
  RoundRobinScheduler(harnFilePath, distSys, iterationsLimit, benchmark = benchmark, numSchedulesLimit = numSchedulesLimit) {

  private val size = items.size
  private val rnd = new Random()
  override def getNext: Agent = {
    current = items(rnd.nextInt(size))
    /* this messes up the the operation of Random it finds bugs in systems where even exhaustive doesn't w/o EX hitting
    the cut off! in other words it changes/messes with the causality of hte implementation.
    * */
//    current.q = Random.shuffle(current.q)
    current
  }

}
