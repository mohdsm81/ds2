package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.ExhaustiveDFSScheduler

import scala.collection.mutable.{Stack => MStack}
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DporScheduler (harnessssFilePath: String,
                     distribSys: DistributedSystem,
                     benchmark: Boolean = false,
                     log: Boolean = false,
                     iterationsLimit: Int = Int.MaxValue,
                     historySizeLimit: Int = 4048,
                     numOfSchedulesLimit: Int = Int.MaxValue
                    ) extends ExhaustiveDFSScheduler(
  harnessssFilePath,
  distribSys,
  benchmark,
  log,
  iterationsLimit,
  historySizeLimit,
  numOfSchedulesLimit){

  private val initState = new DporState(this)
  initState.setEnabledSet(schedule.toSet)
  initState.setBacktrackingSet(initState.getEnabledSet)
  backtrack = MStack(initState)
}
