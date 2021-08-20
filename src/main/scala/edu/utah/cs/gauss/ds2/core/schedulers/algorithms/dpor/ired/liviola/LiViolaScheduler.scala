package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired._

import scala.collection.mutable.{Stack => MStack}
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class LiViolaScheduler(harnessssFilePath: String,
                       distribSys: DistributedSystem,
                       benchmark: Boolean = false,
                       log: Boolean = false,
                       iterationsLimit: Int = Int.MaxValue,
                       historySizeLimit: Int = 4048,
                       numOfSchedulesLimit: Int = Int.MaxValue
                        ) extends IRedScheduler(
  harnessssFilePath,
  distribSys,
  benchmark,
  log,
  iterationsLimit,
  historySizeLimit,
  numOfSchedulesLimit) {

  private val initState = new LiViolaState(this)
  backtrack = MStack(initState)
  initState.setEnabledSet(schedule.toSet)
  initState.setBacktrackingSet(Set(initState.getEnabledSet.head))
}
