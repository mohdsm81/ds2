package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.transdpor

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.DporScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.DFSState

import scala.collection.mutable.{Stack => MStack}
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class TransDporScheduler(harnessssFilePath: String,
                         distribSys: DistributedSystem,
                         benchmark: Boolean = false,
                         log: Boolean = false,
                         iterationsLimit: Int = Int.MaxValue,
                         historySizeLimit: Int = 4048,
                         numOfSchedulesLimit: Int = Int.MaxValue
                        ) extends DporScheduler(
  harnessssFilePath,
  distribSys,
  benchmark,
  log,
  iterationsLimit,
  historySizeLimit,
  numOfSchedulesLimit) {

  private val initState = new TransDporState(this)
  backtrack = MStack(initState)
  initState.setEnabledSet(schedule.toSet)
  initState.setBacktrackingSet(Set(initState.getEnabledSet.head))

  override def pop: DFSState = {
    val oldState = super.pop
    if(backtrack.nonEmpty)
      currentState.asInstanceOf[TransDporState].freeze = false
    oldState
  }
}
