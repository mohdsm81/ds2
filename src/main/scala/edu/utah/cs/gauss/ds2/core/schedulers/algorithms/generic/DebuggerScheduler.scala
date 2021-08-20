package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.AbstractTypes.Schedule
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
class DebuggerScheduler(harnessF: String = "", dSys: DistributedSystem, sch: Schedule = Seq()) extends RoundRobinScheduler(distSys = dSys, harnFilePath = harnessF) with Backtracking {
  override val currentState = new DFSState(this)

  override val maxHistoryLength: Int = 4048
  val tagetAgents = ds.agents
  ds.agents.map(ds.unlock(_)) // unlock all agents

  val receives = schedule.take(2).toSet
  currentState.addToEnabledSet(receives)

  override def explore(): Unit = sch.map{ r =>
    // basically let everything be done by the implementation except the what and when for receives to execute are decided by the schedule
    val rr = currentState.toExploreSet.find{ x => x._1.name == r._1.name && x._2.name == r._2.name}.get
    currentState.performReceive(rr)
    currentState.liftReceivesOfADTAgents // because all receives will be provided in the schedule 'sch'
    currentState.markExplored(rr)
    print("")
  }
}
