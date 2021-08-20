package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.schedulers.{BlockedTasksManager, Scheduler, TaskQ, TimedActionsTracker}
import edu.utah.cs.gauss.ds2.core.tracing.TraceManager

/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  *
  * This class is a container for a Scheduler object state in order
  * to support back-tracking algorithms.
  *
  * It contains all the state that constitutes a scheduler restart from
  * some point in time.
  *
  */
case class SchedulerState(sch: Scheduler) extends State[Scheduler,SchedulerState]{
  override var instanceToRestore: Scheduler = sch

  // ds and traceManagers are not of the scheduler-state
  val clk: BigInt = instanceToRestore.clk
  val taskQ = TaskQState(instanceToRestore.taskQ)
  val consumeQ = instanceToRestore.consumeQ
  val timedActionsTracker: TimedActionsTrackerState = TimedActionsTrackerState(instanceToRestore.timedActionsTracker)
  val blockingMgr: BlockedTasksManagerState = BlockedTasksManagerState(instanceToRestore.blockingMgr)

  override def restore: Unit = {
    instanceToRestore.clk = clk
    instanceToRestore.taskQ = {taskQ.restore; taskQ.instanceToRestore}
    instanceToRestore.consumeQ = consumeQ
    instanceToRestore.timedActionsTracker = {timedActionsTracker.restore; timedActionsTracker.instanceToRestore}
    instanceToRestore.blockingMgr = {blockingMgr.restore; blockingMgr.instanceToRestore}
  }
}
