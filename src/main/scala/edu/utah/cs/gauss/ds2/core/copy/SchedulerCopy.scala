package edu.utah.cs.gauss.ds2.core.copy

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{DistributedSystem, SuspendableTask, TimedAction}
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.{BlockedTasksManager, Scheduler, TaskQ, TimedActionsTracker}


object SchedulerCopy {

  def attachDistributedSystemCopy(sch: Scheduler, ds:DistributedSystem): Unit = sch.attach(ds)

  /**
   * Copies the state of the first scheduler to the target scheduler. This doesn't copy the 'ds' field as
   * DistributedSystem objects need to be copied independently of the scheduler(s).
   * @param scheduler source scheduler whose state is to be copied
   * @param targetScheduler target scheduler, whose state is to be restored to the copied
   */
  def apply(scheduler: Scheduler, targetScheduler: Scheduler): Unit = {
    targetScheduler.clk = scheduler.clk
    targetScheduler.taskQ = apply(scheduler.taskQ,scheduler)
    targetScheduler.blockingMgr = apply(scheduler.blockingMgr,scheduler)
    // link the blocking tasks in the blockingMGR to their taskQ originals
    targetScheduler.blockingMgr.blockedTasks = targetScheduler.blockingMgr.blockedTasks.map{x =>
      targetScheduler.taskQ(x.action.a.name).find{z =>x.id == z.id} match{
        case None => null
        case Some(task) => task
      }
      // all other fields has been copied along from the old taskQ (suspended, timed, ...etc)
    } // now the blockingMGR has fresh copies of the tasks
    targetScheduler.timedActionsTracker = apply(scheduler.timedActionsTracker,scheduler)
    targetScheduler.consumeQ = scheduler.consumeQ map{ x => DistributedSystemCopy(x, scheduler.ds.get(x.a.name), x.action)}
    targetScheduler.consumeQ.map{x => //linking stmts and other scheduler content
      x.taskFrom = targetScheduler.taskQ(x.a.name).find{t => t.id == x.taskFrom.id} match{
        case None => null // impossible to happen. but had to do for the compiler to be happy
        case Some(task) => task
      }
    }
  }
  def apply(task: SuspendableTask, sch: Scheduler): SuspendableTask = {
    val copy = new SuspendableTask(DistributedSystemCopy(task.action,sch.ds.get(task.action.a.name)))
    copy.id = task.id
    copy.isTimed = task.isTimed
    copy.suspended = task.suspended
    copy
  }
  def apply(timedAction: TimedAction, scheduler: Scheduler): TimedAction = {
    val copy = new TimedAction(timedAction.startLimit,
      timedAction.endLimit,
      DistributedSystemCopy(timedAction.action, scheduler.ds.get(timedAction.action.a.name)),
      timedAction.howManyTimes,
      timedAction.submittedClock)
    copy.runtimeHowManyTimes = timedAction.runtimeHowManyTimes
    copy.countDown = timedAction.countDown
    copy
  }
  def apply(tat: TimedActionsTracker, scheduler: Scheduler): TimedActionsTracker = {
    val copy = new TimedActionsTracker
    copy.timedActions = tat.timedActions.map{x => apply(x,scheduler)}
    copy
  }
  def apply(btm: BlockedTasksManager, scheduler: Scheduler): BlockedTasksManager = {
    implicit val sch = scheduler
    implicit val ds = sch.ds
    val copy = new BlockedTasksManager()
    copy.blockedTasks = btm.blockedTasks.map{x => apply(x,scheduler)}
    copy
  }
  def apply(tq: TaskQ, scheduler: Scheduler): TaskQ = {
    val copy = new TaskQ(scheduler)
    copy.queues = tq.queues.map{case (k,v) => k -> v.map{task => apply(task,scheduler)}}
    copy
  }
}
