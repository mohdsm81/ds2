package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.schedulers.TaskQ

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class TaskQState(tq: TaskQ) extends State[TaskQ,TaskQState]{
  override var instanceToRestore: TaskQ = tq

  val queues = for((key,value) <- instanceToRestore.queues)
    yield (key, value map{ x => SuspendableTaskState(x)})

  override def restore: Unit = {
    instanceToRestore.queues = for((k,v) <- queues) yield{
      val restoredQueue = v map{x => x.restore; x.instanceToRestore}
      (k,restoredQueue)
    }
  }
}
