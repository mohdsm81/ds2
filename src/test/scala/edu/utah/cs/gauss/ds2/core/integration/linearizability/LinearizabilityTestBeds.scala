package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Start => StartMsg }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{ Ask, Get => GetStmt }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Start => StartMsg }

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import java.util.concurrent.ScheduledExecutorService
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.tracing.Get
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM
/**
 * @author <br>
 * Mohammed S. Al-Mahfoudh <br/>
 * mahfoudh@cs.utah.edu <br/>
 * SoC - Gauss Group <br/>
 */
object LinearizabilityTestBeds {
  def registeryWith2Clients: DistributedSystem = {
    val ds = new DistributedSystem("Registery")

    val register = Agent("Register")
    val client1 = Agent("client1")
    val client2 = Agent("client2")

    // no need to attach a scheduler now, we can wait later to attach
    // the linearizability scheduler

    

    ds.refresh
    ds
  }


}
