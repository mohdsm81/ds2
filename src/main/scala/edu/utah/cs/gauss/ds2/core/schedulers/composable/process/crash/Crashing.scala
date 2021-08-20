/**
  *
  */
package edu.utah.cs.gauss.ds2.core.schedulers.composable.process.crash

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem }


/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */
trait Crashing {

  def computeCrashSet(ds: DistributedSystem)(): Set[Agent]

  def crashProcess(ds: DistributedSystem)(a:Agent): Unit = ds.crash(a)

  def crashProcesses(ds: DistributedSystem)(agentsToCrash: Set[Agent]) = ds.agents.filter(agentsToCrash.contains(_)).map(crashProcess(ds)(_))
}
