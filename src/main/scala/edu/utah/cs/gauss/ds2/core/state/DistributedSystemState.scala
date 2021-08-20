package edu.utah.cs.gauss.ds2.core.state

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem}

import scala.collection.parallel.ParSet
import scala.collection.mutable.{Map => MMap}

case class DistributedSystemState(ds: DistributedSystem) extends State[DistributedSystem,DistributedSystemState] {
  require(ds != null, "Can't export the state of a null distributed system")
  override var instanceToRestore: DistributedSystem = ds

  val agentsStates: MMap[String,AgentState] = MMap()
  ds.agents.map{x => agentsStates(x.name) = x.snapshot}

  val temporaries: ParSet[String] = ds.temporaries

  val tracingEnabled: Boolean = ds.tracingEnabled

  override def restore: Unit = {
    val mutableIterable: Iterable[Agent] = agentsStates.values map { x => x.restore; x.instanceToRestore}
    instanceToRestore.agents = mutableIterable.toSet
    instanceToRestore.temporaries = temporaries
    instanceToRestore.tracingEnabled = tracingEnabled
  }
}
