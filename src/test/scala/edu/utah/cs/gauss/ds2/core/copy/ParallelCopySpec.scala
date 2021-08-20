package edu.utah.cs.gauss.ds2.core.copy

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.RoundRobinScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.SampleDistributedSystems.{sampleLinearizableNAgentsRegister, sampleNoneLinearizableNAgentsRegister}

import scala.reflect.io.File

class ParallelCopySpec extends MyTestSpecs {

  val (dsInstance,file) = sampleLinearizableNAgentsRegister("parallel.copy.txt",5)
  val dsCopy = DistributedSystemCopy(dsInstance)
  val sch = new RoundRobinScheduler(file.getPath, dsInstance)
  // note that you CAN NOT copy a scheduler without having the DS field set already since the scheduler uses that field
  // to copy a lot of its constructs and their content
  val schCopy = new RoundRobinScheduler(file.getPath, dsCopy)
  SchedulerCopy(sch,schCopy)

  ignore("Use Scheduler with the DS"){

  }

  ignore("Use Scheduler Copy with new Copy of DS"){

  }

  ignore("compare the states of schedulers and/or DS's"){

  }

}
