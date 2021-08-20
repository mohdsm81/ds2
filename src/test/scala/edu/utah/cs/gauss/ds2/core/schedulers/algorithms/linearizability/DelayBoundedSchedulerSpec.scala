package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.DelayBoundedScheduler
import java.io.File

class DelayBoundedSchedulerSpec extends MyTestSpecs{
  test("test - experimenting") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.db.1.txt", 5, log=false)
    val sch = new DelayBoundedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10, delays = 3)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(2).toSet)
    sch.explore
    println(sch.benchmarkStatistics.toString)
    
    if(sample._2.exists()) sample._2.delete()
  }

  test("test - err dist reg") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED_ERRONEOUS("./harness.db.2.txt", 5, log=false)
    val sch = new DelayBoundedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10, delays = 3)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(2).toSet)
    sch.explore
    println(sch.benchmarkStatistics.toString)
  
    if(sample._2.exists()) sample._2.delete()
  }
}
