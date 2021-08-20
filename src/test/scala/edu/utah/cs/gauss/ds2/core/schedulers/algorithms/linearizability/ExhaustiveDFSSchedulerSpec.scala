package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import java.io.File

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem, Message}
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.ExhaustiveDFSScheduler

class ExhaustiveDFSSchedulerSpec extends MyTestSpecs {

  test("First test") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ex.1.txt", 5, log = false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log = false, numOfSchedulesLimit = 10)

    sch.currentState.setEnabledSet(sch.schedule.take(2).toSet)

    sch.explore
    sch.benchmarkStatistics.generateHistories()
    println(sch.benchmarkStatistics.toString)
    
    if(sample._2.exists()) sample._2.delete()
    
  }

  test("second test - experimenting") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ex.2.txt", 5, log=false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

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

  ignore("test - the paper example") {

    val sample: (DistributedSystem, File) = SampleDistributedSystems.transDporExample("./harness.ex.3.txt")
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

    // names of the agents in the distributed system:
    val worker1 = "worker1"
    val worker2 = "worker2"
    val registry = "registry"
    val master = new Agent("master0")

    // names of messages
    val register = "register"
    val registryAddress = "registry-address"

    // message
    val r0 = new Message(register)
    r0.sender = master
    val w1 = new Message(registryAddress)
    w1.sender = master
    val w2 = new Message(registryAddress)
    w2.sender = master

    val ds = sch.ds

    //     set of co-enabled receives
    sch.currentState.setEnabledSet(Set(
      (r0, ds.get(registry)),
      (w1, ds.get(worker1)),
      (w2, ds.get(worker2))
    ))



    //    sch.targetAgents = Set(ds.get(registry), ds.get(worker1), ds.get(worker2))
    sch.targetAgents = Set(ds.get(worker1), ds.get(registry), ds.get(worker2))
    sch.benchmarkStatistics.numOfAgents = sch.targetAgents.size

    sch.explore

    assert(sch.benchmarkStatistics.schedules.size == 30)

//    println(sch.benchmarkStatistics.toString)

    //    val strings = sch.benchmarkStatistics.schedules.map{x =>
    //      x.map{y => s"${y._1.name} by ${y._1.sender.name} => ${y._2.name}"}.mkString("\n")
    //    }.mkString("\n=======================\n")

    val strings = sch.benchmarkStatistics.schedules.map{x =>
      x.map{x => if(x._2.name != "registry") s"w${x._2.name.last}" else s"r${x._1.sender.name.last}"}.mkString(",")
    }.mkString("\n")

    println(strings)
  
    if(sample._2.exists()) sample._2.delete()
  }

  test("test - err dist reg") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED_ERRONEOUS("./harness.ex.4.txt", 5, log=false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

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
