package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem, Message}
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.DporState
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola._
import java.io.File

class LiViolaSchedulerSpec extends MyTestSpecs{
  test("test - experimenting") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.lv.1.txt", 5, log=false)
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

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

    val sample: (DistributedSystem, File) = SampleDistributedSystems.transDporExample("./harness.lv.2.txt")
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

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

    sch.currentState.asInstanceOf[DporState].setBacktrackingSet(Set(sch.currentState.getEnabledSet.head))
    //    sch.currentState.asInstanceOf[DporState].setBacktrackingSet(Set((r0, ds.get(registry))))

    //    sch.dependencySet = sch.dependencySet ++ sch.currentState.getEnabledSet.map{x => (x -> x)}

    //    sch.targetAgents = Set(ds.get(registry), ds.get(worker1), ds.get(worker2))
    sch.targetAgents = Set(ds.get(worker1), ds.get(registry), ds.get(worker2))
    sch.benchmarkStatistics.numOfAgents = sch.targetAgents.size

    sch.explore
//    println(sch.benchmarkStatistics.toString)

    val strings = sch.benchmarkStatistics.schedules.map{x =>
      x.map{x => if(x._2.name != "registry") s"w${x._2.name.last}" else s"r${x._1.sender.name.last}"}.mkString(",")
    }.mkString("\n")

    println(strings)
  
    if(sample._2.exists()) sample._2.delete()
  }

  test("test - err dist reg") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED_ERRONEOUS("./harness.lv.3.txt", 5, log=false)
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = 10)

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
