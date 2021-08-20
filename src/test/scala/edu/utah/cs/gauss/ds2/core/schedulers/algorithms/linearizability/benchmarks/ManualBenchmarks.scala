package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.DporScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.IRedScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola.LiViolaScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.transdpor.TransDporScheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.{ DelayBoundedScheduler, ExhaustiveDFSScheduler, RandomScheduler }
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.SampleDistributedSystems

import java.io.File


class ManualBenchmarks extends MyTestSpecs{

  val numOfWrites1 = 2
  val numOfReads1 = 4
  val numOfWrites2 = 4
  val numOfReads2 = 8
  val numOfWrites3 = 6
  val numOfReads3 = 12

  val cutOff: Int = 10

  val delays = 3

  // ====================================================
  // Exhaustive
  // ====================================================

  test("EX - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ex.6.txt", 5, log=false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("EX - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ex.12.txt", 5, log=false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("EX - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ex.18.txt", 5, log=false)
    val sch = new ExhaustiveDFSScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  // ====================================================
  // Random
  // ====================================================

  test("SR - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.sr.6.txt", 5, log=false)
    val sch = new RandomScheduler(sample._2.getPath, sample._1, benchmark = true, numSchedulesLimit = cutOff)

    // pick any mix
    sch.schedule = sch.schedule.filter { r => sch.isModificationCall(r._1) }.take(numOfWrites1) ++
      sch.schedule.filterNot { r => sch.isModificationCall(r._1) }.take(numOfReads1)
    sch.benchmarkStatistics.harness = sch.schedule.toList
    sch.explore
    sch.benchmarkStatistics.spec = sch.spec
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }
  test("SR - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.sr.12.txt", 5, log=false)
    val sch = new RandomScheduler(sample._2.getPath, sample._1, benchmark = true, numSchedulesLimit = cutOff)

    // pick any mix
    sch.schedule = sch.schedule.filter { r => sch.isModificationCall(r._1) }.take(numOfWrites2) ++
      sch.schedule.filterNot { r => sch.isModificationCall(r._1) }.take(numOfReads2)
    sch.benchmarkStatistics.harness = sch.schedule.toList
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }
  test("SR - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.sr.18.txt", 5, log=false)
    val sch = new RandomScheduler(sample._2.getPath, sample._1, benchmark = true, numSchedulesLimit = cutOff)

    // pick any mix
    sch.schedule = sch.schedule.filter { r => sch.isModificationCall(r._1) }.take(numOfWrites3) ++
      sch.schedule.filterNot { r => sch.isModificationCall(r._1) }.take(numOfReads3)
    sch.benchmarkStatistics.harness = sch.schedule.toList
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  // ====================================================
  // DB
  // ====================================================

  test("DB - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.db.6.txt", 5, log=false)
    val sch = new DelayBoundedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff , delays = delays)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("DB - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.db.12.txt", 5, log=false)
    val sch = new DelayBoundedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff , delays = delays)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("DB - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.db.18.txt", 5, log=false)
    val sch = new DelayBoundedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff , delays = delays)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }
  // ====================================================
  // DP
  // ====================================================

  test("DP - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.dp.6.txt", 5, log=false)
    val sch = new DporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("DP - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.dp.12.txt", 5, log=false)
    val sch = new DporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("DP - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.dp.18.txt", 5, log=false)
    val sch = new DporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  // ====================================================
  // TD
  // ====================================================

  test("TD - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.td.6.txt", 5, log=false)
    val sch = new TransDporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("TD - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.td.12.txt", 5, log=false)
    val sch = new TransDporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("TD - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.td.18.txt", 5, log=false)
    val sch = new TransDporScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  // ====================================================
  // IR
  // ====================================================

  test("IR - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ir.6.txt", 5, log=false)
    val sch = new IRedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("IR - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ir.12.txt", 5, log=false)
    val sch = new IRedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("IR - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.ir.18.txt", 5, log=false)
    val sch = new IRedScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  // ====================================================
  // LV
  // ====================================================

  test("LV - 6"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.lv.6.txt", 5, log=false)
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites1).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads1).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("LV - 12"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.lv.12.txt", 5, log=false)
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites2).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads2).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }

  test("LV - 18"){
    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.lv.18.txt", 5, log=false)
    val sch = new LiViolaScheduler(sample._2.getPath, sample._1, benchmark = true, log=false, numOfSchedulesLimit = cutOff)

    // pick any mix
    sch.currentState.setEnabledSet(
      // there are 3 writes
      sch.schedule.filter{ r => sch.isModificationCall(r._1)}.take(numOfWrites3).toSet
        // and 7 reads
        ++ sch.schedule.filterNot{r => sch.isModificationCall(r._1)}.take(numOfReads3).toSet)
    sch.explore
    sch.benchmarkStatistics.checkUniqueHistories
    println(sch.benchmarkStatistics.toString)
    if(sample._2.exists()) sample._2.delete()
  }



}
