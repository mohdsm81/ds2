//package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability
//
//import java.io.File
//
//import edu.utah.cs.gauss.ds2.core.MyTestSpecs
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
//import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
//import edu.utah.cs.gauss.serialization.IO
//import org.junit.Ignore
//
//@Ignore
//class Benchmarks extends MyTestSpecs {
//
//
//  import OutputStatsToTex.OutputToTexTable
//
//  val file = new File("./Docs/ds2-paper/tables/big-table-data.tex")
//
//  //  =============================================================================================
//  //    Creating a new file with table data
//  //  =============================================================================================
//  test("creating an empty table file with headers"){
//
//    // create the file and headers
//    file.delete
//    file.createNewFile()
//  }
//
//  //  =============================================================================================
//  //    Dist. Reg. benchmarks
//  //  =============================================================================================
//
//  test("Dist. Reg. Benchmark") {
//
//    allBenchmarkResults = Seq.empty
//
//    val sample: (DistributedSystem, File) =
//      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.1.txt", 2)
//    val sch = new RandomScheduler(sample._2.getPath, sample._1)
//
//    sch.explore
//
//  }
//
//  //  =============================================================================================
//  //    Zab benchmarks
//  //  =============================================================================================
//
//  test("Zab") {
//    allBenchmarkResults = Seq.empty
//
//  }
//
//  //  =============================================================================================
//  //    Paxos benchmarks
//  //  =============================================================================================
//  test("Paxos"){
//    allBenchmarkResults = Seq.empty
//
//  }
//  //  =============================================================================================
//  //    Raft benchmarks
//  //  =============================================================================================
//  test("Raft"){
//    allBenchmarkResults = Seq.empty
//
//  }
//
//  //  =============================================================================================
//  //    Open Chord benchmarks
//  //  =============================================================================================
//  test("Open Chord"){
//    allBenchmarkResults = Seq.empty
//
//  }
//}
