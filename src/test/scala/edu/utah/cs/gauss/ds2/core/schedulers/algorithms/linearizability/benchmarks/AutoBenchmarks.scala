package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks

import java.io.File
import benchmarks.{ Algorithms, Systems }
import edu.utah.cs.gauss.Helpers.CollectionsUtils.zip3
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
import edu.utah.cs.gauss.serialization.IO
import Systems._
import Algorithms._
import org.scalatest.Ignore

object BenchmarkParams {
  val numOfWrites1 = 2
  val numOfReads1 = 4
  val numOfWrites2 = 4
  val numOfReads2 = 8
  val numOfWrites3 = 6
  val numOfReads3 = 12

  val cutOff: Int = 10

  val delays = 3

  var fileOutput_DIST_REG = "benchmarks.results.DIST.REG.tex"
  var fileOutput_ERR_DIST_REG = "benchmarks.results.ERR.DIST.REG.tex"
  var fileOutput_ZAB = "benchmarks.results.ZAB.tex"
  var fileOutput_PAXOS = "benchmarks.results.PAXOS.tex"
  var fileOutput_RAFT = "benchmarks.results.RAFT.tex"
  var fileOutput_OPEN_CHORD = "benchmarks.results.OPEN.CHORD.tex"

  object AccumulatorBenchmarks {
    var firstSubColumn: Seq[BenchmarkResults] = Seq()
    var secondSubColumn: Seq[BenchmarkResults] = Seq()
    var thirdSubColumn: Seq[BenchmarkResults] = Seq()

    def printToFile(filePath: String): Unit = {
      val outputString: String = MetaBenchmark.multiResultsString(zip3(firstSubColumn, secondSubColumn, thirdSubColumn).toSeq)
      IO.writeLinesToFile(Seq(outputString), new File(filePath))
    }

    def reset(): Unit = {
      firstSubColumn = Seq()
      secondSubColumn = Seq()
      thirdSubColumn = Seq()
    }
  }

  def run(numReads: Int, numWrites: Int, algorithm: Algorithms.Value, system: Systems.Value, column: Int): Unit = {
    require(column >= 1 && column <= 3, "This benchmark suite supports only 3 columns")
    //    System.gc() // invoke the garbage collector
    val benchmarkResults: Seq[BenchmarkResults] = MetaBenchmark.doBenchmark(algorithm, system, numOfSchedulesLimit = cutOff, numOfWrites = numWrites, numOfReads = numReads)
    val avgBenchmarkResults = MetaBenchmark.averageResults(benchmarkResults)
    println(avgBenchmarkResults)
    column match{
      case 1 => AccumulatorBenchmarks.firstSubColumn = AccumulatorBenchmarks.firstSubColumn :+ avgBenchmarkResults
      case 2 => AccumulatorBenchmarks.secondSubColumn = AccumulatorBenchmarks.secondSubColumn :+ avgBenchmarkResults
      case 3 => AccumulatorBenchmarks.thirdSubColumn = AccumulatorBenchmarks.thirdSubColumn :+ avgBenchmarkResults
    }
  }
}

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks.BenchmarkParams._
@Ignore
class DistributedRegisterBenchmark extends MyTestSpecs {
  //####################################################################################################################
  //                                         DISTRIBUTED REGISTER
  //####################################################################################################################
  // first sub-column
  test("EX - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, EXHAUSTIVE, DISTRIBUTED_REGISTER,1))
  test("SR - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, RANDOM, DISTRIBUTED_REGISTER,1))
  test("DB - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, DELAY_BOUNDED, DISTRIBUTED_REGISTER,1))
  test("DP - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, DPOR, DISTRIBUTED_REGISTER,1))
  test("TD - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, TRANS_DPOR, DISTRIBUTED_REGISTER,1))
  test("IR - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, IRED, DISTRIBUTED_REGISTER,1))
  test("LV - DR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, LIVIOLA, DISTRIBUTED_REGISTER,1))

  // second sub-column
  test("EX - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, EXHAUSTIVE, DISTRIBUTED_REGISTER,2))
  test("SR - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, RANDOM, DISTRIBUTED_REGISTER,2))
  test("DB - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, DELAY_BOUNDED, DISTRIBUTED_REGISTER,2))
  test("DP - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, DPOR, DISTRIBUTED_REGISTER,2))
  test("TD - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, TRANS_DPOR, DISTRIBUTED_REGISTER,2))
  test("IR - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, IRED, DISTRIBUTED_REGISTER,2))
  test("LV - DR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, LIVIOLA, DISTRIBUTED_REGISTER,2))

  // third sub-column
  test("EX - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, EXHAUSTIVE, DISTRIBUTED_REGISTER,3))
  test("SR - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, RANDOM, DISTRIBUTED_REGISTER,3))
  test("DB - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, DELAY_BOUNDED, DISTRIBUTED_REGISTER,3))
  test("DP - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, DPOR, DISTRIBUTED_REGISTER,3))
  test("TD - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, TRANS_DPOR, DISTRIBUTED_REGISTER,3))
  test("IR - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, IRED, DISTRIBUTED_REGISTER,3))
  test("LV - DR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, LIVIOLA, DISTRIBUTED_REGISTER,3))

  // ====================================================
  // printing results to a file
  // ====================================================
  test(s"Writing results to file $fileOutput_DIST_REG}") {
    AccumulatorBenchmarks.printToFile(fileOutput_DIST_REG)
    AccumulatorBenchmarks.reset()
  }
}

//####################################################################################################################
//                                         ERRONEOUS DISTRIBUTED REGISTER
//####################################################################################################################
@Ignore
class ErrDistributedRegisterBenchmark extends MyTestSpecs {
  // first sub-column
  test("EX - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, EXHAUSTIVE, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("SR - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, RANDOM, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("DB - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, DELAY_BOUNDED, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("DP - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, DPOR, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("TD - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, TRANS_DPOR, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("IR - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, IRED, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test("LV - EDR - 6")(BenchmarkParams.run(numOfReads1, numOfWrites1, LIVIOLA, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )

  // second sub-column
  test("EX - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, EXHAUSTIVE, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("SR - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, RANDOM, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("DB - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, DELAY_BOUNDED, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("DP - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, DPOR, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("TD - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, TRANS_DPOR, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("IR - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, IRED, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test("LV - EDR - 12")(BenchmarkParams.run(numOfReads2, numOfWrites2, LIVIOLA, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )

  // third sub-column
  test("EX - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, EXHAUSTIVE, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("SR - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, RANDOM, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("DB - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, DELAY_BOUNDED, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("DP - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, DPOR, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("TD - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, TRANS_DPOR, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("IR - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, IRED, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )
  test("LV - EDR - 18")(BenchmarkParams.run(numOfReads3, numOfWrites3, LIVIOLA, ANOTHER_DISTRIBUTED_REGISTER, 3 ) )

  // ====================================================
  // printing results to a file
  // ====================================================
  test(s"Writing results to file $fileOutput_ERR_DIST_REG}") {
    AccumulatorBenchmarks.printToFile(fileOutput_ERR_DIST_REG)
    AccumulatorBenchmarks.reset()
  }
}

//####################################################################################################################
//                                                    ZAB
//####################################################################################################################
//####################################################################################################################
//                                                    PAXOS
//####################################################################################################################
//####################################################################################################################
//                                                  OPEN CHORD
//####################################################################################################################

