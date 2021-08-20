package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks

import benchmarks.Algorithms._
import benchmarks.Systems._
import benchmarks.{ Algorithms, MicroMetaBenchmark, Systems }
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
import edu.utah.cs.gauss.serialization.IO

import java.io.File

object MicroBenchmarkParams {
  val numOfAgents = 2
  
  val numOfReads1  = 2
  val numOfWrites1 = 1
  val numOfReads2  = 2
  val numOfWrites2 = 2
  
  val cutOff: Int = 1 // Int.MaxValue // for random it has to be specified otherwise it will run forever because
  // it isn't like the rest
  // random I gave 2000 (since Exhaustive did 1680)
  
  val delays = 3
  
  var fileOutput_DIST_REG     = "micro.benchmarks.results.DIST.REG.tex"
  var fileOutput_ERR_DIST_REG = "micro.benchmarks.results.ERR.DIST.REG.tex"
  var fileOutput_ANOTHER_DIST_REG = "micro.benchmarks.results.ANOTHER.DIST.REG.tex"
  var fileOutput_ZAB          = "micro.benchmarks.results.ZAB.tex"
  var fileOutput_PAXOS        = "micro.benchmarks.results.PAXOS.tex"
  var fileOutput_RAFT         = "micro.benchmarks.results.RAFT.tex"
  var fileOutput_OPEN_CHORD   = "micro.benchmarks.results.OPEN.CHORD.tex"
  
  object AccumulatorMicroBenchmarks {
    var firstSubColumn : Seq[ BenchmarkResults ] = Seq()
    var secondSubColumn: Seq[ BenchmarkResults ] = Seq()
    var thirdSubColumn : Seq[ BenchmarkResults ] = Seq()
    
    def printToFile( filePath: String ): Unit = {
      val outputString: String = MicroMetaBenchmark.multiResultsString( firstSubColumn zip secondSubColumn )
      IO.writeLinesToFile( Seq( outputString ), new File( filePath ) )
    }

    def reset( ): Unit = {
      firstSubColumn = Seq()
      secondSubColumn = Seq()
      //      thirdSubColumn = Seq()
    }
  }

  def run( numReads: Int, numWrites: Int, algorithm: Algorithms.Value, system: Systems.Value, column: Int,
           log     : Boolean = false ): Unit = {
    require( column >= 1 && column <= 2, "This benchmark suite supports only 2 columns" )
    //    System.gc() // invoke the garbage collector

    val benchmarkResults: Seq[ BenchmarkResults ] = MicroMetaBenchmark.doBenchmark(
      howManyTimes = 3,
      log = log,
      alg = algorithm,
      sys = system,
      numOfSchedulesLimit = cutOff,
      numOfWrites = numWrites,
      numOfReads = numReads,
      numAgents = numOfAgents )

    val avgBenchmarkResults = MicroMetaBenchmark.averageResults( benchmarkResults )

    println( avgBenchmarkResults )
    //    AccumulatorMicroBenchmarks.firstSubColumn = AccumulatorMicroBenchmarks.firstSubColumn :+ avgBenchmarkResults
    //    column match{
    //      case 1 => AccumulatorMicroBenchmarks.firstSubColumn = AccumulatorMicroBenchmarks.firstSubColumn :+
    //      avgBenchmarkResults
    //      case 2 => AccumulatorMicroBenchmarks.secondSubColumn = AccumulatorMicroBenchmarks.secondSubColumn :+
    //      avgBenchmarkResults
    //    }
  }
}

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks.MicroBenchmarkParams._


class DistributedRegisterMicroBenchmark extends MyTestSpecs {

  //####################################################################################################################
  //                                         DISTRIBUTED REGISTER
  //####################################################################################################################
  // first sub-column
  test( "EX - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, EXHAUSTIVE, DISTRIBUTED_REGISTER, 1 ) )
  test( "SR - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, RANDOM, DISTRIBUTED_REGISTER, 1 ) )
  test( "DB - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DELAY_BOUNDED, DISTRIBUTED_REGISTER, 1 ) )
  test( "DP - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DPOR, DISTRIBUTED_REGISTER, 1 ) )
  test( "TD - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, TRANS_DPOR, DISTRIBUTED_REGISTER, 1 ) )
  test( "IR - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, IRED, DISTRIBUTED_REGISTER, 1 ) )
  test( "LV - DR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, LIVIOLA, DISTRIBUTED_REGISTER, 1 ) )

  //  test("DPIR - DR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED, DISTRIBUTED_REGISTER, 1))
  //  test("DPLV - DR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA, DISTRIBUTED_REGISTER,
  //  1))

  // second sub-column
  test( "EX - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, EXHAUSTIVE, DISTRIBUTED_REGISTER, 2 ) )
  test( "SR - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, RANDOM, DISTRIBUTED_REGISTER, 2 ) )
  test( "DB - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DELAY_BOUNDED, DISTRIBUTED_REGISTER, 2 ) )
  test( "DP - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DPOR, DISTRIBUTED_REGISTER, 2 ) )
  test( "TD - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, TRANS_DPOR, DISTRIBUTED_REGISTER, 2 ) )
  test( "IR - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, IRED, DISTRIBUTED_REGISTER, 2 ) )
  test( "LV - DR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, LIVIOLA, DISTRIBUTED_REGISTER, 2 ) )

  //  test("DPIR - DR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED, DISTRIBUTED_REGISTER, 1))
  //  test("DPLV - DR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA, DISTRIBUTED_REGISTER,
  //  1))

  test( s"Writing results to file $fileOutput_DIST_REG}" ) {
    AccumulatorMicroBenchmarks.printToFile( fileOutput_DIST_REG )
    AccumulatorMicroBenchmarks.reset()
  }
}

class ErrDistributedRegisterMicroBenchmark extends MyTestSpecs {
  
  //####################################################################################################################
  //                                         Erroneous DISTRIBUTED REGISTER
  //####################################################################################################################
  // first sub-column
  test( "EX - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, EXHAUSTIVE, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "SR - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, RANDOM, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "DB - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DELAY_BOUNDED, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "DP - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DPOR, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "TD - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, TRANS_DPOR, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "IR - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, IRED, ERR_DISTRIBUTED_REGISTER, 1 ) )
  test( "LV - EDR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, LIVIOLA, ERR_DISTRIBUTED_REGISTER, 1 ) )
  
  //  test("DPIR - DR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED, DISTRIBUTED_REGISTER, 1))
  //  test("DPLV - DR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA, DISTRIBUTED_REGISTER,
  //  1))
  
  // second sub-column
  test( "EX - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, EXHAUSTIVE, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "SR - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, RANDOM, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "DB - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DELAY_BOUNDED, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "DP - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DPOR, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "TD - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, TRANS_DPOR, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "IR - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, IRED, ERR_DISTRIBUTED_REGISTER, 2 ) )
  test( "LV - EDR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, LIVIOLA, ERR_DISTRIBUTED_REGISTER, 2 ) )
  
  //  test("DPIR - DR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED, DISTRIBUTED_REGISTER, 1))
  //  test("DPLV - DR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA, DISTRIBUTED_REGISTER,
  //  1))
  
  test( s"Writing results to file $fileOutput_ERR_DIST_REG}" ) {
    AccumulatorMicroBenchmarks.printToFile( fileOutput_ERR_DIST_REG )
    AccumulatorMicroBenchmarks.reset()
  }
}

//####################################################################################################################
//                                         ERRONEOUS DISTRIBUTED REGISTER
//####################################################################################################################

class AnotherDistributedRegisterMicroBenchmark extends MyTestSpecs {
  // first sub-column
  test( "EX - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, EXHAUSTIVE, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "SR - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, RANDOM, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "DB - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DELAY_BOUNDED, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "DP - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DPOR, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "TD - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, TRANS_DPOR, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "IR - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, IRED, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  test( "LV - ADR - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, LIVIOLA, ANOTHER_DISTRIBUTED_REGISTER, 1 ) )
  
  
  //  test("DPIR - EDR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED,
  //  ERR_DISTRIBUTED_REGISTER,1))
  //  test("DPLV - EDR - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA,
  //  ERR_DISTRIBUTED_REGISTER,1))
  
  // second sub-column
  test( "EX - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, EXHAUSTIVE, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "SR - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, RANDOM, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "DB - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DELAY_BOUNDED, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "DP - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DPOR, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "TD - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, TRANS_DPOR, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "IR - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, IRED, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  test( "LV - ADR - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, LIVIOLA, ANOTHER_DISTRIBUTED_REGISTER, 2 ) )
  
  //  test("DPIR - EDR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED,
  //  ERR_DISTRIBUTED_REGISTER,1))
  //  test("DPLV - EDR - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA,
  //  ERR_DISTRIBUTED_REGISTER,1))
  
  test( s"Writing results to file $fileOutput_ANOTHER_DIST_REG}" ) {
    AccumulatorMicroBenchmarks.printToFile( fileOutput_ANOTHER_DIST_REG )
    AccumulatorMicroBenchmarks.reset()
  }
}
//####################################################################################################################
//                                                    PAXOS
//####################################################################################################################
class MultiPaxosMicroBenchmark extends MyTestSpecs {
  // first sub-column
  test( "EX - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, EXHAUSTIVE, PAXOS, 1 ) )
  test( "SR - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, RANDOM, PAXOS, 1 ) )
  test( "DB - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DELAY_BOUNDED, PAXOS, 1 ) )
  test( "DP - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DPOR, PAXOS, 1 ) )
  test( "TD - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, TRANS_DPOR, PAXOS, 1 ) )
  test( "IR - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, IRED, PAXOS, 1 ) )
  test( "LV - PX - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, LIVIOLA, PAXOS, 1 ) )

  //  test("DPIR - PX - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED, PAXOS,1))
  //  test("DPLV - PX - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA, PAXOS,1))

  // second sub-column
  test( "EX - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, EXHAUSTIVE, PAXOS, 2 ) )
  test( "SR - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, RANDOM, PAXOS, 2 ) )
  test( "DB - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DELAY_BOUNDED, PAXOS, 2 ) )
  test( "DP - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DPOR, PAXOS, 2 ) )
  test( "TD - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, TRANS_DPOR, PAXOS, 2 ) )
  test( "IR - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, IRED, PAXOS, 2 ) )
  test( "LV - PX - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, LIVIOLA, PAXOS, 2 ) )

  //  test("DPIR - PX - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED, PAXOS,1))
  //  test("DPLV - PX - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA, PAXOS,1))

  test( s"Writing results to file $fileOutput_PAXOS}" ) {
    AccumulatorMicroBenchmarks.printToFile( fileOutput_PAXOS )
    AccumulatorMicroBenchmarks.reset()
  }
}

//####################################################################################################################
//                                                  OPEN CHORD
//####################################################################################################################
class OpenChordMicroBenchmark extends MyTestSpecs {
  // first sub-column
  test( "EX - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, EXHAUSTIVE, OPEN_CHORD, 1 ) )
  test( "SR - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, RANDOM, OPEN_CHORD, 1 ) )
  test( "DB - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DELAY_BOUNDED, OPEN_CHORD, 1 ) )
  test( "DP - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, DPOR, OPEN_CHORD, 1 ) )
  test( "TD - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, TRANS_DPOR, OPEN_CHORD, 1 ) )
  test( "IR - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, IRED, OPEN_CHORD, 1 ) )
  test( "LV - OC - 2r+1w" )( MicroBenchmarkParams.run( numOfReads1, numOfWrites1, LIVIOLA, OPEN_CHORD, 1 ) )

  //  test("DPIR - OC - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED, OPEN_CHORD,1))
  //  test("DPLV - OC - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA, OPEN_CHORD,1))

  // second sub-column
  test( "EX - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, EXHAUSTIVE, OPEN_CHORD, 2 ) )
  test( "SR - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, RANDOM, OPEN_CHORD, 2 ) )
  test( "DB - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DELAY_BOUNDED, OPEN_CHORD, 2 ) )
  test( "DP - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, DPOR, OPEN_CHORD, 2 ) )
  test( "TD - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, TRANS_DPOR, OPEN_CHORD, 2 ) )
  test( "IR - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, IRED, OPEN_CHORD, 2 ) )
  test( "LV - OC - 2r+2w" )( MicroBenchmarkParams.run( numOfReads2, numOfWrites2, LIVIOLA, OPEN_CHORD, 2 ) )

  //  test("DPIR - OC - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED, OPEN_CHORD,1))
  //  test("DPLV - OC - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA, OPEN_CHORD,1))

  test( s"Writing results to file $fileOutput_OPEN_CHORD}" ) {
    AccumulatorMicroBenchmarks.printToFile( fileOutput_OPEN_CHORD )
    AccumulatorMicroBenchmarks.reset()
  }
//####################################################################################################################
//                                                    ZAB -- delayed
//####################################################################################################################
//class ZabMicroBenchmark extends MyTestSpecs {
//  // first sub-column
//  test("EX - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, EXHAUSTIVE, ZAB,1))
//  test("SR - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, RANDOM, ZAB,1))
//  test("DB - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DELAY_BOUNDED, ZAB,1))
//  test("DP - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DPOR, ZAB,1))
//  test("TD - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, TRANS_DPOR, ZAB,1))
//  test("IR - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, IRED, ZAB,1))
//  test("LV - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, LIVIOLA, ZAB,1))
//
////  test("DPIR - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_IRED, ZAB,1))
////  test("DPLV - ZB - 2r+1w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites1, DP_LIVIOLA, ZAB,1))
//
//  // second sub-column
//  test("EX - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, EXHAUSTIVE, ZAB,2))
//  test("SR - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, RANDOM, ZAB,2))
//  test("DB - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, DELAY_BOUNDED, ZAB,2))
//  test("DP - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, DPOR, ZAB,2))
//  test("TD - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, TRANS_DPOR, ZAB,2))
//  test("IR - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, IRED, ZAB,2))
//  test("LV - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads2, numOfWrites2, LIVIOLA, ZAB,2))
//
////  test("DPIR - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_IRED, ZAB,1))
////  test("DPLV - ZB - 2r+2w")(MicroBenchmarkParams.run(numOfReads1, numOfWrites2, DP_LIVIOLA, ZAB,1))
//
//  test(s"Writing results to file $fileOutput_ZAB}") {
//    AccumulatorMicroBenchmarks.printToFile(fileOutput_ZAB)
//    AccumulatorMicroBenchmarks.reset()
//  }
}