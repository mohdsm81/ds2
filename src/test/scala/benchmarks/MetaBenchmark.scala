package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks

import java.io.File

import benchmarks.{Algorithms, Systems}
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.ired.liviola._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.dpor.transdpor._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.SampleDistributedSystems
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
import edu.utah.cs.gauss.ds2.core.time.StopWatch

import scala.concurrent.duration._

object MetaBenchmark {

  /**
   * When the enum name of the distributed system is supplied, the function returns an instance of the distributed system.
   *
   * @param system the system name (enum name in Systems)
   * @return the <code>DistributedSystem</code> object
   */
  def getDistributedSystemInstance(system: Systems.Value): (DistributedSystem, File) = {
    system match {
      case Systems.DISTRIBUTED_REGISTER         =>
        SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED("./harness.1.txt", 5, log = false)
      case Systems.ANOTHER_DISTRIBUTED_REGISTER =>
        SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED_ERRONEOUS("./harness.1.txt", 5, log = false)
      case Systems.ZAB                          => ???
      case Systems.PAXOS => ???
      case Systems.OPEN_CHORD => ???
//      case Systems.RAFT => ???
      case x => throw new Error(s"No model implementation available for: ${x}")
    }
  }

  /**
   * Creates an instance of the algorithm passed, and attaches it to the system under check several times in order to
   * account for noise.
   *
   * @param alg                 the algorithm name listed in <code>Algorithms</code>
   * @param sys                 the system name listed in <code>Systems</code>
   * @param howManyTimes        how many times to check the system
   * @param delays              if the scheduler is the delay-bonded one, then this must be set to a reasonable number of delays
   *                            per schedule.
   * @param historySizeLimit    while exploring a schedule, the history may grow. This parameter sets a limit to the size
   *                            of the history. Note: use only even-number as a limit so that the history isn't truncated
   *                            in a way it makes it incomplete.
   * @param numOfSchedulesLimit the number of schedules allowed for a certain scheduler to generate before it exits
   *                            and returns the results. This is useful in cases where there are so big state space to
   *                            explore by the scheduler (that may run forever) or in cases where we know the system
   *                            won't terminate.
   * @return the list of benchmark results
   */
  def doBenchmark(alg: Algorithms.Value, sys: Systems.Value,
                  howManyTimes: Int = 5,
                  delays: Int = 3,
                  historySizeLimit: Int = 4048,
                  numOfSchedulesLimit: Int = Int.MaxValue,
                  numOfWrites: Int = 1,
                  numOfReads: Int = 2): Seq[BenchmarkResults] = {
    import Algorithms._
    require(howManyTimes >= 1, "Do you want to run this scheduler ZERO or less times? fine! :(")

    val (system, file) = getDistributedSystemInstance(sys)

    (1 to howManyTimes) map { _: Int =>
      val sch = alg match {
        case RANDOM =>
          new RandomScheduler(file.getPath, system, Int.MaxValue, benchmark = true,numOfSchedulesLimit)
        case EXHAUSTIVE =>
          new ExhaustiveDFSScheduler(file.getPath, system, true, false, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
        case DELAY_BOUNDED =>
          new DelayBoundedScheduler(file.getPath, system, true, false, delays, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
        case DPOR =>
          new DporScheduler(file.getPath, system, true, false, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
        case TRANS_DPOR =>
          new TransDporScheduler(file.getPath, system, true, false, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
        case IRED =>
          new IRedScheduler(file.getPath, system, true, false, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
        case LIVIOLA =>
          new LiViolaScheduler(file.getPath, system, true, false, Int.MaxValue, historySizeLimit, numOfSchedulesLimit)
      }

      if (alg != RANDOM) { // all DFS schedulers
        val theScheduler = sch.asInstanceOf[ExhaustiveDFSScheduler]
        // pick any mix
        theScheduler.schedule =
          // there are 3 writes
          {theScheduler.schedule.filter { r => theScheduler.isModificationCall(r._1) }.take(numOfWrites) ++
            // and 7 reads
            theScheduler.schedule.filterNot { r => theScheduler.isModificationCall(r._1) }.take(numOfReads)}

        if(theScheduler.isInstanceOf[TransDporScheduler] ||
          theScheduler.isInstanceOf[IRedScheduler] ||
          theScheduler.isInstanceOf[LiViolaScheduler]){
          // TD, IR, LV
          // init only enabled set
          theScheduler.currentState.setEnabledSet(theScheduler.schedule.toSet)
          theScheduler.currentState.asInstanceOf[TransDporState].setBacktrackingSet(Set(theScheduler.currentState.getEnabledSet.head))
        }
        else if (theScheduler.isInstanceOf[DporScheduler]){
          // DPOR
          // init both backtracking and enabled set
          theScheduler.currentState.setEnabledSet(theScheduler.schedule.toSet)
          theScheduler.currentState.asInstanceOf[DporState].setBacktrackingSet(theScheduler.schedule.toSet)
        }
        else{ // Exhaustive && Delay-Bounded
          theScheduler.currentState.setEnabledSet(theScheduler.schedule.toSet)
        }
        theScheduler.explore
        theScheduler.benchmarkStatistics.checkUniqueHistories
        theScheduler.benchmarkStatistics
      }
      else { // only RANDOM scheduler
        val scheduler = sch.asInstanceOf[RandomScheduler]
        scheduler.schedule = scheduler.schedule.filter { r => scheduler.isModificationCall(r._1) }.take(numOfWrites) ++
          scheduler.schedule.filterNot { r => scheduler.isModificationCall(r._1) }.take(numOfReads)
        sch.benchmarkStatistics.harness = sch.schedule.toList
        scheduler.enabledSet = scheduler.schedule.toSet
        scheduler.explore
        scheduler.benchmarkStatistics.checkUniqueHistories
        scheduler.benchmarkStatistics
      }
    } // map
  }


  def averageResults(listOFResults: Seq[BenchmarkResults]): BenchmarkResults = {
    listOFResults.foreach(_.checkUniqueHistories) // it calculates important info before averaging
    listOFResults.reduce(avgOf)
  }

  private def avgOfStopWatches(watch1: StopWatch, watch2: StopWatch): StopWatch = {
    val avg = StopWatch()
    avg.accumulator = (watch1.getAccumulatedTime + watch2.getAccumulatedTime) / 2
    avg
  }

  private def avgOf(br1: BenchmarkResults, br2: BenchmarkResults): BenchmarkResults = {
    val avg = BenchmarkResults(br1.sch)
    avg.harness = br1.harness
    avg.dsName = br1.dsName
    avg.numOfAgents = br1.numOfAgents
    avg.schedulerName = br1.schedulerName
    avg.schedules = br1.schedules // don't care in paper results
    avg.times = (br1.times zip br2.times) map { x => avgOfStopWatches(x._1, x._2) }
    avg.histories = br1.histories
    avg.uniqueHistories = br1.uniqueHistories
    avg.uniqueHistoriesTimes = (br1.uniqueHistoriesTimes zip br2.uniqueHistoriesTimes) map { x => avgOfStopWatches(x._1, x._2) }
//    avg.firstBuggyHistIdx = br1.firstBuggyHistIdx //Math.floorDiv(br1.firstBuggyHistIdx,br2.firstBuggyHistIdx)

    avg.elapsedTime = avgOfStopWatches(br1.elapsedTime, br2.elapsedTime)
    avg.numBuggyHistories = (br1.numBuggyHistories + br2.numBuggyHistories) / 2

    br1.numUniqueHistories
    br2.numUniqueHistories
    avg.numUniqueHist = (br1.numUniqueHist + br2.numUniqueHist) / 2

    avg.quorum = br1.quorum
    avg.retries = br1.retries
    avg.spec = br1.spec

    avg
  }

  //  def maxOfResults(listOfResults: List[BenchmarkResults]): BenchmarkResults = ???
  //
  //  def minOfResults(listOfResults: List[BenchmarkResults]): BenchmarkResults = ???
  //
  //  def medianOfResults(listOfResults: List[BenchmarkResults]): BenchmarkResults = ???

  /**
   * The list of results are formatted into a string and returned
   *
   * @param rowOfTuplesOfResults the different schedulers results for a specific benchmarked system.
   */
  def multiResultsString(rowOfTuplesOfResults: Seq[(BenchmarkResults, BenchmarkResults, BenchmarkResults)]): String = {
    val strBuilder = new StringBuilder
    // The lines in a benchmark row:
    // #UH
    strBuilder.append(rowOfTuplesOfResults.map { x => s"""${x._1.numUniqueHist} & ${x._2.numUniqueHist} & ${x._3.numUniqueHist}""" }.mkString(" & ") + """\\ \cline{2-23}"""+"\n")
    // #NL
    strBuilder.append(rowOfTuplesOfResults.map { x => s"""${x._1.numBuggyHistories} & ${x._2.numBuggyHistories} & ${x._3.numBuggyHistories}""" }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // #IH
    strBuilder.append(rowOfTuplesOfResults.map { x => s"""${x._1.numIncompleteHistories} & ${x._2.numIncompleteHistories} & ${x._3.numIncompleteHistories}""" }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // NL/S
    strBuilder.append(rowOfTuplesOfResults.map { x => s"""${x._1.numBuggyHistories / x._1.schedules.size} & ${x._2.numBuggyHistories / x._2.schedules.size} & ${x._3.numBuggyHistories / x._3.schedules.size}""" }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // TS
    strBuilder.append(rowOfTuplesOfResults.map { x =>
      val d1 = Duration(x._1.elapsedTime.getAccumulatedTime, "millis")
      val d2 = Duration(x._2.elapsedTime.getAccumulatedTime, "millis")
      val d3 = Duration(x._3.elapsedTime.getAccumulatedTime, "millis")
      s"""${d1.toMinutes + ":" + "%02d" format d1.toSeconds % 60} & ${d2.toMinutes + ":" + "%02d" format d2.toSeconds % 60} & ${d3.toMinutes + ":" + "%02d" format d3.toSeconds % 60}"""
    }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // ST
    strBuilder.append(rowOfTuplesOfResults.map { x =>
      val d1 = Duration(x._1.times.map { x => x.getAccumulatedTime }.sum, "millis")
      val d2 = Duration(x._2.times.map { x => x.getAccumulatedTime }.sum, "millis")
      val d3 = Duration(x._3.times.map { x => x.getAccumulatedTime }.sum, "millis")
      s"""${d1.toMinutes + ":" + "%02d" format d1.toSeconds % 60} & ${d2.toMinutes + ":" + "%02d" format d2.toSeconds % 60} & ${d3.toMinutes + ":" + "%02d" format d3.toSeconds % 60}"""
    }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // TC
    strBuilder.append(rowOfTuplesOfResults.map { x =>
      val d1 = Duration(x._1.uniqueHistoriesTimes.map { x => x.getAccumulatedTime }.sum, "millis")
      val d2 = Duration(x._2.uniqueHistoriesTimes.map { x => x.getAccumulatedTime }.sum, "millis")
      val d3 = Duration(x._3.uniqueHistoriesTimes.map { x => x.getAccumulatedTime }.sum, "millis")
      s"""${d1.toMinutes + ":" + "%02d" format d1.toSeconds % 60} & ${d2.toMinutes + ":" + "%02d" format d2.toSeconds % 60} & ${d3.toMinutes + ":" + "%02d" format d3.toSeconds % 60}"""
    }.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")
    // TT
    strBuilder.append(rowOfTuplesOfResults.map { x =>
      val tc1 =
        if (x._1.firstBuggyHistIdx > -1)
          x._1.uniqueHistoriesTimes.splitAt(x._1.firstBuggyHistIdx)._1 :+ x._1.uniqueHistoriesTimes(x._1.firstBuggyHistIdx)
        else Seq()
      val tc2 =
        if (x._2.firstBuggyHistIdx > -1)
          x._2.uniqueHistoriesTimes.splitAt(x._2.firstBuggyHistIdx)._1 :+ x._2.uniqueHistoriesTimes(x._2.firstBuggyHistIdx)
        else Seq()
      val tc3 =
        if (x._3.firstBuggyHistIdx > -1)
          x._3.uniqueHistoriesTimes.splitAt(x._3.firstBuggyHistIdx)._1 :+ x._3.uniqueHistoriesTimes(x._3.firstBuggyHistIdx)
        else Seq()

      val d1 = Duration(x._1.uniqueHistoriesTimes.map { y => y.getAccumulatedTime }.sum +
        tc1.map { y => y.getAccumulatedTime }.sum, "millis")
      val d2 = Duration(x._2.uniqueHistoriesTimes.map { y => y.getAccumulatedTime }.sum +
        tc2.map { y => y.getAccumulatedTime }.sum, "millis")
      val d3 = Duration(x._3.uniqueHistoriesTimes.map { y => y.getAccumulatedTime }.sum +
        tc3.map { y => y.getAccumulatedTime }.sum, "millis")
      s"""${d1.toMinutes + ":" + "%02d" format d1.toSeconds % 60} & ${d2.toMinutes + ":" + "%02d" format  d2.toSeconds % 60} & ${d3.toMinutes + ":" + "%02d" format d3.toSeconds % 60}"""}.mkString(" & ") + """\\ \cline{2-23}\n"""+"\n")

    // TF
    strBuilder.append(rowOfTuplesOfResults.map { x =>
      val tc1 =
        if (x._1.firstBuggyHistIdx > -1)
          x._1.uniqueHistoriesTimes.splitAt(x._1.firstBuggyHistIdx)._1 :+ x._1.uniqueHistoriesTimes(x._1.firstBuggyHistIdx)
        else Seq()
      val tc2 =
        if (x._2.firstBuggyHistIdx > -1)
          x._2.uniqueHistoriesTimes.splitAt(x._2.firstBuggyHistIdx)._1 :+ x._2.uniqueHistoriesTimes(x._2.firstBuggyHistIdx)
        else Seq()
      val tc3 =
        if (x._3.firstBuggyHistIdx > -1)
          x._3.uniqueHistoriesTimes.splitAt(x._3.firstBuggyHistIdx)._1 :+ x._3.uniqueHistoriesTimes(x._3.firstBuggyHistIdx)
        else Seq()

      val d1 = Duration(tc1.map { y => y.getAccumulatedTime }.sum, "millis")
      val d2 = Duration(tc2.map { y => y.getAccumulatedTime }.sum, "millis")
      val d3 = Duration(tc3.map { y => y.getAccumulatedTime }.sum, "millis")

      s"""${d1.toMinutes + ":" + "%02d" format d1.toSeconds % 60} & ${d2.toMinutes + ":" + "%02d" format d2.toSeconds % 60} & ${d3.toMinutes + ":" + "%02d" format d3.toSeconds % 60}"""
    }.mkString(" & ") + """\\ \hline\hline \n"""+"\n")

    // output to .tex file format (you need to manually copy paste the output to the big table to show up in the paper)
    strBuilder.toString
  }
}
