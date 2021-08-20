package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.benchmarks

import java.io.File

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks.WGL
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics.BenchmarkResults
import edu.utah.cs.gauss.ds2.core.time.StopWatch
import edu.utah.cs.gauss.serialization.IO

object OutputStatsToTex {

  implicit class OutputToTexTable(val bmss: Seq[BenchmarkResults]) {

    def numSchedules: String = bmss.map{x => x.numOfSchedules}.mkString(" & ")
    def numUniqueHistories: String = bmss.map{x => x.numUniqueHistories}.mkString(" & ")
    def numNoneLinearizable: String = bmss.map{x => x.checkUniqueHistories; x.numBuggyHistories}.mkString(" & ")
    def numIncompleteHistories: String = bmss.map{x => x.numIncompleteHistories}.mkString(" & ")
    def ratiosOfNoneLinearizableToNumSchedules: String = bmss.map{x => x.buggyToSchedulesRatio}.mkString(" & ")
    def timeToGenerateSchedules: String = bmss.map{x => x.elapsedTime.getAccumulatedTime}.mkString(" & ")
    def timeToFirstBuggySchedule: String =
      bmss.map { x =>
        val timeElapsed = StopWatch()
        val realHistories = x.uniqueHistories.map { x => Event.makeHistory(x) }
        timeElapsed.start
        realHistories.map { h => if (WGL(h).check == false) timeElapsed.stop }
        timeElapsed.getAccumulatedTime.toString
      }.mkString(" & ")
    def timeToCheckAllUniqueHistories: String =
      bmss.map{x =>
        val elapsedTime = StopWatch()
        val realHistories = x.uniqueHistories.map{x => Event.makeHistory(x)}
        elapsedTime.start
        realHistories.map{h => WGL(h).check}
        elapsedTime.stop
        elapsedTime.getAccumulatedTime.toString
      }.mkString(" & ")

    def numRetries: String = bmss.map{x => x.retries}.mkString(" & ")
    def quorum: String = bmss.map{x => x.quorum}.mkString(" & ")

    def writeResultsToFile(file: File): Unit ={
      IO.appendToFile(file.getPath,
        numSchedules,
        numUniqueHistories,
        numNoneLinearizable,
        numIncompleteHistories,
        ratiosOfNoneLinearizableToNumSchedules,
        numRetries,
        quorum,
        timeToGenerateSchedules,
        timeToFirstBuggySchedule,
        timeToCheckAllUniqueHistories
      )
    }
  }
}
