package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.metrics

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.AbstractTypes.{Receive, Schedule}
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.Helpers
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.checks.WGL2
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.{Event, History}
import edu.utah.cs.gauss.ds2.core.time.StopWatch

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */
case class BenchmarkResults(sch: Scheduler with Helpers) {

  var harness: List[Receive] = List[Receive]()
  var dsName: String = ""
  var numOfAgents: Int = 0
  var schedulerName = ""
  var schedules: List[Schedule] = List()
  var times: List[StopWatch] = List()
  var timesToGenHistories: List[Int] = List()
  var histories: List[History] = List()
  var uniqueHistories: Set[Seq[Event]] = Set()
  var uniqueHistoriesTimes: Seq[StopWatch] = Seq()
  var firstBuggyHistIdx: Int = -1
  var numBuggyHistories: Int = 0
  var elapsedTime: StopWatch = StopWatch()
  var quorum: Double = 1
  var retries: Int = 1
  var spec: MMap[Any,Any] = MMap.empty[Any,Any]


  var numUniqueHist = 0


  def addSchedule(schedule: Schedule): Unit = schedules = schedules :+ schedule

  def addTime(sw: StopWatch): Unit = times = times :+ sw.copy

  def numOfSchedules: Int = schedules.size

  def addHistory(hist: History): Unit = {
    histories = histories :+ hist
    uniqueHistories = uniqueHistories + Event.historyToSeq(hist.copyAll)
  }

  def numUniqueHistories: Int = {
    numUniqueHist = uniqueHistories.size
    numUniqueHist
  }

  def uniqueToSchedulesRatio: Double =  scala.math.BigDecimal((numUniqueHistories.toDouble / numOfSchedules) * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  def buggyToSchedulesRatio: Double = scala.math.BigDecimal(numBuggyHistories.toDouble / numOfSchedules * 100).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble

  def buggyToUniqueRatio: Double = scala.math.BigDecimal(numBuggyHistories.toDouble / numUniqueHistories * 100).setScale(2,BigDecimal.RoundingMode.HALF_UP).toDouble

  def maxUniqueSchedulesPossibleForSystem: Int = schedules.toSet.size

  def generateHistories(): Unit = {
    histories = List.empty[History]
    uniqueHistoriesTimes = List.empty[StopWatch]

    schedules.foreach { x =>

      var hist = Seq[Event]()
      x.foreach {
        case x if sch.isInvocation(x._1) && (x._1.sender in sch.clients) => hist = hist :+  sch.makeInvocation(x._1, x._2)
        case x if sch.isResponse(x._1) && (x._2 in sch.clients) => hist = hist :+ sch.makeResponse(x._1, x._2)
        case _ => // do nothing
        //        println(x._1.name+", ")
      }
      uniqueHistories = uniqueHistories + hist
      val linkedHistory = Event.makeHistory(hist)
      histories = histories :+ linkedHistory
    }
  }

//  def generateHistories(): Unit = {
//    histories = List.empty[History]
//    uniqueHistoriesTimes = List.empty[StopWatch]
//
//    schedules.foreach { x =>
//
//      var hist = Seq[Event]()
//      x.foreach {
//        case x if sch.isInvocation(x._1) && x._1.sender.name.startsWith("IRed")=> hist = hist :+  sch.makeInvocation(x._1, x._2)
//        case x if sch.isResponse(x._1) && x._2.name.startsWith("IRed") => hist = hist :+ sch.makeResponse(x._1, x._2)
//        case _ => // do nothing
//        //        println(x._1.name+", ")
//      }
//      uniqueHistories = uniqueHistories + hist
//      val linkedHistory = Event.makeHistory(hist)
//      histories = histories :+ linkedHistory
//    }
//  }

  def checkUniqueHistories: Seq[(History, Boolean)] = {
    generateHistories()
    uniqueHistoriesTimes = List.empty[StopWatch]

    // generate histories from schedules (actual schedules)
    val sw = StopWatch()
    val setOfEvents: Seq[Event] = uniqueHistories.toSeq.map{ hs: Seq[Event] => Event.makeHistory(hs).copyAll }

    //-----------------------------
    // DEBUG
//    val setOfEvents: Seq[Event] = histories.map{ hs: Event => hs.copyAll.head }
    //-----------------------------

    val checkResults = setOfEvents.filter { x => Event.isComplete(x) }.map { h =>

    //-----------------------------
    // DEBUG
//    val checkResults = (setOfEvents zipWithIndex).filter(x => Event.isComplete(x._1)) map { case (h,idx) if Event.isComplete(h) =>
    //-----------------------------
//    val checkResults = setOfEvents.map{x =>
//      val strippedHist = Event.stripOffPadding(x)
//      if(Event.isComplete(strippedHist)) x else Event.doComplete(strippedHist)
//    }.map { h =>
      // start time here
      sw.start
      //-----------------------------
      // DEBUG
//      val historyCopy = h.copyAll
      //-----------------------------
      val history = Event.stripOffPadding(h) // removes the padding since WGL adds its own
      val alg = new WGL2(history.first)
      alg.spec = sch.spec
      val isLinearizable = alg.check

      //-----------------------------
      // DEBUG
//      if(!isLinearizable) {
//        println(historyCopy.mkString())
//        println(schedules(idx).mkString("\n"))
//        println("----------------------------------------------------------------------------")
//      }
      //-----------------------------

      // stop time
      sw.stop
      // add to an array of stop-watches
      uniqueHistoriesTimes = uniqueHistoriesTimes :+ sw.copy
      sw.reset
      if (!isLinearizable && firstBuggyHistIdx == -1) firstBuggyHistIdx = uniqueHistoriesTimes.size - 1
      (h, isLinearizable)
    }
    numBuggyHistories = checkResults.count { case (_, isLin) => !isLin }
    checkResults
  }

  def incompleteHistories: Seq[Event] = {
    val result = histories.filterNot { x => Event.isComplete( Event.stripOffPadding( x ).first ) }
    result // for debugging
  }
  def numIncompleteHistories: Int = {
    val result = incompleteHistories.size
    result // for debugging
  }
//
//  def numSchedulesTillFirstBug: Int = {
//    histories.map { x =>
//      val trimmed = Event.stripOffPadding(x).first
//      if(trimmed.first.size > 1) {
//        val wgl = WGL(trimmed.first)
//        wgl.spec = spec.clone()
//        val ans = wgl.check
//        ans
//      } else false
//    }.takeWhile(x => x).size
//  }

  def tt: FiniteDuration = Duration(elapsedTime.getAccumulatedTime + uniqueHistoriesTimes.map{ x => x.getAccumulatedTime}.sum , "millis")

  def statelessTime: FiniteDuration = Duration(times.map{ x => x.getAccumulatedTime}.sum, "millis")

  def tf: FiniteDuration = Duration( if(firstBuggyHistIdx > -1)
    (uniqueHistoriesTimes.splitAt(firstBuggyHistIdx)._1 :+
      uniqueHistoriesTimes(firstBuggyHistIdx)).
      map{x => x.getAccumulatedTime}.sum else 0, "millis")

  def tc: FiniteDuration = Duration(uniqueHistoriesTimes.map{ x => x.getAccumulatedTime}.sum, "millis")

  def duration: FiniteDuration = Duration(elapsedTime.getAccumulatedTime, "millis")

  // output
  //  def toTexTableRow: String = ???
  override def toString: String = {
    checkUniqueHistories

    s"""
       |Harness Size: ${harness.size}
       |System Name: $dsName
       |# of Agents: $numOfAgents
       |Scheduler: $schedulerName
       |# of Schedules: $numOfSchedules
       |# of Histories: ${histories.size}
       |Total Time to generate all schedules: ${duration.toMinutes}:${"%02d".format(duration.toSeconds % 60)}
       |Total Time to generate all schedules (stateless): ${statelessTime.toMinutes}:${"%02d".format(statelessTime.toSeconds % 60)}
       |# Unique Schedules: $maxUniqueSchedulesPossibleForSystem
       |# of Unique Histories: $numUniqueHistories
       |Time to Check all Unique Histories: ${tc.toMinutes}:${"%02d".format(tc.toSeconds % 60)}
       |Time to generate schedules and check unique histories (TT): ${tt.toMinutes}:${"%02d".format(tt.toSeconds % 60)}
       |Time to (including) first buggy unique history: ${tf.toMinutes}:${"%02d".format(tf.toSeconds % 60)}
       |# of Incomplete Histories: $numIncompleteHistories
       |Unique Histories to #Schedules ratio: $uniqueToSchedulesRatio%
       |Buggy Histories to Schedules Ratio: $buggyToSchedulesRatio%
       |Buggy to Unique Ratio: $buggyToUniqueRatio%
       |# Schedules till first bug: $firstBuggyHistIdx
       |""".stripMargin
  }
}
