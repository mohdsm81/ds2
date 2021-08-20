package edu.utah.cs.gauss.ds2.core.schedulers

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import net.liftweb.json._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.schedulers.composable._
import edu.utah.cs.gauss.ds2.core.tracing.TraceManager
import scala.collection.mutable.{ Seq => MSeq }
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

/**
 * @author Mohammed S. Al-Mahfoudh
 * 		   mahfoudh@cs.utah.edu
 * 		   Gauss Group - SoC
 * 		   The University of Utah
 * The Scheduler DeSerializers from JSON. 
 * A user needs to extend and then override the method inside each DeSerializer. 
 * 
 * Forexample, implicit calss MyClass extends FromJsonScheduler
 *
 */

@SerialVersionUID(1200)
object DeSerializers {

  implicit class FromJsonScheduler(val sch: Scheduler) extends Serializable {

    def fromJson(js: JValue): Scheduler = {
      implicit val formats = DefaultFormats

      val ds = js \\ "ds" match {
        case x: JObject => DistributedSystem.fromJson(x)
        case _          => throw new Error("Scheduler.fromJson - can't extract 'ds'")
      }

      val numThreads = js \\ "numThreads" match {
        case JInt(x) => x
        case _       => throw new Error("Scheduler.fromJson - can't extract 'numThreads'")
      }

      //    val scheduelingAlgorithmName = js \\ "scheduingAlgorithm" match {
      //      case JString(x) => x
      //      case _          => throw new Error("Scheduler.fromJson - can't extract 'scheduingAlgorithm'")
      //    }

      val clock = js \\ "clock" match {
        case JInt(x) => x
        case _       => throw new Error("Scheduler.fromJson - can't extract 'clock'")
      }

      //    val taskQ: SchedulerMultiTaskQ = js \\ "taskQ" match {
      //      case JArray(x) => x map { y => SuspendableTask.fromJson(y) } 
      //      case _         => throw new Error("Scheduler.fromJson - can't extract 'clock'")
      //    }
      val taskQ: TaskQ = js \\ "taskQ" match {
        case x: JObject => TaskQ.fromJson(x)
        case _          => throw new Error("Scheduler.fromJson - can't extract 'clock'")
      }

      val consumeQ = js \\ "consumeQ" match {
        case JArray(x) => x map { y => Statement.fromJson(y) } // used to be SuspendableTask.fromJson(y)
        case _         => throw new Error("Scheduler.fromJson - can't extract 'consumeQ'")
      }

      val blockingMgr = js \\ "blockingMgr" match {
        case x: JObject => BlockedTasksManager.fromJson(x)
        case _          => throw new Error("Scheduler.fromJson - can't extract 'blockingMgr'")
      }

      val timedActionsTracker = js \\ "timedActionsTracker" match {
        case x: JObject => TimedActionsTracker.fromJson(x)
        case _          => throw new Error("Scheduler.fromJson - can't extract 'timedActionsTracker'")
      }

      val traceManager = js \\ "traceManager" match {
        case x: JObject => TraceManager.fromJson(x)
        case _          => throw new Error("Scheduler.fromJson - can't extract 'traceManager'")
      }

      // why the gymnastics? to remove the last character which is '$'
      //val scheduler = Scheduler(ds, decideAlg(scheduelingAlgorithmName.slice(0, scheduelingAlgorithmName.size - 1)))

      // now it is done reflectively, using Scala-support
      val scheduler = sch.getClass.newInstance

      // ds is set already
      scheduler.numThreads = numThreads.toInt
      // scheduling alg is set already

      scheduler.clk = clock

      scheduler.taskQ = taskQ //Seq(taskQ: _*)

      scheduler.consumeQ = MSeq(consumeQ: _*)

      scheduler.blockingMgr = blockingMgr

      scheduler.timedActionsTracker = timedActionsTracker

      scheduler.traceManager = traceManager

      scheduler
    }

  }

}
