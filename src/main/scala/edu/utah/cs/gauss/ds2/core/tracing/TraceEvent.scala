package edu.utah.cs.gauss.ds2.core.tracing

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Message
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import scala.language.postfixOps
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

/**
  *
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  *
  *
  */

@SerialVersionUID( 1900 )
trait TraceEvent extends Serializable {
  def toJson: JValue

  override def hashCode: Int = {
    getClass.getSimpleName.##
  }

  override def equals( that: Any ): Boolean = {
    hashCode() == that.hashCode()

  }

  def copy: TraceEvent = {
    // we need not do anything regarding events, they are constants detailed only
    // the only reason this one is here is to facilitate copying traces and TraceManager
    // No need to cross reference agents, we only need their names after all, its a trace.
    TraceEvent.fromJson( toJson )
  }

}
object TraceEvent {
  def fromJson( js: JValue ): TraceEvent = {
    implicit val formats = DefaultFormats

    val className = js match {
      case JObject( List( JField( x, _ ) ) ) ⇒ x
      case _                                 ⇒ throw new Error( "TraceEvent.fromJson -> can't extract the specific class name" )
    }

    // TODO add all the "faults" events to this object, and their equivalent classes/objects

    className match {
      case "Send"                    ⇒ Send.fromJson( js )
      case "Ask"                     ⇒ Ask.fromJson( js )
      case "Create"                  ⇒ Create.fromJson( js )
      case "Start"                   ⇒ Start.fromJson( js )
      case "Stop"                    ⇒ Stop.fromJson( js )
      case "Kill"                    ⇒ Kill.fromJson( js )
      case "Lock"                    ⇒ Lock.fromJson( js )
      case "Unlock"                  ⇒ Unlock.fromJson( js )
      case "StopConsuming"           ⇒ StopConsuming.fromJson( js )
      case "ResumeConsuming"         ⇒ ResumeConsuming.fromJson( js )
      case "Become"                  ⇒ Become.fromJson( js )
      case "Unbecome"                ⇒ Unbecome.fromJson( js )
      case "Stash"                   ⇒ Stash.fromJson( js )
      case "Unstash"                 ⇒ Unstash.fromJson( js )
      case "UnstashAll"              ⇒ UnstashAll.fromJson( js )
      case "HasWork"                 ⇒ HasWork.fromJson( js )
      case "BootStrap"               ⇒ BootStrap.fromJson( js )
      case "BootStrapAll"            ⇒ BootStrapAll.fromJson( js )
      case "Get"                     ⇒ Get.fromJson( js )
      case "GetTimeout"              ⇒ GetTimeout.fromJson( js )
      case "Resolve"                 ⇒ Resolve.fromJson( js )
      case "StopAll"                 ⇒ StopAll.fromJson( js )
      case "ShutdownAll"             ⇒ ShutdownAll.fromJson( js )
      case "AddAgent"                ⇒ AddAgent.fromJson( js )
      case "Pick"                    ⇒ Pick.fromJson( js )
      case "PickRandom"              ⇒ PickRandom.fromJson( js )
      case "ExecuteTimed"            ⇒ ExecuteTimed.fromJson( js )
      case "ExecuteStatement"        ⇒ ExecuteStatement.fromJson( js )
      case "ExecuteSpecific"         ⇒ ExecuteSpecific.fromJson( js )
      case "DoSchedule"              ⇒ DoSchedule.fromJson( js )
      case "DoSchedulePeriodic"      ⇒ DoSchedulePeriodic.fromJson( js )
      case "DoSchedulePeriodicRange" ⇒ DoSchedulePeriodicRange.fromJson( js )
      case "Consume"                 ⇒ Consume.fromJson( js )
      case "Execute"                 ⇒ Execute.fromJson( js )
      case "ExecuteAll"              ⇒ ExecuteAll.fromJson( js )
      case "ExecuteSpecial"          ⇒ ExecuteSpecial.fromJson( js )
      case "ExecuteOneStatement"     ⇒ ExecuteOneStatement.fromJson( js )
      case "ExecuteTimedAction"      ⇒ ExecuteTimedAction.fromJson( js )
      case "Tick"                    ⇒ Tick.fromJson( js )
      case "ExploreStart"            ⇒ ExploreStart.fromJson( js )
      case "ExploreEnd"              ⇒ ExploreEnd.fromJson( js )
      case "ShutdownScheduler"       ⇒ ShutdownScheduler.fromJson( js )
      case "Crashing"                ⇒ Crashing.fromJson(js)
      case "UnPartitioning"          ⇒ UnPartitioning.fromJson(js)
      case "Partitioning"            ⇒ Partitioning.fromJson(js)
      case "DuplicateMessage"        ⇒ DuplicateMessage.fromJson(js)
      case "DropMessage"             ⇒ DropMessage.fromJson(js)
      // TODO add these additional events one done with mixins
//      case "Interleaving" => Interleaving.fromJson(js)
//      case "ReOrdering" => ReOrdering.fromJson(js)
      //      case "ExecuteAny"              => ExecuteAny.fromJson(js)
      case _                         ⇒ throw new Error( "TraceEvent.fromJson - can't deserialize this event [" + className + "]" )
    }
  }
}

//======================================
// Faults trace entries
//======================================

//--------------
// Message Drop
//--------------

object DropMessage {
  def fromJson( js: JValue ): DropMessage = {
    val a = js \ "DropMessage" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "DropMessage.fromJson(js) -- can't extract an agent" )
    }

    val m = js \ "DropMessage" \ "m" match {
      case x: JObject ⇒ Message.fromJson( x )
      case _          ⇒ throw new Error( "DropMessage.fromJson(js) -- can't extract the message" )
    }

    val idx = js \ "DropMessage" \ "idx" match {
      case JInt( x ) ⇒ x.toInt
      case _         ⇒ throw new Error( "DropMessage.fromJson(js) -- can't extract index" )
    }

    implicit val clk = js \ "DropMessage" \ "clk" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DropMessage.fromJson(js) -- can't extract clk" )
    }
    DropMessage( a, idx, m )( () ⇒ clk )
  }
}

// note that I ommitted the "sink" since it is not part of the distributed system, rather part of some algorithm.
case class DropMessage( a: Agent, idx: Int, m: Message )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  override def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "idx" -> idx ) ~
      ( "m" -> m.toJson ) ~
      ( "clk" -> clk ) )
  }
}

//--------------
// Message Dupl.
//--------------

object DuplicateMessage {
  def fromJson( js: JValue ): DuplicateMessage = {
    val a = js \ "DuplicateMessage" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "DuplicateMessage.fromJson(js) -- can't extract an agent" )
    }

    val idx = js \ "DuplicateMessage" \ "idx" match {
      case JInt( x ) ⇒ x.toInt
      case _         ⇒ throw new Error( "DuplicateMessage.fromJson(js) -- can't extract index" )
    }

    val m = js \ "DuplicateMessage" \ "m" match {
      case x: JObject ⇒ Message.fromJson( x )
      case _          ⇒ throw new Error( "DuplicateMessage.fromJson(js) -- can't extract the message" )
    }

    implicit val clk = js \ "DuplicateMessage" \ "clk" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DuplicateMessage.fromJson(js) -- can't extract clk" )
    }
    DuplicateMessage( a, m, idx )( () ⇒ clk )
  }
}

case class DuplicateMessage( a: Agent, m: Message, idx: Int )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  override def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "m" -> m.toJson ) ~
      ( "idx" -> idx ) ~
      ( "clk" -> clk ) )
  }
}

//--------------
// Net Part
//--------------

object Partitioning {

  def fromJson( js: JValue ): Partitioning = {

    val part1 = js \ "Partitioning" \ "partNo1" match {
      case JArray( list ) ⇒ list map ( Agent.fromJson( _ ) ) toSet
      case _              ⇒ throw new Error( "Partitioning.fromJson(js) -- couldn't extract part1" )
    }

    val partNo1 = js \ "UnPartitioning" \ "part1" match {
      case JInt(x) ⇒ x
      case _         ⇒ throw new Error( "Partitioning.fromJson(js) -- couldn't extract partNo1" )
    }

    implicit val clk = js \ "Partitioning" \ "clk" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Partitioning.fromJson(js) -- can't extract clk" )
    }

    Partitioning( part1, partNo1)( () ⇒ clk )
  }
}

case class Partitioning( part1: Set[Agent], partNo1: BigInt )(implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  override def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "part1" -> part1.map( _.toJson ) ) ~
      ( "partNo1" -> partNo1 ) ~
      ( "clk" -> clk ) )
  }
}

//--------------
// Net UnPart
//--------------

object UnPartitioning {
  def fromJson( js: JValue ): UnPartitioning = {
    val part1 = js \ "UnPartitioning" \ "part1" match {
      case JArray( list ) ⇒ list map ( Agent.fromJson( _ ) ) toSet
      case _         ⇒ throw new Error( "Partitioning.fromJson(js) -- couldn't extract partNo1" )
    }

    val part2 = js \ "UnPartitioning" \ "part2" match {
      case JArray( list ) ⇒ list map ( Agent.fromJson( _ ) ) toSet
      case _         ⇒ throw new Error( "Partitioning.fromJson(js) -- couldn't extract partNo2" )
    }

    val clk = js \ "UnPartitioning" \ "clk" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "UnPartitioning.fromJson(js) -- can't extract clk" )
    }

    UnPartitioning( part1,part2 )( () ⇒ clk )
  }
}

case class UnPartitioning( part1: Set[Agent], part2: Set[Agent] )(implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  override def toJson: JValue = {
    ( getClass.getSimpleName ->
       ( "part1" -> part1.map( _.toJson ) ) ~
       ( "part2" -> part2.map( _.toJson ) ) ~
      ( "clk" -> clk ) )
  }

}

//--------------
// Crash
//--------------

object Crashing {

  def fromJson( js: JValue ): Crashing = {
    val a = js \ "Crashing" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Crashing.fromJson(js) -- can't extract the agent" )
    }

    val clk = js \ "Crashing" \ "clk" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Crashing.fromJson(js) -- can't extract clk" )
    }

    Crashing( a )( () ⇒ clk )
  }
}

case class Crashing( a: Agent )(implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  override def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clk" -> clk ) )
  }
}

//--------------
// Interleaving
//--------------

// TODO fill these later once you get to code the mixins

//--------------
// ReOrdering
//--------------

// TODO fill these later once you get to code the mixins

//======================================
// Communication trace entries
//======================================

//--------------
// SEND
//--------------

object Send {
  def fromJson( js: JValue ): Send = {
    implicit val formats = DefaultFormats

    val src = js \ "Send" \ "src" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case x ⇒
        //DEBUG
        //        println(x)
        throw new Error( "Send - can't extract 'src'" )
    }

    val m = js \ "Send" \ "m" match {
      case x: JObject ⇒ Message.fromJson( x )
      case _          ⇒ throw new Error( "Send - can't extract 'm'" )
    }

    val dst = js \ "Send" \ "dst" match {
      case x: Object ⇒ Agent.fromJson( x )
      case _         ⇒ throw new Error( "Send - can't extract 'dst'" )
    }

    val special = js \ "Send" \ "special" match {
      case JBool( x ) ⇒ x
      case _          ⇒ throw new Error( "Send - can't extract 'special'" )
    }

    val clock = js \ "Send" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Send - can't extract 'clock'" )
    }

    val e = Send( src, m, dst, special )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Send( src: Agent, m: Message, dst: Agent, special: Boolean = false )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {

    ( getClass.getSimpleName ->
      ( "src" -> src.toJson ) ~
      ( "m" -> m.toJson ) ~
      ( "dst" -> dst.toJson ) ~
      ( "special" -> special ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// ASK
//-------------
object Ask {
  def fromJson( js: JValue ): Ask = {
    implicit val formats = DefaultFormats
    //    js.extract[Ask]

    val src = js \ "Ask" \ "src" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Ask - can't extract 'src'" )
    }

    val m = js \ "Ask" \ "m" match {
      case x: JObject ⇒ Message.fromJson( x )
      case _          ⇒ throw new Error( "Ask - can't extract 'm'" )
    }

    val dst = js \ "Ask" \ "dst" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Ask - can't extract 'dst'" )
    }

    val f = js \ "Ask" \ "f" match {
      case x: JObject ⇒ DummyFuture.fromJson( x )
      case _          ⇒ throw new Error( "Ask - can't extract 'f'" )
    }

    val special = js \ "Ask" \ "special" match {
      case JBool( x ) ⇒ x
      case _          ⇒ throw new Error( "Ask - can't extract 'special'" )
    }

    val clock = js \ "Ask" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Ask - can't extract 'clock'" )
    }

    val e = Ask( src, m, dst, f, special )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Ask( src: Agent, m: Message, dst: Agent, f: DummyFuture, special: Boolean = false )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {

    f.synchronized {
      ( getClass.getSimpleName ->
        ( "src" -> src.toJson ) ~
        ( "m" -> m.toJson ) ~
        ( "dst" -> dst.toJson ) ~
        ( "f" -> f.toJson ) ~
        ( "special" -> special ) ~
        ( "clock" -> clk ) )
    }
  }
}

//-------------
// CREATE
//-------------

object Create {
  def fromJson( js: JValue ): Create = {
    implicit val formats = DefaultFormats
    //    js.extract[Create]
    val p = js \ "Create" \ "p" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Create - can't extract 'p'" )
    }

    val created = js \ "Create" \ "created" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Create - can't extract 'created'" )
    }

    val name = js \ "Create" \ "name" match {
      case JString( x ) ⇒ x
      case _            ⇒ throw new Error( "Create - can't extract 'name'" )
    }

    val clock = js \ "Create" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Create - can't extract 'clock'" )
    }

    val e = Create( p, name, created )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Create( p: Agent, nameOfNewOne: String, created: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {

    ( getClass.getSimpleName ->
      ( "p" -> p.toJson ) ~
      ( "created" -> created.toJson ) ~
      ( "name" -> nameOfNewOne ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// START
//-------------

object Start {
  def fromJson( js: JValue ): Start = {
    implicit val formats = DefaultFormats
    //    js.extract[Start]

    val starter = js \ "Start" \ "starter" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Start - can't extract 'starter'" )
    }

    val started = js \ "Start" \ "started" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Start - can't extract 'started'" )
    }

    val clock = js \ "Start" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Start - can't extract 'clock'" )
    }

    val e = Start( starter, started )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Start( starter: Agent, started: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "starter" -> starter.toJson ) ~
      ( "started" -> started.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// STOP
//-------------

object Stop {
  def fromJson( js: JValue ): Stop = {
    implicit val formats = DefaultFormats
    //    js.extract[Stop]
    val stopper = js \ "Stop" \ "stopper" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Stop - can't extract 'stopper'" )
    }

    val stopped = js \ "Stop" \ "stopped" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Stop - can't extract 'stopped'" )
    }

    val clock = js \ "Stop" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Stop - can't extract 'clock'" )
    }

    val e = Stop( stopper, stopped )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Stop( stopper: Agent, stopped: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "stopper" -> stopper.toJson ) ~
      ( "stopped" -> stopped.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// KILL
//-------------

object Kill {
  def fromJson( js: JValue ): Kill = {
    implicit val formats = DefaultFormats
    //    js.extract[Kill]
    val killer = js \ "Kill" \ "killer" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Kill - can't extract 'killer'" )
    }

    val victim = js \ "Kill" \ "victim" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Kill - can't extract 'victim'" )
    }

    val clock = js \ "Kill" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Kill - can't extract 'clock'" )
    }

    val e = Kill( killer, victim )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Kill( killer: Agent, victim: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "killer" -> killer.toJson ) ~
      ( "victim" -> victim.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// KILL
//-------------

object Lock {
  def fromJson( js: JValue ): Lock = {
    implicit val formats = DefaultFormats
    //    js.extract[Lock]

    val a = js \ "Lock" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Lock - can't extract 'a'" )
    }

    val clock = js \ "Lock" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Lock - can't extract 'clock'" )
    }

    val e = Lock( a )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Lock( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// UNLOCK
//-------------

object Unlock {
  def fromJson( js: JValue ): Unlock = {
    implicit val formats = DefaultFormats
    //    js.extract[Unlock]

    val a = js \ "Unlock" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Unlock - can't extract 'a'" )
    }

    val clock = js \ "Unlock" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Unlock - can't extract 'clock'" )
    }

    val e = Unlock( a )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Unlock( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// STOP-CONSUMING
//-------------

object StopConsuming {
  def fromJson( js: JValue ): StopConsuming = {
    implicit val formats = DefaultFormats
    //    js.extract[StopConsuming]

    val a = js \ "StopConsuming" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "StopConsuming - can't extract 'a'" )
    }

    val clock = js \ "StopConsuming" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "StopConsuming - can't extract 'clock'" )
    }

    val e = StopConsuming( a )( () ⇒ clock )

    e.clk = clock
    e
  }
}
case class StopConsuming( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// RESUME-CONSUMING
//-------------

object ResumeConsuming {
  def fromJson( js: JValue ): ResumeConsuming = {
    implicit val formats = DefaultFormats
    //    js.extract[ResumeConsuming]

    val a = js \ "ResumeConsuming" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "ResumeConsuming - can't extract 'a'" )
    }

    val clock = js \ "ResumeConsuming" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ResumeConsuming - can't extract 'clock'" )
    }
    val e = ResumeConsuming( a )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class ResumeConsuming( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// BECOME
//-------------

object Become {
  def fromJson( js: JValue ): Become = {
    implicit val formats = DefaultFormats
    //    js.extract[Become]

    val a = js \ "Become" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Become - can't extract 'a'" )
    }

    val b = js \ "Become" \ "b" match {
      case JString( x ) ⇒ x
      case _            ⇒ throw new Error( "Become - can't extract 'b'" )
    }

    val remember = js \ "Become" \ "remember" match {
      case JBool( x ) ⇒ x
      case _          ⇒ throw new Error( "Become - can't extract 'remember'" )
    }

    val clock = js \ "Become" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Become - can't extract 'clock'" )
    }

    val e = Become( a, b, remember )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Become( a: Agent, b: String, remember: Boolean = false )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "b" -> b ) ~
      ( "remember" -> remember ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// UNBECOME
//-------------

object Unbecome {
  def fromJson( js: JValue ): Unbecome = {
    implicit val formats = DefaultFormats
    //    js.extract[Unbecome]

    val a = js \ "Unbecome" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Unbecome - can't extract 'a'" )
    }

    val clock = js \ "Unbecome" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Unbecome - can't extract 'clock'" )
    }

    val e = Unbecome( a )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Unbecome( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// STASH
//-------------

object Stash {
  def fromJson( js: JValue ): Stash = {
    implicit val formats = DefaultFormats
    //    js.extract[Stash]

    val a = js \ "Stash" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Stash - can't extract 'a'" )
    }

    val m = js \ "Stash" \ "m" match {
      case x: JObject ⇒ Message.fromJson( x )
      case _          ⇒ throw new Error( "Stash - can't extract 'm'" )
    }
    val clock = js \ "Stash" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Stash - can't extract 'clock'" )
    }

    val e = Stash( a, m )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Stash( a: Agent, m: Message )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "m" -> m.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// UNSTASH
//-------------

object Unstash {
  def fromJson( js: JValue ): Unstash = {
    implicit val formats = DefaultFormats
    //    js.extract[Unstash]

    val a = js \ "Unstash" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Unstash - can't extract 'a'" )
    }

    val clock = js \ "Unstash" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Unstash - can't extract 'clock'" )
    }

    val e = Unstash( a )( () ⇒ clock )
    e.clk = clock
    e
  }
}

case class Unstash( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// UNSTASH-ALL
//-------------

object UnstashAll {
  def fromJson( js: JValue ): UnstashAll = {
    implicit val formats = DefaultFormats
    //    js.extract[UnstashAll]

    val a = js \ "UnstashAll" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "UnstashAll - can't extract 'a'" )
    }

    val clock = js \ "UnstashAll" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "UnstashAll - can't extract 'clock'" )
    }
    val e = UnstashAll( a )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class UnstashAll( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// HAS-WORK
//-------------

object HasWork {
  def fromJson( js: JValue ): HasWork = {
    implicit val formats = DefaultFormats
    //    js.extract[HasWork]
    val agents = js \ "HasWork" \ "agents" match {
      case JArray( x ) ⇒ x map { y ⇒ Agent.fromJson( y ) }
      case _           ⇒ throw new Error( "HasWork - can't extract 'agents'" )
    }

    val clock = js \ "HasWork" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "HasWork - can't extract 'clock'" )
    }

    val e = HasWork( agents.toSet )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class HasWork( agents: Set[Agent] )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "agents" -> agents.map { x ⇒ x.toJson } ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// BOOT-STRAP
//-------------

object BootStrap {
  def fromJson( js: JValue ): BootStrap = {
    implicit val formats = DefaultFormats
    //    js.extract[BootStrap]
    val a = js \ "BootStrap" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "BootStrap - can't extract 'a'" )
    }

    val clock = js \ "BootStrap" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "BootStrap - can't extract 'clock'" )
    }

    val e = BootStrap( a )( () ⇒ clock )

    e.clk = clock
    e

  }
}
case class BootStrap( a: Agent )( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// BOOT-STRAP-ALL
//-------------

object BootStrapAll {
  def fromJson( js: JValue ): BootStrapAll = {
    implicit val formats = DefaultFormats
    //    js.extract[BootStrapAll]

    val agents = js \ "BootStrapAll" \ "agents" match {
      case JArray( x ) ⇒ x map { y ⇒ Agent.fromJson( y ) }
      case _           ⇒ throw new Error( "BootStrapAll - can't extract 'agents'" )
    }

    val clock = js \ "BootStrapAll" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "BootStrapAll - can't extract 'clock'" )
    }

    val e = BootStrapAll( agents.toSet )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class BootStrapAll( agents: Set[Agent] )( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "agents" -> agents.map { x ⇒ x.toJson } ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// GET
//-------------

object Get {
  def fromJson( js: JValue ): Get = {
    implicit val formats = DefaultFormats
    //    js.extract[Get]

    val agent = js \ "Get" \ "agent" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Get - can't extract 'agent'" )
    }

    val future = js \ "Get" \ "future" match {
      case x: JObject ⇒ DummyFuture.fromJson( x )
      case _          ⇒ throw new Error( "Get - can't extract 'future'" )
    }

    val act = js \ "Get" \ "act" match {
      case x: JObject ⇒ Action.fromJson( x )
      case _          ⇒ throw new Error( "Get - can't extract 'act'" )
    }

    val clock = js \ "Get" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Get - can't extract 'clock'" )
    }

    import edu.utah.cs.gauss.serialization.IO.fromBytes
    val resultBytes = js \ "Get" \ "result" match {
      case JArray( x ) ⇒ x map { y ⇒
        y match {
          case JInt( z ) ⇒ z.toByte
          case _         ⇒ throw new Error( "Get - can't extract a byte from 'result'" )
        }
      }
      case _ ⇒ throw new Error( "Get - can't extract 'result'" )
    }

    val result = fromBytes[Option[Any]]( resultBytes.toSeq )

    val e = Get( agent, future, act, result )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Get( agent: Agent, future: DummyFuture, act: Action, result: Option[Any] )( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {

    import edu.utah.cs.gauss.serialization.IO.toBytes

    ( getClass.getSimpleName ->
      ( "agent" -> agent.toJson ) ~
      ( "future" -> future.toJson ) ~
      ( "act" -> act.toJson ) ~
      ( "result" -> JArray( toBytes( result ).toList map { x ⇒ JInt( x ) } ) ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// GET-TIMEOUT
//-------------

object GetTimeout {
  def fromJson( js: JValue ): GetTimeout = {
    implicit val formats = DefaultFormats
    //    js.extract[GetTimeout]
    val agent = js \ "GetTimeout" \ "agent" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "GetTimeout - can't extract 'agent'" )
    }

    val future = js \ "GetTimeout" \ "future" match {
      case x: JObject ⇒ DummyFuture.fromJson( x )
      case _          ⇒ throw new Error( "GetTimeout - can't extract 'future'" )
    }

    val act = js \ "GetTimeout" \ "act" match {
      case x: JObject ⇒ Action.fromJson( x )
      case _          ⇒ throw new Error( "GetTimeout - can't extract 'act'" )
    }

    val clock = js \ "GetTimeout" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "GetTimeout - can't extract 'clock'" )
    }

    import edu.utah.cs.gauss.serialization.IO.fromBytes
    val resultBytes = js \ "GetTimeout" \ "result" match {
      case JArray( x ) ⇒ x map { y ⇒
        y match {
          case JInt( z ) ⇒ z.toByte
          case _         ⇒ throw new Error( "GetTimeout - can't extract a byte from 'result'" )
        }
      }
      case _ ⇒ throw new Error( "GetTimeout - can't extract 'result'" )
    }

    val result = fromBytes[Option[Any]]( resultBytes.toSeq )

    val timeout = js \ "GetTimeout" \ "timeout" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "GetTimeout - can't extract 'timeout'" )
    }

    val e = GetTimeout( agent, future, timeout, act, result )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class GetTimeout( agent: Agent, future: DummyFuture, timeout: BigInt, act: Action, result: Option[Any] )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    import edu.utah.cs.gauss.serialization.IO.toBytes
    ( getClass.getSimpleName ->
      ( "agent" -> agent.toJson ) ~
      ( "future" -> future.toJson ) ~
      ( "timeout" -> timeout ) ~
      ( "act" -> act.toJson ) ~
      ( "result" -> JArray( toBytes( result ).toList map { x ⇒ JInt( x ) } ) ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// Resolve
//-------------

object Resolve {
  def fromJson( js: JValue ): Resolve = {
    implicit val formats = DefaultFormats
    //    js.extract[Resolve]

    val future = js \ "Resolve" \ "future" match {
      case x: JObject ⇒ DummyFuture.fromJson( x )
      case _          ⇒ throw new Error( "Resolve - can't extract 'future'" )
    }

    val clock = js \ "Resolve" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Resolve - can't extract 'clock'" )
    }

    val e = Resolve( future )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class Resolve( future: DummyFuture )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "future" -> future.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// STOP-ALL
//-------------

object StopAll {
  def fromJson( js: JValue ): StopAll = {
    implicit val formats = DefaultFormats
    //    js.extract[StopAll]

    val clock = js \ "StopAll" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "StopAll - can't extract 'clock'" )
    }

    val e = StopAll()( () ⇒ clock )

    e.clk = clock
    e
  }
}
case class StopAll()( implicit val clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "clock" -> clk ) )
  }
}

//-------------
// SHUTDOWN-ALL
//-------------

object ShutdownAll {
  def fromJson( js: JValue ): ShutdownAll = {
    implicit val formats = DefaultFormats
    //    js.extract[ShutdownAll]

    val clock = js \ "ShutdownAll" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ShutdownAll - can't extract 'clock'" )
    }

    val e = ShutdownAll()( () ⇒ clock )

    e.clk = clock
    e

  }
}
case class ShutdownAll()( implicit val clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "clock" -> clk ) )
  }
}

//-------------
// SHUTDOWN-ALL
//-------------

object AddAgent {
  def fromJson( js: JValue ): AddAgent = {
    implicit val formats = DefaultFormats
    //    js.extract[AddAgent]

    val a = js \ "AddAgent" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "AddAgent - can't extract 'a'" )
    }

    val clock = js \ "AddAgent" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "AddAgent - can't extract 'clock'" )
    }

    val e = AddAgent( a )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class AddAgent( a: Agent )( implicit val clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//======================================
// Scheduler Entries
//======================================

//-------------
// PICK
//-------------
object Pick {
  def fromJson( js: JValue ): Pick = {
    implicit val formats = DefaultFormats
    //    js.extract[Pick]

    val a = js \ "Pick" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "Pick - can't extract 'a'" )
    }

    val task = js \ "Pick" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "Pick - can't extract 'task'" )
    }

    val clock = js \ "Pick" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Pick - can't extract 'clock'" )
    }

    val e = Pick( a, task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class Pick( a: Agent, task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// PICK-RANDOM
//-------------
object PickRandom {
  def fromJson( js: JValue ): PickRandom = {
    implicit val formats = DefaultFormats
    //    js.extract[PickRandom]

    val task = js \ "PickRandom" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "PickRandom - can't extract 'task'" )
    }

    val clock = js \ "PickRandom" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "PickRandom - can't extract 'clock'" )
    }

    val e = PickRandom( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class PickRandom( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// EXECUTE-TIMED
//-------------

object ExecuteTimed {
  def fromJson( js: JValue ): ExecuteTimed = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteTimed]

    val timedAction = js \ "ExecuteTimed" \ "timedAction" match {
      case x: JObject ⇒ TimedAction.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteTimed - can't extract 'timedAction'" )
    }

    val threadID = js \ "ExecuteTimed" \ "threadID" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteTimed - can't extract 'threadID'" )
    }

    val clock = js \ "ExecuteTimed" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteTimed - can't extract 'clock'" )
    }

    val e = ExecuteTimed( timedAction, threadID.toLong )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExecuteTimed(timedAction: TimedAction, threadID: Long )(implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "timedAction" -> timedAction.toJson ) ~
      ( "threadID" -> threadID ) ~
      ( "clock" -> clk ) )
  }
}

////-------------
//// EXECUTE- ANY Task
////-------------
//
//object ExecuteAny {
//  def fromJson(js: JValue): ExecuteAny = {
//    implicit val formats = DefaultFormats
//    //    js.extract[ExecuteAny]
//
//    val task = js \ "ExecuteAny" \ "task" match {
//      case x: JObject => SuspendableTask.fromJson(x)
//      case _          => throw new Error("ExecuteAny - can't extract 'task'")
//    }
//
//    val threadID = js \ "ExecuteAny" \ "threadID" match {
//      case JInt(x) => x
//      case _       => throw new Error("ExecuteAny - can't extract 'threadID'")
//    }
//
//    val clock = js \ "ExecuteAny" \ "clock" match {
//      case JInt(x) => x
//      case _       => throw new Error("ExecuteAny - can't extract 'clock'")
//    }
//
//    val e = new ExecuteAny(task, threadID.toLong)(() => clock)
//    e.clk = clock
//    e
//
//  }
//}
//case class ExecuteAny(task: SuspendableTask, threadID: Long)(implicit clock: () => BigInt) extends TraceEvent {
//  var clk = clock()
//  def toJson: JValue = {
//    (getClass.getSimpleName ->
//      ("task" -> task.toJson) ~
//      ("threadID" -> threadID) ~
//      ("clock" -> clk))
//  }
//}
////-------------
//// EXECUTE- ANY STATEMENT
////-------------
//
//object ExecuteAny {
//  def fromJson(js: JValue): ExecuteAny = {
//    implicit val formats = DefaultFormats
//    //    js.extract[ExecuteAny]
//
//    val stmt = js \ "ExecuteAny" \ "stmt" match {
//      case x: JObject => Statement.fromJson(x)
//      case _          => throw new Error("ExecuteAny - can't extract 'task'")
//    }
//
//    val threadID = js \ "ExecuteAny" \ "threadID" match {
//      case JInt(x) => x
//      case _       => throw new Error("ExecuteAny - can't extract 'threadID'")
//    }
//
//    val clock = js \ "ExecuteAny" \ "clock" match {
//      case JInt(x) => x
//      case _       => throw new Error("ExecuteAny - can't extract 'clock'")
//    }
//
//    val e = ExecuteAny(stmt, threadID.toLong)(() => clock)
//    e.clk = clock
//    e
//
//  }
//}
//case class ExecuteAny(stmt: Statement, threadID: Long)(implicit clock: () => BigInt) extends TraceEvent {
//  var clk = clock()
//  def toJson: JValue = {
//    (getClass.getSimpleName ->
//      ("task" -> stmt.toJson) ~
//      ("threadID" -> threadID) ~
//      ("clock" -> clk))
//  }
//}

//-------------
// EXECUTE- Statement
//-------------

object ExecuteStatement {
  def fromJson( js: JValue ): ExecuteStatement = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteAny]

    val stmt = js \ "ExecuteStatement" \ "stmt" match {
      case x: JObject ⇒ Statement.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteAny - can't extract 'statement'" )
    }

    val threadID = js \ "ExecuteStatement" \ "threadID" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteStatement - can't extract 'threadID'" )
    }

    val clock = js \ "ExecuteStatement" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteStatement - can't extract 'clock'" )
    }

    val e = ExecuteStatement( stmt, threadID.toLong )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExecuteStatement( stmt: Statement, threadID: Long )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "stmt" -> stmt.toJson ) ~
      ( "threadID" -> threadID ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE-SPECIFIC
//-------------
object ExecuteSpecific {
  def fromJson( js: JValue ): ExecuteSpecific = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteSpecific]
    val a = js \ "ExecuteSpecific" \ "a" match {
      case x: JObject ⇒ Agent.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteSpecific - can't extract 'a'" )
    }

    val task = js \ "ExecuteSpecific" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteSpecific - can't extract 'task'" )
    }

    val threadID = js \ "ExecuteSpecific" \ "threadID" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteSpecific - can't extract 'threadID'" )
    }

    val clock = js \ "ExecuteSpecific" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteSpecific - can't extract 'clock'" )
    }

    val e = new ExecuteSpecific( a, task, threadID.toLong )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class ExecuteSpecific( a: Agent, task: SuspendableTask, threadID: Long )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "a" -> a.toJson ) ~
      ( "task" -> task.toJson ) ~
      ( "threadID" -> threadID ) ~
      ( "clock" -> clk ) )
  }

}

//-------------
// DO-SCHEDULE
//-------------

object DoSchedule {
  def fromJson( js: JValue ): DoSchedule = {
    implicit val formats = DefaultFormats
    //    js.extract[DoSchedule]

    val task = js \ "DoSchedule" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "DoSchedule - can't extract 'task'" )
    }

    val clock = js \ "DoSchedule" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedule - can't extract 'clock'" )
    }

    val e = new DoSchedule( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class DoSchedule( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}
//-------------
// DO-SCHEDULE-PERIODIC
//-------------

object DoSchedulePeriodic {
  def fromJson( js: JValue ): DoSchedulePeriodic = {
    implicit val formats = DefaultFormats
    //    js.extract[DoSchedulePeriodic]
    val task = js \ "DoSchedulePeriodic" \ "task" match {
      case x: JObject ⇒ TimedAction.fromJson( x )
      case _          ⇒ throw new Error( "DoSchedulePeriodic - can't extract 'task'" )
    }

    val clock = js \ "DoSchedulePeriodic" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedulePeriodic - can't extract 'clock'" )
    }

    val e = new DoSchedulePeriodic( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class DoSchedulePeriodic( task: TimedAction )(implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// DO-SCHEDULE-PERIODIC
//-------------

object DoSchedulePeriodicRange {
  def fromJson( js: JValue ): DoSchedulePeriodicRange = {
    implicit val formats = DefaultFormats
    //    js.extract[DoSchedulePeriodicRange]

    val task = js \ "DoSchedulePeriodicRange" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "DoSchedulePeriodicRange - can't extract 'task'" )
    }

    val n = js \ "DoSchedulePeriodicRange" \ "n" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedulePeriodicRange - can't extract 'clock'" )
    }

    val c1 = js \ "DoSchedulePeriodicRange" \ "c1" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedulePeriodicRange - can't extract 'clock'" )
    }

    val c2 = js \ "DoSchedulePeriodicRange" \ "c2" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedulePeriodicRange - can't extract 'clock'" )
    }

    val clock = js \ "DoSchedulePeriodicRange" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "DoSchedulePeriodicRange - can't extract 'clock'" )
    }

    val e = new DoSchedulePeriodicRange( task, n.toInt, c1, c2 )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class DoSchedulePeriodicRange( task: SuspendableTask, n: Int, c1: BigInt, c2: BigInt )( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "n" -> n ) ~
      ( "c1" -> c1 ) ~
      ( "c2" -> c2 ) ~
      ( "clock" -> clk ) )
  }

}
//-------------
// CONSUME
//-------------
object Consume {
  def fromJson( js: JValue ): Consume = {
    implicit val formats = DefaultFormats
    //    js.extract[Consume]

    val task = js \ "Consume" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "Consume - can't extract 'task'" )
    }

    val clock = js \ "Consume" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Consume - can't extract 'clock'" )
    }

    val e = new Consume( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class Consume( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE
//-------------

/**
  * Execute something from the front of Scheduler.consumeQ
  *
  */
object Execute {
  def fromJson( js: JValue ): Execute = {
    implicit val formats = DefaultFormats
    //    js.extract[Execute]

    val task = js \ "Execute" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "Execute - can't extract 'task'" )
    }

    val clock = js \ "Execute" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Execute - can't extract 'clock'" )
    }

    val e = new Execute( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class Execute( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE-ALL
//-------------

/**
  * Execute a bench (collection) of tasks
  *
  */
object ExecuteAll {
  def fromJson( js: JValue ): ExecuteAll = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteAll]

    val stmts = js \ "ExecuteAll" \ "stmts" match {
      case JArray( x ) ⇒ x map { y ⇒ Statement.fromJson( y ) }
      case _           ⇒ throw new Error( "ExecuteAll - can't extract 'tasks'" )
    }

    val clock = js \ "ExecuteAll" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteAll - can't extract 'clock'" )
    }

    val tid = js \ "ExecuteAll" \ "tid" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteAll - can't extract 'tid'" )
    }

    val e = new ExecuteAll( stmts, tid )( () ⇒ clock )
    e.clk = clock
    e
  }
}
case class ExecuteAll( stmts: Seq[Statement], tid: BigInt )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "stmts" -> stmts.map { x ⇒ x.toJson } ) ~
      ( "tid" -> tid ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE-SPECIAL
//-------------

/**
  * Execute a special action task.
  *
  */
object ExecuteSpecial {
  def fromJson( js: JValue ): ExecuteSpecial = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteSpecial]

    val task = js \ "ExecuteSpecial" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteSpecial - can't extract 'task'" )
    }

    val clock = js \ "ExecuteSpecial" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteSpecial - can't extract 'clock'" )
    }

    val e = new ExecuteSpecial( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExecuteSpecial( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {
  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE-SPECIAL
//-------------

object ExecuteOneStatement {
  def fromJson( js: JValue ): ExecuteOneStatement = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteOneStatement]
    val task = js \ "ExecuteOneStatement" \ "task" match {
      case x: JObject ⇒ SuspendableTask.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteOneStatement - can't extract 'task'" )
    }

    val clock = js \ "ExecuteOneStatement" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteOneStatement - can't extract 'clock'" )
    }

    val e = new ExecuteOneStatement( task )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExecuteOneStatement( task: SuspendableTask )( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()
  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "task" -> task.toJson ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// EXECUTE-TIMED-ACTION
//-------------

object ExecuteTimedAction {
  def fromJson( js: JValue ): ExecuteTimedAction = {
    implicit val formats = DefaultFormats
    //    js.extract[ExecuteTimedAction]

    val timedAction = js \ "ExecuteTimedAction" \ "timedAction" match {
      case x: JObject ⇒ TimedAction.fromJson( x )
      case _          ⇒ throw new Error( "ExecuteTimedAction - can't extract 'task'" )
    }

    val clock = js \ "ExecuteTimedAction" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExecuteTimedAction - can't extract 'clock'" )
    }

    val e = new ExecuteTimedAction( timedAction )( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExecuteTimedAction( timedAction: TimedAction )(implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "timedAction" -> timedAction.toJson ) ~
      ( "clock" -> clk ) )
  }
}

object Tick {
  def fromJson( js: JValue ): Tick = {
    implicit val formats = DefaultFormats
    //    js.extract[Tick]

    val triggered = js \ "Tick" \ "triggered" match {
      case JArray( x ) ⇒ x map { y ⇒ TimedAction.fromJson( y ) }
      case _           ⇒ throw new Error( "Tick - can't extract 'clock'" )
    }

    val clock = js \ "Tick" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "Tick - can't extract 'clock'" )
    }

    val e = new Tick()( () ⇒ clock )
    e.clk = clock
    e.triggered = triggered.toSet
    e

  }
}
case class Tick()( implicit val clock: () ⇒ BigInt ) extends TraceEvent {
  var triggered: Set[TimedAction] = Set()
  var clk = clock()
  def toJson: JValue = {

    ( getClass.getSimpleName ->
      ( "triggered" -> triggered.map { x ⇒ x.toJson } ) ~
      ( "clock" -> clk ) )
  }
}

//-------------
// START-EXPLORE
//-------------

object ExploreStart {
  def fromJson( js: JValue ): ExploreStart = {
    implicit val formats = DefaultFormats
    //    js.extract[ExploreStart]

    val clock = js \ "ExploreStart" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExploreStart - can't extract 'clock'" )
    }

    val e = new ExploreStart()( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExploreStart()( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "clock" -> clk ) )
  }
}

object ExploreEnd {
  def fromJson( js: JValue ): ExploreEnd = {
    implicit val formats = DefaultFormats
    //    js.extract[ExploreEnd]

    val clock = js \ "ExploreEnd" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ExploreEnd - can't extract 'clock'" )
    }

    val e = new ExploreEnd()( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ExploreEnd()( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "clock" -> clk ) )
  }
}

object ShutdownScheduler {
  def fromJson( js: JValue ): ShutdownScheduler = {
    implicit val formats = DefaultFormats
    //    js.extract[ShutdownScheduler]

    val clock = js \ "ShutdownScheduler" \ "clock" match {
      case JInt( x ) ⇒ x
      case _         ⇒ throw new Error( "ShutdownScheduler - can't extract 'clock'" )
    }

    val e = new ShutdownScheduler()( () ⇒ clock )
    e.clk = clock
    e

  }
}
case class ShutdownScheduler()( implicit clock: () ⇒ BigInt ) extends TraceEvent {

  var clk = clock()

  def toJson: JValue = {
    ( getClass.getSimpleName ->
      ( "clock" -> clk ) )
  }
}
