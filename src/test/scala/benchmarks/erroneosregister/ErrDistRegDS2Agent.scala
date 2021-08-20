package benchmarks.erroneosregister

import benchmarks.MajorityTracker
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Action, Agent, LocalState, Message }

import java.util.UUID
import scala.collection.mutable
import scala.util.Random

// Write related
case class Write( k: String, value: Any, client: Agent = null ) extends Message( "Write", Seq( k, value, client ) )

case class WriteReplica( k: String, value: Any, client: Agent, uuid: String,
                         round: Int = 0 ) extends Message( "WriteReplica", Seq( k, value, client, uuid, round ) )

case class WriteReplicaAck( k: String, value: Any, client: Agent, uuid: String,
                            round: Int = 0 ) extends Message( "WriteReplicaAck", Seq( k, value, client, uuid, round ) )

case class WriteAck( k: String, value: Any ) extends Message( "WriteAck", Seq( k, value ) )

// read related
case class Read( k: String, client: Agent = null ) extends Message( "Read", Seq( k, client ) )

case class ReadReplica( k: String, client: Agent, uuid: String,
                        round: Int = 0 ) extends Message( "ReadReplica", Seq( k, client, uuid, round ) )

case class ReadReplicaAck( k: String, value: Any, client: Agent, uuid: String,
                           round: Int = 0 ) extends Message( "ReadReplicaAck", Seq( k, value, client, uuid, round ) )

case class ReadAck( k: String, value: Any ) extends Message( "ReadAck", Seq( k, value ) )


class ErrDistRegDS2Agent( name: String, val peersCount: Int, val leader: Boolean = false,
                          val log: Boolean = false ) extends Agent( name ) {
  //-------------------------------------------------------------------------------------------
  //                                     Init and Decls
  //-------------------------------------------------------------------------------------------
  // declarations

  val KEY = "k"
  val REGISTER = s"register${LocalState.DELIM}mutable.Map[Any,Any]"
  type REGISTER_TYPE = mutable.Map[Any, Any]

  val UUID_MAP = s"uuidMap${LocalState.DELIM}mutable.Map[Agent,UUID]" // client->uuid
  type UUID_MAP_TYPE = mutable.Map[Agent, UUID]

  val WRITES_TRACKER = s"writesTracker${LocalState.DELIM}mutable.Map[Agent,Tracker]" // client->Tracker
  type WRITES_TRACKER_TYPE = mutable.Map[Agent, MajorityTracker]

  val READS_TRACKER = s"readsTracker${LocalState.DELIM}mutable.Map[Agent,Tracker]"
  type READS_TRACKER_TYPE = mutable.Map[Agent, MajorityTracker]

  // initializations
  localState(REGISTER) = mutable.Map[Any, Any](KEY -> "0")
  localState(WRITES_TRACKER) = mutable.Map[Agent, MajorityTracker]()
  localState(READS_TRACKER) = mutable.Map[Agent, MajorityTracker]()


  // util
  /**
   * gets all agents filtering out IRed agents and returns a set of agents in the
   * system including this agent.
   *
   * @return a set of agent including this agent, but filtering out all IRed agents
   */
  def getSystemAgents: Set[ Agent ] = {
    ds.agents.filterNot( _.name.startsWith( "IRed" ) )
  }

  def getPeers: Set[Agent] = getSystemAgents - this

  def getLeader: Agent = getPeers.filter { case x: ErrDistRegDS2Agent => x.leader }.head

  def trackersContain(client: Agent, write: Boolean = true): Boolean = {
    if (write) localState[WRITES_TRACKER_TYPE](WRITES_TRACKER).contains(client)
    else localState[READS_TRACKER_TYPE](READS_TRACKER).contains(client)
  }

  // Logging
  def received( src: Agent, mm: Message, dst: Agent ): Unit = {
    println( s"RECEIVED:\t${dst.name}\t\t<--\t\t${src.name}\t$mm" )
  }

  def sent( src: Agent, mm: Message, dst: Agent): Unit = println( s"SENT:\t\t${src.name}\t\t-->\t\t${dst.name}\t\t$mm" )

  //-------------------------------------------------------------------------------------------
  //                                           WRITE
  //-------------------------------------------------------------------------------------------
  val writeAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    //    val mm = m.asInstanceOf[Write]
    val k = m.payload[String](0)
    val value = m.payload[Any](1)

    //logging
    if (log) received(m.sender,m,a)

    if (leader) {
      a.localState[REGISTER_TYPE](REGISTER)(KEY) = value

      if (m.payload.size == 2) // direct from client
        a.localState[WRITES_TRACKER_TYPE](WRITES_TRACKER)(m.sender) = MajorityTracker(peersCount = this.peersCount,
                                                                                      value = value)
      else if (m.payload.size == 3) // forwarded from another peer
        a.localState[WRITES_TRACKER_TYPE](WRITES_TRACKER)(m.payload[Agent](2)) = MajorityTracker(peersCount = this
          .peersCount, value = value)

      val uuid = UUID.randomUUID().toString.takeRight(4)
      for (peer <- getPeers) {
        val msg = WriteReplica(k, value, m.sender, uuid)
        a.ds.send(a, msg, peer)
        //logging
        if (log) sent(a, msg, peer)
      }
    } else {
      val msg = Write(m.payload[String](0),m.payload[Any](1), m.sender )
      a.ds.send(a, msg, getLeader)
      //logging
      if (log) sent(a, msg, getLeader)
    }
  } // statement
  //-------------------------------------------------------------------------------------------
  //                                       WRITE REPLICA
  //-------------------------------------------------------------------------------------------
  val writeReplicaAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[WriteReplica]
    //logging
    if (log) received(mm.sender, mm, a)

    a.localState[REGISTER_TYPE](REGISTER)(KEY) = mm.value
    val msg = WriteReplicaAck(mm.k, mm.value, mm.client, mm.uuid, mm.round)
    a.ds.send(a, msg, mm.sender)
    //logging
    if (log) sent(a, msg, mm.sender)
  } // statement

  //-------------------------------------------------------------------------------------------
  //                                      WRITE REPLICA ACK
  //-------------------------------------------------------------------------------------------
  val writeReplicaAckAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[WriteReplicaAck]
    //logging
    if (log) received(mm.sender, mm, a)

    if (leader && trackersContain(mm.client)) {
      val tracker: MajorityTracker = a.localState[WRITES_TRACKER_TYPE](WRITES_TRACKER)(mm.client)
//      tracker.updateAcksAccordingToValue(mm.round, mm.value)
//      if (tracker.reachedMajority) {
        val msg = WriteAck(KEY, tracker.value)
        a.ds.send(a, msg, mm.client) // confirm write
        //Logging
        if (log) sent(a, msg, mm.client)

        a.localState[WRITES_TRACKER_TYPE](WRITES_TRACKER) -= mm.client // remove client req. tracker
      }
      // no retries for writes, they just go through
//    }
  } // statement

//  //-------------------------------------------------------------------------------------------
//  //                                           READ
//  //-------------------------------------------------------------------------------------------
//  val readAction: Action = new Action + Statement { (m: Message, a: Agent) =>
//    //    val mm = m.asInstanceOf[Read]
//    val k = m.payload[String](0)
//    //logging
//    if (log) received(m.sender,m,a)
//
//    if (leader) {
//      val thisValue = a.localState[REGISTER_TYPE](REGISTER)(KEY)
//
//      if (m.payload.size == 1) // direct from client
//        a.localState[READS_TRACKER_TYPE](READS_TRACKER)(m.sender) = MajorityTracker(peersCount = this.peersCount,
  //        value = thisValue)
//      else if (m.payload.size == 2) // forwarded from other peers
//        a.localState[READS_TRACKER_TYPE](READS_TRACKER)(m.payload[Agent](1)) = MajorityTracker(peersCount = this
  //        .peersCount, value = thisValue)
//
//      val uuid = UUID.randomUUID().toString.takeRight(4)
//      for (peer <- getPeers) {
//        var msg = new Message("whatever will get replaced")
//        //        DONE fix the ReadReplica to have the client in the payload if originally forwarded by the other
  //        peers
//
//        if(m.payload.size == 1) // direct message from client
//          msg = ReadReplica(k, m.sender, uuid)
//        else if(m.payload.size == 2) // forwarded from another peer
//          msg = ReadReplica(k, m.payload[Agent](1), uuid)
//
//        a.ds.send(a, msg, peer)
//        //logging
//        if (log) sent(a, msg, peer)
//      }
//    } else {
//      val msg = Read(m.payload[String](0), m.sender)
//      a.ds.send(a, msg, getLeader)
//      //logging
//      if (log) sent(a, msg, getLeader)
//    }
//  } // statement

  //-------------------------------------------------------------------------------------------
  //                            BUGGY READ - in purpose
  //    === Look at the commented above action to know the correct implementation ===
  //-------------------------------------------------------------------------------------------
  val readAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    //    val mm = m.asInstanceOf[Read]
    val k = m.payload[String](0)
    //logging
    if (log) received(m.sender,m,a)

    // just reply to the client with whatever value is stored here

      val thisValue = a.localState[REGISTER_TYPE](REGISTER)(KEY)

        val msg = ReadAck(k,thisValue)
        a.ds.send(a, msg, m.sender)
        //logging
        if (log) sent(a, msg, m.sender)
  
  } // statement

  //-------------------------------------------------------------------------------------------
  //                                       READ REPLICA
  //-------------------------------------------------------------------------------------------
  val readReplicaAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[ReadReplica]
    //Logging
    if (log) received(mm.sender,mm, a)

//    val thisValue = a.localState[REGISTER_TYPE](REGISTER)(KEY)
    val thisValue = Random.nextPrintableChar()
    val msg = ReadReplicaAck(mm.k, thisValue, mm.client, mm.uuid, mm.round)
    a.ds.send(a, msg, mm.sender)
    //logging
    if (log) sent(a, msg, mm.sender)
  } // statement

  //-------------------------------------------------------------------------------------------
  //                                     READ REPLICA ACK
  //-------------------------------------------------------------------------------------------
  val readReplicaAckAction: Action = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[ReadReplicaAck]
    //Logging
    if (log) received(mm.sender,mm,a)

    if (leader && trackersContain(mm.client, write = false)) {
      val tracker: MajorityTracker = a.localState[READS_TRACKER_TYPE](READS_TRACKER)(mm.client)

      tracker.updateAcksAccordingToValue(mm.round, mm.value)

//      if (tracker.reachedMajority) { // send readAck
        val msg = ReadAck(mm.k, tracker.value)
        a.ds.send(a, msg, mm.client)
        //Logging
        if (log) sent(a, msg, mm.client)
        a.localState[READS_TRACKER_TYPE](READS_TRACKER) -= mm.client
//      }
//      if (!tracker.reachedMajority && tracker.reachedNonFruitfulEndOfRound) {
//        println("DEBUG -----> NonFruitful end,,, so will retry! otherwise I am a dead code!")
//        val round = tracker.nextRound()
//        for (peer <- getPeers) {
//          val msg = ReadReplica(mm.k, mm.client, mm.uuid, round)
//          a.ds.send(a, msg, peer)
//          // Logging
//          if (log) sent(a, msg, peer)
//        }
//      }
    }
  } // statement

  //-------------------------------------------------------------------------------------------
  //                                     Adding Reactions
  //-------------------------------------------------------------------------------------------

  defaultBehavior += new Message("Write") -> writeAction
  defaultBehavior += new Message("WriteReplica") -> writeReplicaAction
  defaultBehavior += new Message("WriteReplicaAck") -> writeReplicaAckAction
  defaultBehavior += new Message("Read") -> readAction
  defaultBehavior += new Message("ReadReplica") -> readReplicaAction
  defaultBehavior += new Message("ReadReplicaAck") -> readReplicaAckAction

  reactions = defaultBehavior
  
}
