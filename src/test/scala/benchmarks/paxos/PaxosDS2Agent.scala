//package benchmarks.paxos
//
//import edu.utah.cs.gauss.ds2.core.ir.datastructures._
//import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
//import edu.utah.cs.gauss.ds2.core.tracing.Logging.AgentMessaging._
//
//import java.util.logging.Logger
//import scala.collection.mutable
//
//object OperationType extends Enumeration {
//  type OperationType = Value
//  val READ, WRITE = Value
//}
//
//import benchmarks.paxos.OperationType._
////-------------------------------------------------------------------------------------------
////                                ===  INVOCATION's  ===
////-------------------------------------------------------------------------------------------
///**
// * Client "read" request
// * === part of HARNESS TWEAK ===
// * i.e. leave out of key-map since it maps one-to-one with <code>Prepare</code>-invocation */
//case class Request( key: String ) extends Message( "Request", Seq( key ) )
//
///**
// * Client "write" request
// * === part of HARNESS TWEAK ===
// * i.e. leave out of key-map since it maps one-to-one with <code>Propose</code>-invocation */
//case class ProposeCommand( key: String, value: Any ) extends Message( "ProposeCommand", Seq( key, value ) )
//
////-------------------------------------------------------------------------------------------
////                                ===  PROTOCOL  ===
////-------------------------------------------------------------------------------------------
///**
// * Internal Multi-Paxos message to start processing a client "write" request.
// * === Invocation to a WRITE operation === */
//case class Propose( key: String, value: Any, client: Agent,
//                    operation: OperationType.Value ) extends Message( "Propose", Seq( key, value, client, operation ) )
//
///**
// * 'value' is definitely not needed here in most messages but i added it so i don't have to track them locally on
// * agents (messages associate values with their proposalIDs better and the last one making a quorum or more gets to be
// * sent as a reply to client if READ, otherwise phase 2 starts in WRITE operations -- i.e. started with a Propose).
// *
// * This is good since it offsets from the captured state during alg. snapshotting states. For a real implementation
// * however, it is better to track these in local maps rather than overwhelming the network with bigger messages
// * (messages with more data). */
//case class Prepare( instance: BigInt, // internal to protocol
//                    pid: ProposalID, // internal to protocol
//                    key: String, value: Any, client: Agent,
//                    operation: OperationType.Value ) extends Message( "Prepare", Seq( instance, pid, key, value,
//                                                                                      client, operation ) )
//
//case class Promise( instance                   : BigInt, // internal to protocol
//                    nodeID                     : Int, // internal to protocol
//                    valuePidPair: (Any, ProposalID), // internal to protocol
//                    promisedPid                : ProposalID, // internal to protocol
//                    key: String, value: Any, client: Agent,
//                    operation                  : OperationType.Value ) extends Message( "Promise", Seq( instance,
//                                                                                                        nodeID,
//                                                                                                        valuePidPair,
//                                                                                                        promisedPid,
//                                                                                                        key, value,
//                                                                                                        client,
//                                                                                                        operation ) )
//
//case class Nack( instance                                                                         : BigInt, //
//                 // internal to protocol
//                 highestPid                                                                       : ProposalID, //
//                 // internal to protocol
//                 key: String, value: Any,
//                 client: Agent,
//                 operation                                                                        : OperationType
//                 .Value ) extends Message( "Nack", Seq( instance, highestPid,
//                                                        key, value,
//                                                        client, operation ) )
//
//case class Accept( instance: BigInt, // internal to protocol
//                   toAcceptPid                          : ProposalID, // internal to protocol
//                   key                                  : String, //                  accValue: Any,
//                   value                                : Any, client: Agent,
//                   operation                            : OperationType.Value ) extends Message( "Accept", Seq(
//  instance,
//  toAcceptPid, key,
//  value, client,
//  operation ) )
//
//case class Accepted( instance                                                                          : BigInt, //
//                     // internal to
//                     // protocol
//                     nodeID: Int, //
//                     // internal to protocol
//                     pid                                                                               : ProposalID,
//                     // internal to
//                     // protocol
//                     key                                                                               : String,
//                     value: Any,
//                     client                                                                            : Agent,
//                     operation                                                                         : OperationType.Value )
//  extends Message(
//    "Accepted", Seq( instance,
//                     nodeID, pid, key,
//                     value, client,
//                     operation ) )
//
//case class Response( instance                                                                                     : BigInt, // internal to protocol
//                     accPid                                                                                       : ProposalID, // internal to protocol
//                     key                                                                                          : String,
//                     value                                                                                        : Any,
//                     client: Agent,
//                     operation: OperationType.Value ) extends Message( "Response", Seq(
//  instance, accPid, key, value,
//  client, operation ) )
//
////-------------------------------------------------------------------------------------------
////                                ===  RESPONSE's  ===
////-------------------------------------------------------------------------------------------
///**
// * Client READ response */
//case class ClientReadResponse( key                                                            : String,
//                               value                                                          : Any ) extends Message(
//  "ClientReadResponse", Seq(
//    key, value ) )
//
///**
// * Client WRITE response */
//case class ClientWriteResponse( key: String, value: Any ) extends Message( "ClientWriteResponse", Seq( key, value ) )
//
////-------------------------------------------------------------------------------------------
////                                ===  Multi-Paxos Agent  ===
////-------------------------------------------------------------------------------------------
///**
// * A paxos actor can act as three types of agents: proposer, acceptor, learner.
// * As proposer, users can prompt the paxos to propose a value from the console
// * As acceptor, the paxos can also receive prepare message, and send out promise or nack message as response
// * As learner, the paxos is always listening from other paxos accepted value.
// *
// * @author <br>
// *         Mohammed S. Al-Mahfoudh <br/>
// *         mahfoudh@cs.utah.edu <br/>
// *         SoC - Gauss Group <br/>
// *
// *         based on the Akka implementation done by Zepeng Zhao. */
//class PaxosDS2Agent( name: String, numOfAgentsInSystem: Int, log: Boolean = false ) extends Agent( name ) {
//
//  // Constants
//  private val logger     = Logger.getLogger( Logger.GLOBAL_LOGGER_NAME ) // constant
//  private val quorumSize = numOfAgentsInSystem / 2 + 1 // constant
//  var leaderLive = true // the leader is ALWAYS live in this model and it is always with 'name' == '1'
//  val id: Int    = name.toInt
//
//  // declarations
//  private val PROPOSAL_VALUES   = s"proposalValues${LocalState.DELIM}List[Any]"
//  private val PROPOSING_VALUE   = s"proposingValue${LocalState.DELIM}Any"
//  private val PROPOSING_ID      = s"proposingId${LocalState.DELIM}ProposalID"
//  //((instance_number,proposal_id)=>List[(value,proposal_id)])
//  private val PROMISES          = s"promises${LocalState.DELIM}Map[String,List[(Any,ProposalID)]]"
//  private val LEARNED_PROPOSALS = s"learnedProposals${LocalState.DELIM}Map[String,(Any,ProposalID,Int)]"
//  private val PROMISE_IDS       = s"promiseIds${LocalState.DELIM}Map[BigInt,ProposalID]"
//  private val NEXT_INSTANCE     = s"nextInstance${LocalState.DELIM}BigInt"
//  //  private var leader: Int = this.name
//  private val LEADER            = s"leader${LocalState.DELIM}Int"
//  private val LEADING_INSTANCE  = s"leadingInstance${LocalState}BigInt"
//  //(instance_number => (value, pid))
//  private val LOGS              = s"logs${LocalState.DELIM}Map[BigInt, (Any, ProposalID)]".trim
//  // Map [(Client, key) -> List[Any]] // list of values
//  private val READS_TRACKER     = s"readsTracker${LocalState.DELIM}mutable.Map[(Agent,String),List[Any]]"
//
//  // types
//  type PROPOSAL_VALUES_TYPE = List[ Any ]
//  type PROPOSING_VALUE_TYPE = Any
//  type PROPOSING_ID_TYPE = ProposalID
//  type PROMISES_TYPE = Map[ String, List[ (Any, ProposalID) ] ]
//  type LEARNED_PROPOSALS_TYPE = Map[ String, (Any, ProposalID, Int) ]
//  type PROMISE_IDS_TYPE = Map[ BigInt, ProposalID ]
//  type NEXT_INSTANCE_TYPE = BigInt
//  type LOGS_TYPE = Map[ BigInt, (Any, ProposalID) ]
//  type LEADER_TYPE = Int
//  type LEADING_INSTANCE_TYPE = BigInt
//  // Map [(Client, key) ->List[Any]] // list of values
//  type READS_TRACKER_TYPE = mutable.Map[ (Agent, String), List[ Any ] ]
//
//  // initializations
//  localState( PROPOSAL_VALUES ) = List[ Any ]()
//  localState( PROPOSING_VALUE ) = null
//  localState( PROPOSING_ID ) = null
//  localState( PROMISES ) = Map[ String, List[ (Any, ProposalID) ] ]()
//  localState( LEARNED_PROPOSALS ) = Map[ String, (Any, ProposalID, Int) ]()
//  localState( PROMISE_IDS ) = Map[ BigInt, ProposalID ]()
//  localState( NEXT_INSTANCE ) = BigInt( 0 )
//  localState( LOGS ) = Map[ BigInt, (Any, ProposalID) ]()
//  localState( LEADER ) = id
//  localState( LEADING_INSTANCE ) = BigInt( 0 )
//  // Map [(Client, key) -> List[Any]] // list of values
//  localState( READS_TRACKER ) = mutable.Map[ (Agent, String), List[ Any ] ]()
//  //  localState(LEADING_INSTANCE) = localState[BigInt](NEXT_INSTANCE) //weird to initialize twice in original code
//  if( log ) logger.info( "Next instance:" + localState[ BigInt ]( NEXT_INSTANCE ) )
//
//  // The SPEC that WGL will capture, that represents the Empty Entries table (check
//  private val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"
//  type SPEC_TYPE = mutable.Map[ Any, Any ]
//  localState( SPEC ) = mutable.Map.empty[ Any, Any ]
//
//  //-------------------------------------------------------------------------------------------
//  //                                     ProposeCommand
//  //-------------------------------------------------------------------------------------------
//  //  case class ProposeCommand(key: String, value: Any)
//  private val proposeCommandAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    // regular Message produced by client from harness
//    val key    = m.payload[ String ]( 0 )
//    val value  = m.payload[ Any ]( 1 )
//    val client = m.sender
//
//    // this is the "write" command from clients to the cluster of paxos nodes (forwarding to leader)
//    val msg = Propose( key, value, client, WRITE )
//    ds.send( this, msg, ds.get( localState[ LEADER_TYPE ]( LEADER ).toString ) )
//    if( log ) sent( this, msg, ds.get( localState[ LEADER_TYPE ]( LEADER ).toString ) )
//
//    if( log ) logger.info( "ask node:[" + id + "] to propose {value:" + value + "}" )
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                ===  Propose  ===
//  //-------------------------------------------------------------------------------------------
//  //  case class Propose(key: String,
//  //                     value: Any,
//  //                     client: Agent,
//  //                     operation: OperationType.Value)
//  private val proposeAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    val mm = m.asInstanceOf[ Propose ]
//
//    val key       = mm.key
//    val value     = mm.value
//    val client    = mm.client
//    val operation = mm.operation
//
//    if( log ) logger.info( "received propose command:propose{value:" + value + "}" )
//
//    if( ref[ PROPOSING_VALUE_TYPE ]( PROPOSING_VALUE ) == null ) {
//      localState( PROPOSING_VALUE ) = value
//      localState( PROPOSING_ID ) = ProposalID( 0, id )
//
//      // because it starts a new multi-paxos instance (round), we have to increment it
//      localState( NEXT_INSTANCE ) = localState[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ) + 1
//
//      val nextInstance = ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE )
//      val proposingId  = ref[ PROPOSING_ID_TYPE ]( PROPOSING_ID )
//      for( (dstName, dst) <- ds.agents.map { x =>
//        (x.name, x)
//      } ) {
//
//        val msg = Prepare( nextInstance, proposingId, key, value, client, operation ) // the operation WRITE is set
//        // in the proposeCommandAction
//        ds.send( this, msg, dst )
//        if( log ) sent( this, msg, dst )
//
//        if( log ) logger.info( s"Send prepare{instance:[$nextInstance], proposal_id:$proposingId} to node: $dstName" )
//      }
//    } else {
//      localState( PROPOSAL_VALUES ) = ref[ PROPOSAL_VALUES_TYPE ]( PROPOSAL_VALUES ) :+ value
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Request
//  //-------------------------------------------------------------------------------------------
//  //  case class Request(key: String)
//  private val requestAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    // a regular Message so no casting allowed (sent by a client/IRed made
//    // from harness)
//
//    val ins         = localState[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ) //mm.ins
//    val proposingID = localState[ PROPOSING_ID_TYPE ]( PROPOSING_ID )
//    val key         = m.payload[ String ]( 0 )
//    val client      = m.sender
//
//    // here is where we initialize an accumulator for the client/key request to values read from majority
//    localState[ READS_TRACKER_TYPE ]( READS_TRACKER )( (client, key) ) = List[ Any ]() // check if this kv exists in
//    // receiver of "Promise" and update accordingly
//    // this sends a Prepare to self and TAG it as READ request so that once promised/Nacked it can be tracked/retries
//    for( dst <- ds.agents ) { // broadcast a majority READ request (Prepare that is tagged with READ)
//      // yes, send a prepare even to self
//      val msg = Prepare( ins, proposingID, key, null, client, READ ) // value field is ignored in a READ Prepare message
//      ds.send( this, msg, dst ) // remote is m.sender in terms of UDP
//      if( log ) sent( this, msg, dst )
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                ===  Prepare  ===
//  //    === the one that starts an invocation: read/write, after Request/Propose is invoked ===
//  //-------------------------------------------------------------------------------------------
//  //  case class Prepare(ins: BigInt, // internal to protocol
//  //                     pid: ProposalID, // internal to protocol
//  //                     key: String,
//  //                     value: Any,
//  //                     client: Agent,
//  //                     operation: OperationType.Value)
//  private val prepareAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    val mm          = m.asInstanceOf[ Prepare ]
//    val ins         = mm.instance
//    val proposedPid = mm.pid
//    val key         = mm.key
//    val value       = mm.value
//    val client      = mm.client
//    val operation   = mm.operation
//
//    val promiseIds = ref[ PROMISE_IDS_TYPE ]( PROMISE_IDS )
//    val logs       = ref[ LOGS_TYPE ]( LOGS )
//
//    if( log )
//      logger.info( "received prepare{instance_number:" + ins.toString() + ", proposal_id:" + proposedPid
//        .toString() + "} from remote:[" + m.sender.name + "]" )
//
//    if( log ) logger.info( "Received prepare from:" + m.sender )
//
//    // local localPid (the received one is proposedPid)
//    val localPid: ProposalID = if( promiseIds.contains( ins ) ) promiseIds( ins ) else null
//
//    // checking if localPiD is smaller/equal to remote
//    if( localPid == null || !localPid.isGreaterThan( proposedPid ) ) {
//
//      if( operation == WRITE )
//        localState( PROMISE_IDS ) = ref[ PROMISE_IDS_TYPE ]( PROMISE_IDS ) + ( ins -> proposedPid )
//
//      if( log ) logger.info( "Send back promise to remote:[" + m.sender.toString() + "]" )
//
//      val acc_v_id: (Any, ProposalID) = if( logs.contains( ins ) ) logs( ins ) else null
//
//      //      val msg = Promise(ins, id, acc_v_id, proposedPid)
//      val msg = Promise( ins, id,
//                         acc_v_id,
//                         proposedPid,
//                         key,
//                         if( acc_v_id != null ) acc_v_id._1 else null,
//                         client,
//                         operation )
//
//      ds.send( this, msg, m.sender )
//      if( log ) sent( this, msg, m.sender )
//
//    } else { // received proposal ID is smaller than local proposal ID (localPid)
//      if( log ) logger.info( "Send back promise to remote:[" + m.sender.name + "]" )
//
//      val msg = Nack( ins, localPid, key, value, client, operation ) // same Nack for WRITE or READ operation
//      ds.send( this, msg, m.sender )
//      if( log ) sent( this, msg, m.sender )
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Promise
//  //-------------------------------------------------------------------------------------------
//  //  case class Promise(ins: BigInt, // internal to protocol
//  //                     nodeID: Int, // internal to protocol
//  //                     valueIDPair: (Any, ProposalID), // internal to protocol
//  //                     promisedPid: ProposalID, // internal to protocol
//  //                     key: String,
//  //                     value: Any,
//  //                     client: Agent,
//  //                     operation: OperationType.Value)
//  private val promiseAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    val mm = m.asInstanceOf[ Promise ]
//
//    val ins         = mm.instance
//    //    val nid = mm.nodeID
//    val accValueId  = mm.valuePidPair
//    val promisedPid = mm.promisedPid
//    val key1        = mm.key
//    val value       = mm.value
//    val client      = mm.client
//    val operation   = mm.operation
//
//    val promises     = ref[ PROMISES_TYPE ]( PROMISES )
//    val nextInstance = ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE )
//
//    if( operation == READ ) { // READ branch
//      val readTrackerKey        = (client,key1)
//      val valuesAlreadyReceived = localState[ READS_TRACKER_TYPE ]( READS_TRACKER )
//
//      if(valuesAlreadyReceived.contains(readTrackerKey)){
//        valuesAlreadyReceived( readTrackerKey ) = valuesAlreadyReceived( readTrackerKey ) :+ value
//        // find the value that repeats "max" number of time in comparison to others
//        val maxEntry         = valuesAlreadyReceived( readTrackerKey ).
//                               groupBy( identity ).toList.reduce { ( x, y ) => if( x._2.size > y._2.size ) x else y}
//        val maxEntryCount    = maxEntry._2.size
//        val reachedAcksCount = valuesAlreadyReceived( readTrackerKey ).size
//
//        if( maxEntryCount >= quorumSize ) { // send ClientReadResponse
//          // remove entry from reads tracker map
//          localState(READS_TRACKER) = valuesAlreadyReceived - readTrackerKey
//          // send response to client
//          val msg = ClientReadResponse(key1,maxEntry._1) // yes maxEntry._1 == value
//          ds.send(this,msg,client)
//          if(log) sent(this,msg,client)
//        }
//        else if( reachedAcksCount == numOfAgentsInSystem ) {
//          for(dst <- ds.agents){
//            // a Prepare for a READ should ignore the NEXT_INSTANCE and PROPOSING_ID on the prepareAction (receiver)
//            val msg = Prepare( localState[NEXT_INSTANCE_TYPE](NEXT_INSTANCE),
//                               localState[PROPOSING_ID_TYPE](PROPOSING_ID),
//                               key1,value,client,READ) // these are the only relevant fields for a READ
//            ds.send(this,msg,dst)
//            if(log) sent(this,msg,dst)
//          }
//          // reset old key content
//          valuesAlreadyReceived(readTrackerKey) = List[Any]()
//        }// otherwise key was removed since acks max possible count (peers count) has been reached before quorum
//      }
//    } else if( nextInstance == ins ) { // this is only for a WRITE and only if THIS agent is the one issuing the propose
//      val key = promisedPid.toString()
//      var l   = List( accValueId )
//      if( promises.contains( key ) ) l = l ++ promises( key )
//      localState( PROMISES ) = promises + ( key -> l ) // accumulating promises (for the final count and acceptance)
//      if( l.size >= this.quorumSize ) { // ready to send accept
//        var mid: ProposalID = null
//        l.foreach( f => {
//          if( f != null && ( mid == null || f._2.isGreaterThan( mid ) ) ) {
//            if( mid == null ) {
//              localState( PROPOSAL_VALUES ) = ref[ PROPOSAL_VALUES_TYPE ]( PROPOSAL_VALUES ) :+
//                ref[ PROPOSING_VALUE_TYPE ]( PROPOSING_VALUE )
//            }
//            mid = f._2 // proposal-id
//            localState( PROPOSING_VALUE ) = f._1
//          }
//        } ) //        localState(PROMISES) = Map[String, List[(Any, ProposalID)]]() // reset promises-counter/tracker
//        localState( PROMISES ) = localState[ PROMISES_TYPE ]( PROMISES ) - key // remove concerned key only
//        for( dst <- ds.agents ) { // broadcast accept
//          //          val msg = Accept(ref[NEXT_INSTANCE_TYPE](NEXT_INSTANCE),
//          //            ref[PROPOSING_VALUE_TYPE](PROPOSING_VALUE), ref[PROPOSING_ID_TYPE](PROPOSING_ID))
//          val msg = Accept( ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ), ref[ PROPOSING_ID_TYPE ]( PROPOSING_ID ),
//                            key1, ref[ PROPOSING_VALUE_TYPE ]( PROPOSING_VALUE ), client, operation )
//
//          ds.send( this, msg, dst )
//          if( log ) sent( this, msg, dst )
//        }
//      }
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Nack
//  //-------------------------------------------------------------------------------------------
//  //  case class Nack(ins: BigInt, // internal to protocol
//  //                  highestPid: ProposalID, // internal to protocol
//  //                  key: String,
//  //                  value: Any,
//  //                  client: Agent,
//  //                  operation: OperationType.Value)
//  private val nackAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    val mm = m.asInstanceOf[ Nack ]
//
//    val ins       = mm.instance
//    val higherPid = mm.highestPid
//    val key       = mm.key
//    val value     = mm.key
//    val client    = mm.client
//    val operation = mm.operation
//
//    val leader       = ref[ LEADER_TYPE ]( LEADER )
//    val nextInstance = ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE )
//    val proposingId  = ref[ PROPOSING_ID_TYPE ]( PROPOSING_ID )
//
//    if( log ) {
//      logger.info(
//        "Received nack{instance:" + ins + ",higher_pid:" + higherPid.toString() + "} from:" + m.sender.toString() )
//    }
//    if( leader == id ) { // if current node is leader
//      if( ins == nextInstance && proposingId != null && higherPid.isGreaterThan( proposingId ) ) {
//        localState( PROPOSING_ID ) = ProposalID( higherPid.num + 1, id )
//        for( dst <- ds.agents ) { // broadcast new Prepare msg to all with highestProposalID + 1
//          val msg = Prepare( nextInstance, proposingId, key, value, client, operation )
//          ds.send( this, msg, dst )
//          if( log ) sent( this, msg, dst )
//          if( log ) {
//            logger.info( "Send prepare{instance:[" + nextInstance + "], proposal_id:" +
//                           ref[ PROPOSING_ID_TYPE ]( PROPOSING_ID ).toString() + "} to node:" + dst.name )
//          }
//        }
//      }
//    } else {
//      //      val msg = ProposeCommand(ref[PROPOSING_VALUE_TYPE](PROPOSING_VALUE), leader)
//      val msg = ProposeCommand( key, value )
//      ds.send( this, msg, this )
//      if( log ) sent( this, msg, this )
//    }
//
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Accept
//  //-------------------------------------------------------------------------------------------
//  //  case class Accept(ins: BigInt, // internal to protocol
//  //                    accPid: ProposalID, // internal to protocol
//  //                    key: String,
//  //                    //                  accValue: Any,
//  //                    value: Any,
//  //                    client: Agent,
//  //                    operation: OperationType.Value)
//  private val acceptAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//    val mm = m.asInstanceOf[ Accept ]
//
//    val ins           = mm.instance
//    val toAcceptPid   = mm.toAcceptPid
//    val key           = mm.key
//    val toAcceptValue = mm.value
//    val client        = mm.client
//    val operation     = mm.operation
//
//    val promiseIds = ref[ PROMISE_IDS_TYPE ]( PROMISE_IDS )
//    val logs       = ref[ LOGS_TYPE ]( LOGS )
//
//    val localPid: ProposalID = if( promiseIds.contains( ins ) ) promiseIds( ins ) else null
//
//    logger.info( "Received accept from:" + m.sender.name + " value:[" + toAcceptValue + "],proposal id:[" +
//                   toAcceptPid.toString() + "]" )
//
//    if( localPid == null || !localPid.isGreaterThan( toAcceptPid ) ) {
//      localState( PROMISE_IDS ) = promiseIds + ( ins -> toAcceptPid )
//      localState( LOGS ) = logs + ( ins -> (toAcceptValue, toAcceptPid) )
//
//      val msg = Accepted( ins, id, toAcceptPid, key, toAcceptValue, client, operation )
//      ds.send( this, msg, m.sender )
//      if( log ) sent( this, msg, m.sender )
//
//      //      Util.writeToDisk(filename, logs) // in the algorithm, we don't restart or reload anything from disk, we
//      //      don't deal with crashes
//    } else {
//      if( log ) logger.info( "send back nack to remote:[" + m.sender.name + "]" )
//
//      val msg = Nack( ins, localPid, key, toAcceptValue, client, operation )
//      ds.send( this, msg, m.sender )
//      if( log ) sent( this, msg, m.sender )
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                    === Accepted ===
//  //-------------------------------------------------------------------------------------------
//  //  case class Accepted(ins: BigInt, // internal to protocol
//  //                      uid: Int, // internal to protocol
//  //                      pid: ProposalID, // internal to protocol
//  //                      key: String,
//  //                      value: Any,
//  //                      client: Agent,
//  //                      operation: OperationType.Value)
//  private val acceptedAction = new Action + Statement { ( m: Message, _: Agent ) =>
//    if( log ) received( m.sender, m, this )
//
//    val mm             = m.asInstanceOf[ Accepted ]
//    val ins            = mm.instance
//    val nid            = mm.nodeID
//    val acceptedPid    = mm.pid
//    val keyFromMessage = mm.key
//    val acceptedValue  = mm.value
//    val client         = mm.client
//
//    if( log ) logger.info( "learning a value:[" + acceptedValue + "] from node " + nid )
//
//    // this key was replaced by the 'key' field of the received message (Accepted)
//    val key = acceptedPid.toString() + "_" + ins
//    //println("accept key:"+key)
//
//    if( ins >= ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ) ) {
//      if( !ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS ).contains( key ) ) {
//        localState( LEARNED_PROPOSALS ) = ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS ) +
//          ( key -> (acceptedValue, acceptedPid, 1) )
//      } else {
//        val previouslyLearnedRecord = ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS )( key )
//
//        val newCount                = previouslyLearnedRecord._3 + 1
//
//        localState( LEARNED_PROPOSALS ) = ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS ) +
//          ( key -> (acceptedValue, acceptedPid, newCount) )
//
//        if( newCount >= quorumSize ) {
//          // here is where we send a response to the client for its Write(k,v) request
//          val msg = ClientWriteResponse( keyFromMessage, acceptedValue )
//          ds.send( this, msg, client )
//          if( log ) sent( this, msg, client )
//
//          if( log ) {
//            print( "\nLearned value:" + acceptedValue + ",instance:" +
//                     ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ) + "\n->" )
//          } // reset to prepare for next round/instance proposal
//          localState( PROPOSING_VALUE ) = null
//          localState( PROPOSING_ID ) = null
//          localState( NEXT_INSTANCE ) = ref[ NEXT_INSTANCE_TYPE ]( NEXT_INSTANCE ) + 1
//        }
//      }
//    }
//  }
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Util Methods
//  //-------------------------------------------------------------------------------------------
//  def ref[ T ]( variable: String ): T = {
//    localState[ T ]( variable )
//  } //-------------------------------------------------------------------------------------------
//  //                                     Adding Reactions
//  //-------------------------------------------------------------------------------------------
//  defaultBehavior += new Message( "Prepare" ) -> prepareAction
//  defaultBehavior += new Message( "Promise" ) -> promiseAction
//  defaultBehavior += new Message( "Nack" ) -> nackAction
//  defaultBehavior += new Message( "Accept" ) -> acceptAction
//  defaultBehavior += new Message( "Accepted" ) -> acceptedAction
//  defaultBehavior += new Message( "Propose" ) -> proposeAction
//  defaultBehavior += new Message( "Request" ) -> requestAction
//  defaultBehavior += new Message( "ProposeCommand" ) -> proposeCommandAction
//
//  reactions = defaultBehavior
//
//  //-------------------------------------------------------------------------------------------
//  //                                     Start Action
//  //-------------------------------------------------------------------------------------------
//  specialReactions( Start() ) + Statement { ( _: Message, _: Agent ) =>
//
//    /**
//     * During start:
//     * - if id == 1 => make leader
//     * - if id != 1 => choose leader to be node 1, and be not-leader
//     * ... */
//    if( id == 1 ) { // leader
//      localState( LEADER ) = id
//    } else { // not leader
//      localState( LEADER ) = 1
//    }
//  }
//
//}
