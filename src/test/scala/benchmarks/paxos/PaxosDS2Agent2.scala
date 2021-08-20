package benchmarks.paxos

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.tracing.Logging.AgentMessaging._

import java.util.logging.Logger
import scala.collection.mutable

object OperationType extends Enumeration {
  type OperationType = Value
  val READ, WRITE = Value
}

import benchmarks.paxos.OperationType._

// before you mutate any object inside the localState... just copy it before mutating and re-assign to the field
//-------------------------------------------------------------------------------------------
//                                ===  INVOCATION's  ===
//-------------------------------------------------------------------------------------------
/**
 * Client "read" request
 * === part of HARNESS TWEAK ===
 * i.e. leave out of key-map since it maps one-to-one with <code>Prepare</code>-invocation */
case class Request( key: String ) extends
  Message( "Request", Seq( key ) )

/**
 * Client "write" request
 * === part of HARNESS TWEAK ===
 * i.e. leave out of key-map since it maps one-to-one with <code>Propose</code>-invocation */
case class ProposeCommand( key: String, value: Any ) extends
  Message( "ProposeCommand", Seq( key, value ) )

//-------------------------------------------------------------------------------------------
//                                ===  PROTOCOL  ===
//-------------------------------------------------------------------------------------------
/**
 * Internal Multi-Paxos message to start processing a client "write" request.
 * === Invocation to a WRITE operation === */
case class Propose( key: String, value: Any, client: Agent, operation: OperationType.Value ) extends
  Message( "Propose", Seq( key, value, client, operation ) )

case class Prepare( instance: Instance, value: Any ) extends
  Message( "Prepare", Seq( instance, instance.key, value ) )

case class Promise( instance: Instance, value: Any ) extends
  Message( "Promise", Seq( instance, instance.key, value ) )

// highestPid is higher than the one in the instance, reason behind Nack-ing of this instance
case class Nack( instance: Instance, highestPid: Int, value: Any ) extends
  Message( "Nack", Seq( instance, highestPid, instance.key, value ) )

case class Accept( instance: Instance, value: Any ) extends
  Message( "Accept", Seq( instance, instance.key, value ) )

case class Accepted( instance: Instance, value: Any ) extends
  Message( "Accepted", Seq( instance, instance.key, value ) )

//-------------------------------------------------------------------------------------------
//                                ===  RESPONSE's  ===
//-------------------------------------------------------------------------------------------
/**
 * Client READ response */
case class ClientReadResponse( key: String, value: Any ) extends
  Message( "ClientReadResponse", Seq( key, value ) )

/**
 * Client WRITE response */
case class ClientWriteResponse( key: String, value: Any ) extends
  Message( "ClientWriteResponse", Seq( key, value ) )

//-------------------------------------------------------------------------------------------
//                                ===  Multi-Paxos Agent  ===
//-------------------------------------------------------------------------------------------
/**
 * A paxos actor can act as three types of agents: proposer, acceptor, learner.
 * As proposer, users can prompt the paxos to propose a value from the console
 * As acceptor, the paxos can also receive prepare message, and send out promise or nack message as response
 * As learner, the paxos is always listening from other paxos accepted value.
 *
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *
 *         based on the Akka implementation done by Zepeng Zhao. */
class PaxosDS2Agent2( name: String, numOfAgentsInSystem: Int, log: Boolean = false )
  extends Agent( name ) {
  
  // Constants
  private val logger      = Logger.getLogger( Logger.GLOBAL_LOGGER_NAME ) // constant
  private val quorumSize  = numOfAgentsInSystem / 2 + 1 // constant
  private val leader: Int = 1
  var leaderLive = true // the leader is ALWAYS live in this model and it is always with 'name' == '1'
  val id: Int    = name.toInt
  
  // declarations
  private val LOGS              = s"logs${LocalState.DELIM}Map[ Instance, Record ]".trim
  private val LEARNED_PROPOSALS = s"learnedProposals${LocalState.DELIM}Map[ String, Any ]".trim
  private val LATEST_INSTANCE   = s"latestInstance${LocalState.DELIM}Instance"
  
  // types
  type LOGS_TYPE = Map[ Instance, Record ]
  type LEARNED_PROPOSALS_TYPE = Map[ String, Any ] // [k,v] : (String,Any)
  type LATEST_INSTANCE_TYPE = Instance
  
  // initializations
  
  // tracks learned promises on all nodes (after receiving accept/accepted)
  localState( LEARNED_PROPOSALS ) = Map[ String, Any ]()
  localState( LATEST_INSTANCE ) = null // tracks what is the last instance this node issued
  localState( LOGS ) = Map[ Instance, Record ]() // logs real-time progress
  if( log ) logger.info( "Next instance:" + localState[ BigInt ]( LATEST_INSTANCE ) )
  
  // The SPEC that WGL will capture, that represents the Empty Entries table (check
  private val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"
  type SPEC_TYPE = mutable.Map[ Any, Any ]
  localState( SPEC ) = mutable.Map.empty[ Any, Any ]
  
  //-------------------------------------------------------------------------------------------
  //                                     Util Methods
  //-------------------------------------------------------------------------------------------
  def ref[ T ]( variable: String ): T = {
    localState[ T ]( variable )
  }
  
  def iAmTheProposerOf( instance: Instance ): Boolean = instance.id == id
  
  def adtAgents: Set[ Agent ] = ds.agents.filterNot( _.name.startsWith( "IRed" ) )
  
  //-------------------------------------------------------------------------------------------
  //                                     ProposeCommand
  //-------------------------------------------------------------------------------------------
  //  case class ProposeCommand(key: String, value: Any)
  private val proposeCommandAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    // regular Message produced by client from harness
    val key    = m.payload[ String ]( 0 )
    val value  = m.payload[ Any ]( 1 )
    val client = m.sender
    
    // this is the "write" command from clients to the cluster of paxos nodes (forwarding to leader)
    val msg = Propose( key, value, client, WRITE )
    ds.send( this, msg, ds.get( leader ) )
    if( log ) sent( this, msg, ds.get( leader ) )
    
    if( log ) logger.info( "ask node:[" + id + "] to propose {value:" + value + "}" )
  }
  
  //-------------------------------------------------------------------------------------------
  //                                ===  Propose  ===
  //-------------------------------------------------------------------------------------------
  //  case class Propose(key: String,
  //                     value: Any,
  //                     client: Agent,
  //                     operation: OperationType.Value)
  private val proposeAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    
    val mm = m.asInstanceOf[ Propose ]
    
    val key       = mm.key
    val value     = mm.value
    val client    = mm.client
    val operation = mm.operation
    
    var latestInstance = ref[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )
    val logs           = ref[ LOGS_TYPE ]( LOGS )
    
    if( log ) logger.info( "received propose command:propose{value:" + value + "}" )
    
    if( null == latestInstance ) { // only first write request
      localState( LATEST_INSTANCE ) = Instance( client, key, id, operation, quorumSize )
      latestInstance = localState[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )
    }
    
    // because it starts a new multi-paxos instance (round), we have to increment it
    val latestInstanceCopy = latestInstance.copy
    val instance           = Instance( client, key, id, operation, quorumSize ) // copy
    instance.nextProposalNumber( latestInstanceCopy.getProposalNum ) // mutate
    localState( LATEST_INSTANCE ) = instance // store
    
    localState( LOGS ) = logs + ( instance -> Record( key, value, numOfAgentsInSystem ) )
    
    for( (dstName, dst) <- adtAgents.map { x => (x.name, x) } ) {
      
      val msg = Prepare( instance, value )
      ds.send( this, msg, dst )
      if( log ) sent( this, msg, dst )
      
      if( log ) logger.info( s"Send prepare{instance:[$instance], proposal_id:${instance.id}} to node: $dstName" )
    }
  }
  
  //-------------------------------------------------------------------------------------------
  //                                     Request
  //-------------------------------------------------------------------------------------------
  //  case class Request(key: String)
  private val requestAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    // a regular Message so no casting allowed (sent by a client/IRed made
    // from harness)
    val key    = m.payload[ String ]( 0 )
    val client = m.sender
    
    val copyInstance = Instance( client, key, id, READ, quorumSize )
    
    val msg = Prepare( copyInstance, null )
    ds.send( this, msg, ds.get( leader ) )
    if( log ) sent( this, msg, ds.get( leader ) )
  }
  
  //-------------------------------------------------------------------------------------------
  //                                ===  Prepare  ===
  //    === the one that starts an invocation: read/write, after Request/Propose is invoked ===
  //-------------------------------------------------------------------------------------------
  //  case class Prepare(ins: BigInt, // internal to protocol
  //                     pid: ProposalID, // internal to protocol
  //                     key: String,
  //                     value: Any,
  //                     client: Agent,
  //                     operation: OperationType.Value)
  private val prepareAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    val mm = m.asInstanceOf[ Prepare ]
    
    val instance    = mm.instance
    val proposedPid = mm.instance.id
    val operation   = mm.instance.operation
    val value       = mm.value
    val key         = instance.key
    val sender      = m.sender
    
    val learned = ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS )
    var logs    = ref[ LOGS_TYPE ]( LOGS )
    
    
    if( log ) logger.info( "received prepare{instance_number:" +
                             instance.toString + ", proposal_id:" +
                             proposedPid.toString + "} from remote:[" + m.sender.name + "]" )
    
    operation match {
      //--------------------------------------------------------------------------------------
      case READ =>
        var value: Any = null
        if( learned contains key ) value = learned( key )
        
        val msg = Promise( instance, value )
        ds.send( this, msg, sender )
        if( log ) sent( this, msg, sender )
      
      //--------------------------------------------------------------------------------------
      case WRITE =>
        // local localPid (the received one is proposedPid)
        var latestInstance = localState[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )
        
        // replicas
        if( latestInstance == null || latestInstance < instance ) {
          localState( LATEST_INSTANCE ) = instance
          latestInstance = localState[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )
        }
        
        if( !( logs contains instance ) )
          localState( LOGS ) = logs + ( instance -> Record( key, value, numOfAgentsInSystem ) )
        
        
        if( instance >= latestInstance ) {
          
          if( log ) logger.info( "Send back promise to remote:[" + m.sender.toString() + "]" )
          
          logs = localState[ LOGS_TYPE ]( LOGS ) // get the latest logs map, since it may have been updated earlier
          
          val msg = Promise( instance, value ) // 'record.value' should equal 'value'
          ds.send( this, msg, m.sender )
          if( log ) sent( this, msg, m.sender )
          
          
        } else { // received proposal ID is smaller than local proposal ID (localPid)
          
          if( log ) logger.info( "Send back nack to remote:[" + m.sender.name + "]" )
          
          val msg = Nack( instance, latestInstance.getProposalNum, value ) // same Nack for WRITE or READ operation
          ds.send( this, msg, m.sender )
          if( log ) sent( this, msg, m.sender )
          
        }
    } // match
  }
  
  //-------------------------------------------------------------------------------------------
  //                                     Promise
  //-------------------------------------------------------------------------------------------
  //  case class Promise(ins: BigInt, // internal to protocol
  //                     nodeID: Int, // internal to protocol
  //                     valueIDPair: (Any, ProposalID), // internal to protocol
  //                     promisedPid: ProposalID, // internal to protocol
  //                     key: String,
  //                     value: Any,
  //                     client: Agent,
  //                     operation: OperationType.Value)
  private val promiseAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    val mm = m.asInstanceOf[ Promise ]
    
    val instance  = mm.instance
    val key       = mm.instance.key
    val client    = mm.instance.client
    val operation = mm.instance.operation
    var value     = mm.value
    
    var logs = ref[ LOGS_TYPE ]( LOGS )
    
    //    val nextInstance = ref[ NEXT_INSTANCE_TYPE ]( LATEST_INSTANCE )
    operation match {
      //--------------------------------------------------------------------------------------
      case READ => // read from learnedProposals, if it doesn't exist then send "" as value
        if( value == null ) value = "" // WGL likes it this way instead of 'null' (which is a value by itself)
        
        val msg = ClientReadResponse( key, value )
        ds.send( this, msg, client )
        if( log ) sent( this, msg, client )
      //--------------------------------------------------------------------------------------
      case WRITE => // proceed as before
        if( logs.contains( instance ) ) {
          // checking for a proposal ID is only at the "prepare" action
          
          // this is only for a WRITE and only if THIS agent is the one issuing the propose
          
          // copy, mutate, then store => otherwise all hell breaks loose (weird bugs)
          val record = logs( instance ).copy // copy
          record.promise( value ) // mutate
          localState( LOGS ) = logs + ( instance -> record ) // store
          
          // it changed so we access it again from the localState
          logs = localState[ LOGS_TYPE ]( LOGS )
          if( logs( instance ).reachedQuorumPromises ) { // ready to send accept
            for( dst <- adtAgents ) { // broadcast accept
              val msg = Accept( instance, value )
              ds.send( this, msg, dst )
              if( log ) sent( this, msg, dst )
            }
          } else if( logs( instance ).reachedPrepareAcksLimit ) {
            val latestInstance = localState[LATEST_INSTANCE_TYPE](LATEST_INSTANCE)
            val newInstance = instance.copy // copy
            // top the highest pid by one
            newInstance.nextProposalNumber(latestInstance maxProposal newInstance) // mutate
            localState( LATEST_INSTANCE ) = newInstance // store
            
            // and new reset record
            val record = logs( instance ).resetCopy // reset and mutate
            localState( LOGS ) = localState[ LOGS_TYPE ]( LOGS ) + ( newInstance -> record )
            
            for( dst <- adtAgents ) { // broadcast new Prepare msg to all with highestProposalID + 1
              val msg = Prepare( newInstance, value )
              ds.send( this, msg, dst )
              if( log ) sent( this, msg, dst )
              if( log ) {
                logger.info( "Send prepare{instance:[" + newInstance + "], proposal_id:" +
                               newInstance.id.toString + "} to node:" + dst.name )
              }
            }
          }
        }
    }
  }
  
  //-------------------------------------------------------------------------------------------
  //                                     Nack
  //-------------------------------------------------------------------------------------------
  //  case class Nack(ins: BigInt, // internal to protocol
  //                  highestPid: ProposalID, // internal to protocol
  //                  key: String,
  //                  value: Any,
  //                  client: Agent,
  //                  operation: OperationType.Value)
  private val nackAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    val mm = m.asInstanceOf[ Nack ]

    val instance    = mm.instance
    val proposingId = instance.getProposalNum //ref[ PROPOSING_ID_TYPE ]( PROPOSING_ID )
    val higherPid   = mm.highestPid
    val value       = mm.value

    var logs = ref[ LOGS_TYPE ]( LOGS )

    if( log ) {
      logger.info(
        "Received nack{instance:" + instance + ",higher_pid:" + higherPid + "} from:" +
          m.sender.toString() )
    }

    if(higherPid > proposingId && ( logs contains instance ) ) {
      // copy, mutate, then store
      val record = logs( instance ).copy // copy
      record.nack() // mutate
      localState( LOGS ) = logs + ( instance -> record ) // store

      logs = localState[ LOGS_TYPE ]( LOGS ) // since it was updated already

      if( logs( instance ).reachedNackMajority || logs( instance ).reachedPrepareAcksLimit ) {
        val newInstance = instance.copy // copy
        // top the highest pid by one
        newInstance.nextProposalNumber( higherPid ) // mutate
        localState( LATEST_INSTANCE ) = newInstance // store

        // and new reset record
        val record = logs( instance ).resetCopy // reset and mutate
        localState( LOGS ) = localState[ LOGS_TYPE ]( LOGS ) + ( newInstance -> record )

        for( dst <- adtAgents ) { // broadcast new Prepare msg to all with highestProposalID + 1
          val msg = Prepare( newInstance, value )
          ds.send( this, msg, dst )
          if( log ) sent( this, msg, dst )
          if( log ) {
            logger.info( "Send prepare{instance:[" + newInstance + "], proposal_id:" +
                           newInstance.id.toString + "} to node:" + dst.name )
          }
        }
      }
    }
  }

  //-------------------------------------------------------------------------------------------
  //                                     Accept
  //-------------------------------------------------------------------------------------------
  //  case class Accept(ins: BigInt, // internal to protocol
  //                    accPid: ProposalID, // internal to protocol
  //                    key: String,
  //                    //                  accValue: Any,
  //                    value: Any,
  //                    client: Agent,
  //                    operation: OperationType.Value)
  private val acceptAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )
    val mm = m.asInstanceOf[ Accept ]

    val instance      = mm.instance
    val toAcceptPid   = mm.instance.getProposalNum
    val toAcceptValue = mm.value

//    val latestInstance = ref[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )

//    val localPid: Int = if( latestInstance != null ) latestInstance.getProposalNum else -1

    if( log ) logger.info( "Received accept from:" + m.sender.name + " value:[" + toAcceptValue + "],proposal id:[" +
                             toAcceptPid + "]" )
    
  
    // here is where we update the learned proposals for the replica... but we ignore it since leader never dies
    
    val msg = Accepted( instance, toAcceptValue )
    ds.send( this, msg, m.sender )
    if( log ) sent( this, msg, m.sender )
  }

  //-------------------------------------------------------------------------------------------
  //                                    === Accepted ===
  //-------------------------------------------------------------------------------------------
  //  case class Accepted(ins: BigInt, // internal to protocol
  //                      uid: Int, // internal to protocol
  //                      pid: ProposalID, // internal to protocol
  //                      key: String,
  //                      value: Any,
  //                      client: Agent,
  //                      operation: OperationType.Value)
  private val acceptedAction = new Action + Statement { ( m: Message, _: Agent ) =>
    if( log ) received( m.sender, m, this )

    val mm            = m.asInstanceOf[ Accepted ]
    val instance      = mm.instance
    val nid           = mm.instance.id
    val key           = mm.instance.key
    val client        = mm.instance.client
    val acceptedValue = mm.value

    var logs           = ref[ LOGS_TYPE ]( LOGS )
    val learned        = ref[ LEARNED_PROPOSALS_TYPE ]( LEARNED_PROPOSALS )
    val latestInstance = ref[ LATEST_INSTANCE_TYPE ]( LATEST_INSTANCE )

    if( log ) logger.info( "learning a value:[" + acceptedValue + "] from node " + nid )

    if(logs contains instance) {

      // copy, mutate, store
      val record = logs( instance ).copy // copy
      record.accept( acceptedValue ) // mutate
      localState( LOGS ) = logs + ( instance -> record ) // store

      logs = localState[ LOGS_TYPE ]( LOGS ) // since we just updated it above

      if( logs( instance ).reachedQuorumAcceptance ) {

        // here is where we send a response to the client for its Write(k,v) request
        val msg = ClientWriteResponse( key, acceptedValue )
        ds.send( this, msg, client )
        if( log ) sent( this, msg, client )

        localState( LEARNED_PROPOSALS ) = learned + ( key -> logs( instance ).getMajorityValue )

        if( log ) print( "\nLearned value:" + acceptedValue + ",instance:" + instance + "\n->" )

        // reset to prepare for next round/instance proposal
        localState( LATEST_INSTANCE ) = latestInstance.copy // copy, store
        // and remove the entry from the logs so that no future events trigger the sending of write response again
        //localState(LOGS) = localState[LOGS_TYPE](LOGS)  - instance
      }
    }
  }

  //-------------------------------------------------------------------------------------------
  //                                     Adding Reactions
  //-------------------------------------------------------------------------------------------
  defaultBehavior += new Message( "Prepare" ) -> prepareAction
  defaultBehavior += new Message( "Promise" ) -> promiseAction
  defaultBehavior += new Message( "Nack" ) -> nackAction
  defaultBehavior += new Message( "Accept" ) -> acceptAction
  defaultBehavior += new Message( "Accepted" ) -> acceptedAction
  defaultBehavior += new Message( "Propose" ) -> proposeAction
  defaultBehavior += new Message( "Request" ) -> requestAction
  defaultBehavior += new Message( "ProposeCommand" ) -> proposeCommandAction

  reactions = defaultBehavior

  //-------------------------------------------------------------------------------------------
  //                                     Start Action
  //-------------------------------------------------------------------------------------------
  //    specialReactions( Start() ) + Statement { ( _: Message, _: Agent ) =>
  //
  //      /**
  //       * During start:
  //       * - if id == 1 => make leader
  //       * - if id != 1 => choose leader to be node 1, and be not-leader
  //       * ... */
  //      if( id == 1 ) { // leader
  //        localState( LEADER ) = id
  //      } else { // not leader
  //        localState( LEADER ) = 1
  //      }
  //    }
  
}
