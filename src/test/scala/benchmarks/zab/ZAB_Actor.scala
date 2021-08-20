//package benchmarks.zab
///***************************************************************************
// *                                                                         *
// *                           ZAB_Actor.scala                               *
// *                            -------------------                          *
// *   date                 : 9.30.2015                                      *
// *   email                : heath.french@utah.edu                          *
// *                                                                         *
// *                                                                         *
// ***************************************************************************/
//
///***************************************************************************
// *                                                                         *
// *   This program is free software; you can redistribute it and/or modify  *
// *   it under the terms of the GNU General Public License as published by  *
// *   the Free Software Foundation; either version 2 of the License, or     *
// *   (at your option) any later version.                                   *
// *                                                                         *
// *   A copy of the license can be found in the license.txt file supplied   *
// *   with this software or at: http://www.gnu.org/copyleft/gpl.html        *
// *                                                                         *
// ***************************************************************************/
//
//import java.util.concurrent.TimeUnit
//
//import akka.actor._
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.duration.Duration
//
//case class SystemMaintenance(condition : Any)
//  case class UpdateSystemConnection()
//  case class GetSystemInformation()
//  case class GetLeader()
//  case class ReceivedSystemInformation(members : Set[ActorRef])
//  case class GetHighestZXID()
//
//  // different in every phase but still connected to maintenance
//  case class SystemChanges(added : Set[ActorRef], removed : Set[ActorRef])
//  case class FollowerLost(removed : ActorRef)
//
//
//case class MethodCalls(method : Any)
//  case class SubmitState(value : Any)
//  case class GetHistory()
//  case class GetCommittedHistory()
//  case class GetProposedHistory()
//  case class GetSystemNodes()
//  case class GetFollowerNodes()
//  case class GetLeaderRef()
//  case class GetCurrentState()
//  case class ForceReElection()
//
//// heart beat classes used by zab protocol
//case class ZabHeartbeat(method : Any)
//  case class LeaderHeartbeat()
//  case class LeaderBoopidiboop()
//  case class LeaderBeaterChecker()
//  case class FollowerHeartbeat()
//  case class FollowerBoopidiboop()
//  case class FollowerBeaterChecker(follower : ActorRef)
//
//// leader cases
//case class FollowerInfo(e : Long, lastZXID : ZXID)
//case class AckEpoch(currentEpoch : Long, history : ArrayBuffer[Proposal], lastZXID : ZXID)
//case class AckNewLeader()
//case class Write(message : Any)
//case class Ack(proposal : Proposal)
//
//// follower cases
//case class NewEpoch(e : Long)
//case class NewLeader(e : Long, History : ArrayBuffer[Proposal])
//case class Commit(transaction : Any)
//case class TakeProposal(proposal : Proposal)
//case class NotLeader()
//
//// phase 0 cases
//case class CurrentLeader(actor : ActorRef)
//case class HighestZXID(zxid : ZXID)
//case class Drained()
//
//// special phase timers
//case class Phase0Timer()
//case class Phase1Timer()
//
//// initialization cases
//case class InitialSetupComplete()
//
//
//
//class ZAB_Actor(bootstrap : URL) extends Actor{
//
//  // values used to handle incoming submission requests
//  private val writeCommands : mutable.Queue[Any] = new mutable.Queue[Any]()
//
//  // list used to maintain connection with the system
//  private var systemList : Set[ActorRef] = Set[ActorRef]()
//  private var tempSystemList : Set[ActorRef] = Set[ActorRef]()
//
//  // values specifically used by a leader
//  private var counter : Long = 0
//  private var followerList : mutable.HashMap[ActorRef, Boolean] = new mutable.HashMap[ActorRef, Boolean]()
//  private var leader : Boolean = false
//  private var recievedQuorum : Boolean = false
//  private var followerZXID : mutable.HashMap[ActorRef, ZXID] = new mutable.HashMap[ActorRef, ZXID]()
//  private var updatedHistory : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//
//  // values used by a follower
//  private var leaderRef : ActorRef = _
//  private var leaderHeart : Boolean = false
//  private var ready : Boolean = false
//
//  // values used for ZAB protocol
//  private var lastZXID : ZXID = new ZXID(0, 0)
//  private var lastCommitedZXID : ZXID = new ZXID(0, 0) // will need to use to specify committed and non-committed proposals.
//  private var history : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//  private var acceptedEpoch : Long = 0
//  private var currentEpoch : Long = 0
//
//  // phase 0 values
//  private var drained : Boolean = false
//  private var highestZXID : ZXID = _
//  private var possibleLeader : ActorRef = _
//  private var possibleLeaders : mutable.HashMap[ActorRef, Long] = new mutable.HashMap[ActorRef, Long]()
//
//  // phase 1 values
//  private var highestEpoch : Long = 0 // acceptedEpoch
//  private var otherHighestEpoch : Long = 0 // currentEpoch
//  private var otherHighestZXID : ZXID = new ZXID(0, 0)
//
//  // phase 2&3 values
//  private var holderProposal : Proposal = _
//  private var committedZXIDs : ArrayBuffer[ZXID] = new ArrayBuffer[ZXID]()
//  private var proposedTransactions : mutable.HashMap[Proposal, ArrayBuffer[ActorRef]] = new mutable.HashMap[Proposal, ArrayBuffer[ActorRef]]()
//
//  initiateSystemCommunication(bootstrap)
//
//  // methods used to deal with transactions of data
//  private def ready(epoch : Long){
//    if(!this.ready){
//      this.ready = true
//      // will start sending update messages to leader
//      if(this.writeCommands.nonEmpty){
//        while(this.writeCommands.nonEmpty){
//          this.leaderRef ! Write(this.writeCommands.dequeue())
//        }
//      }
//    }
//  }
//
//  // method used to propose messages.
//  // proposed messages are added to the history without updating
//  // the location of the last committed proposal
//  private def propose(proposal : Proposal){ // won't add proposal to history if their is another value with larger zxid
//    if(this.history.isEmpty || this.history.last.getTransaction.getZXID < proposal.getTransaction.getZXID){
//      this.history += proposal
//      this.lastZXID = proposal.getTransaction.getZXID
//    }
//  }
//
//  // method used to commit messages that have already
//  // been proposed.
//  // if the commit message is for a proposal that is
//  // after another message that has been proposed
//  // but not yet committed, then this will not commit
//  // the current proposal but instead make note that it
//  // has been approved for commitment once the program
//  // gets to that proposal.
//  @tailrec
//  private def commit(proposal : Proposal){
//    var i : Int = this.history.size-1
//    var break : Boolean = false
//    var position : Int = -1
//    while(i >= 0 && !break && this.history(i).getTransaction.getZXID != this.lastCommitedZXID){
//      if(this.history(i).getTransaction.getZXID == proposal.getTransaction.getZXID){
//        position = i
//        break = true
//      }
//      i = i - 1
//    }
//    if(position == -1)
//      return
//    else if(position == 0)
//      this.lastCommitedZXID = proposal.getTransaction.getZXID
//    else if(this.history(position - 1).getTransaction.getZXID == this.lastCommitedZXID)
//      this.lastCommitedZXID = proposal.getTransaction.getZXID
//    else committedZXIDs += proposal.getTransaction.getZXID
//
//    // checks to see if the next item in history was previously requested for commitment but couldn't
//    // if so, runs commit on the next item as well.
//    if(this.history.size - 1 > position && this.committedZXIDs.contains(this.history(position + 1).getTransaction.getZXID)){
//      this.committedZXIDs.remove(this.committedZXIDs.indexOf(this.history(position + 1).getTransaction.getZXID))
//      this.commit(this.history(position + 1))
//    }
//  }
//
//
//  // handles all messages related to overall system maintenance
//  private def handleMaintenance(condition : Any, actor : ActorRef) : Unit = {
//    condition match {
//      /*
//       * checks to see which nodes in the system have
//       * responded to the previous heartbeat in the
//       * given time. Then will resend the heartbeat to current
//       * members of the system and start a new countdown
//       * to run this case.
//       */
//      case UpdateSystemConnection() =>
//        val addedMembers : Set[ActorRef] = tempSystemList -- systemList
//        val removedMembers : Set[ActorRef] = systemList -- tempSystemList
//
//        systemList = Set[ActorRef]() // updates the list of nodes in system
//
//        for(currentRef : ActorRef <- tempSystemList){
//          systemList += currentRef
//        }
//
//        tempSystemList = Set[ActorRef]()
//
//        self ! SystemChanges(addedMembers, removedMembers)
//
//        for(current : ActorRef <- systemList){ // sends a heartbeat to each node in system
//          current ! SystemMaintenance(GetSystemInformation())
//        }
//
//        // waits awhile until ready to check again
//        context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, SystemMaintenance(UpdateSystemConnection()))
//      /*
//       * received when sender is looking for a heartbeat or first
//       * joining the system. receiver will sends its list
//       * of nodes in system as a reply using ReceivedSystemInformation
//       * If receiver hasn't seen this node in the system before,
//       * receiver adds them to their temp list.
//       */
//      case GetSystemInformation() =>
//        //println("responding to heartbeat from " + sender.path.toString())
//        if(!systemList.contains(sender) && !tempSystemList.contains(sender)){
//          tempSystemList = tempSystemList + sender
//        }
//        sender ! SystemMaintenance(ReceivedSystemInformation(systemList))
//      /*
//       * sent when sender wishes to know if a leader currently exists.
//       * receiver will send a ref to that leader using CurrentLeader()
//       * ref in responce. A message will not be sent if the node
//       * doesn't currently have a leader.
//       */
//      case GetLeader() =>
//        if(leaderRef != null){
//          sender ! CurrentLeader(leaderRef)
//        }
//      /*
//       * Sent when sender is responding to previous GetSystemInformation.
//       * receiver will first acknowledge the senders responce by adding them
//       * to the tempSystemList. Then, if their are any new members in the senders
//       * list that aren't in the receivers, will send them a heartbeat.
//       */
//      case ReceivedSystemInformation(members : Set[ActorRef]) =>
//        tempSystemList = tempSystemList + sender
//        val newMembers : Set[ActorRef] = members -- (tempSystemList ++ systemList)
//
//        for(current : ActorRef <- newMembers){ // sends a heartbeat to each node in system
//          current ! SystemMaintenance(GetSystemInformation())
//        }
//      /*
//       * sent when sender wishes to know the highest transaction
//       * done by receiver. Receiver respondes by sending it's HighestZxid value
//       */
//      case GetHighestZXID() =>
//        sender ! HighestZXID(lastZXID)
//      case _ =>
//    }
//  }
//
//  // handles all method related calls from the ZAB_Impl
//  // all of these cases will be ? rather than ! so they
//  // must return something.
//  private def methodResponse(method : Any) : Any = {
//    method match {
//      case SubmitState(value : Any) =>
//        if(this.ready) this.leaderRef ! Write(value)
//        else this.writeCommands += value
//      case GetHistory() => this.history
//      case GetCommittedHistory() =>
//        var i : Int = 0
//        var committedHistory : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//        while(i < this.history.size){
//        if(this.history(i).getTransaction.getZXID > this.lastCommitedZXID){
//           return committedHistory
//        }
//        committedHistory += this.history(i)
//        i = i + 1
//        }
//        committedHistory
//      case GetProposedHistory() =>
//        var i : Int = this.history.size - 1
//        var proposedHistory : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//        while(i >= 0){
//        if(!(this.history(i).getTransaction.getZXID > this.lastCommitedZXID)){
//           return proposedHistory.reverse
//        }
//        proposedHistory += this.history(i)
//        i = i - 1
//        }
//        proposedHistory.reverse
//      case GetSystemNodes() => systemList
//      case GetFollowerNodes() =>
//        if(this.leader) this.followerList.keySet.toSet
//        else new ArrayBuffer[ActorRef]().toSet
//      case GetLeaderRef() => this.leaderRef
//      case GetCurrentState() =>
//        var i : Int = this.history.size - 1
//        while(i >= 0){
//          if(this.history(i).getTransaction.getZXID == this.lastCommitedZXID)
//            return this.history(i)
//          else i = i - 1
//        } // while
//      case ForceReElection() => initiatePhase0()
//      case _ =>
//    }
//  }
//
//  // deals with messages related to the heart beats
//  // used in the Zab protocol
//  private def heartBeatDealer(method : Any, sender : ActorRef) : Unit = {
//    method match {
//      case LeaderHeartbeat() =>
//        if(this.leader) sender ! ZabHeartbeat(LeaderBoopidiboop())
//      case LeaderBoopidiboop() => this.leaderHeart = true
//      case LeaderBeaterChecker() =>
//        if(this.leaderHeart){
//         this.leaderHeart = false
//         if(this.leaderRef != null){
//           this.leaderRef ! ZabHeartbeat(LeaderHeartbeat())
//           context.system.scheduler.scheduleOnce(Duration(1000, TimeUnit.MILLISECONDS), self, ZabHeartbeat(LeaderBeaterChecker()))
//          }
//        }
//        else initiatePhase0()
//      case FollowerHeartbeat() => sender ! ZabHeartbeat(FollowerBoopidiboop())
//      case FollowerBoopidiboop() =>
//        if(this.followerList.contains(sender)) this.followerList(sender) = true
//      case FollowerBeaterChecker(follower : ActorRef) =>
//        if(this.leader && this.followerList.contains(follower)){
//          if(this.followerList(follower)){
//            this.followerList(follower) = false
//            follower ! ZabHeartbeat(FollowerHeartbeat())
//            context.system.scheduler.scheduleOnce(Duration(1000, TimeUnit.MILLISECONDS), self, ZabHeartbeat(FollowerBeaterChecker(follower)))
//          }
//          else self ! FollowerLost(follower)
//        }
//    }
//  }
//
//  //******* Phase Initiation Functions *******
//  private def initiateSystemCommunication(bootstrap : URL) : Unit = {
//    if(bootstrap != null){
//      val address : String = "akka.tcp://ChordSystem" + bootstrap.getPort + "@" + bootstrap.getHost + ":" + bootstrap.getPort + "/user/nodeImpl"
//      val selection : ActorSelection = context.actorSelection(address)
//      selection ! SystemMaintenance(GetSystemInformation())
//    }
//    self ! SystemMaintenance(GetSystemInformation())
//
//    context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, InitialSetupComplete())
//  }
//
//  private def initiatePhase0() : Unit = {
//    println("Initiating phase 0")
//
//    this.leader = true
//    this.leaderRef = null
//    highestZXID = lastZXID
//    possibleLeader = self
//    possibleLeaders = new mutable.HashMap[ActorRef, Long]()
//
//    context.become(phase0)
//
//    this.drained = false
//    self ! Drained()
//
//    for(currentNode : ActorRef <- systemList){
//      currentNode ! SystemMaintenance(GetLeader())
//      currentNode ! SystemMaintenance(GetHighestZXID())
//    }
//
//    context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, Phase0Timer())
//
//  }
//
//  private def initiatePhase1() : Unit = {
//    println("Initiating phase 1")
//
//    highestEpoch = this.acceptedEpoch
//    followerList = new mutable.HashMap[ActorRef, Boolean]()
//    followerZXID = new mutable.HashMap[ActorRef, ZXID]()
//    leaderHeart = false
//    recievedQuorum = false
//    updatedHistory = new ArrayBuffer[Proposal]()
//
//    context.become(phase1)
//
//    if(leader){
//      context.system.scheduler.scheduleOnce(Duration.create(5, TimeUnit.SECONDS), self, Phase1Timer())
//    }
//    else{  //Not necessary, if leader times out, follower will need to go back to phase 0.
//      context.system.scheduler.scheduleOnce(Duration.create(1, TimeUnit.SECONDS), self, Phase1Timer())
//    }
//
//    leaderRef ! FollowerInfo(this.acceptedEpoch, this.lastZXID)
//  }
//
//  private def initiatePhase2() : Unit = {
//    println("Initiating phase 2")
//
//    holderProposal = new Proposal(0, new Transaction("", new ZXID(0,0)))
//    committedZXIDs = new ArrayBuffer[ZXID]()
//    proposedTransactions = new mutable.HashMap[Proposal, ArrayBuffer[ActorRef]]()
//    proposedTransactions  += ((holderProposal, new ArrayBuffer[ActorRef]()))
//
//    context.become(phase2)
//
//    if(leader){
//      for(currentRef : ActorRef <- this.followerList.keysIterator){
//        val currentZXID : ZXID = this.followerZXID.apply(currentRef)
//        var i : Int = 0
//        var switch : Boolean = false
//        val missingKnowledge : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//        /**
//         * runs through each element in the complete history until it finds
//         * the last element that the current zab node had. After that, it
//         * adds each proposal after that to a new list which will be sent to
//         * that specific node. This is done for each follower of the leader.
//         */
//        while(i < this.history.size){
//          if(switch){
//            missingKnowledge += this.history(i)
//          }
//          else if(this.history(i).getTransaction.getZXID > lastZXID){
//              switch = true
//              missingKnowledge += this.history(i)
//          }
//
//          i = i + 1
//        }
//
//        currentRef ! NewLeader(this.acceptedEpoch, missingKnowledge)
//
//      }
//    }
//
//  }
//
//  private def initiatePhase3() : Unit = {
//    println("Initiating phase 3")
//
//    this.proposedTransactions = new mutable.HashMap[Proposal, ArrayBuffer[ActorRef]]()
//    this.counter = 0
//
//    context.become(phase3)
//
//    this.ready(this.currentEpoch)
//  }
//
//  //******* Phase Match Cases *******
//
//  // Leader Election
//  def phase0 : Receive = {
//    case SystemMaintenance(condition) => this.handleMaintenance(condition, sender)
//    case MethodCalls(method) => sender ! this.methodResponse(method)
//
//    case CurrentLeader(actor : ActorRef) =>
//      //println("current leader from " + sender.path.toString())
//      if(this.drained){
//        if(actor != null && possibleLeaders.contains(actor))
//          possibleLeaders(actor) = possibleLeaders(actor) + 1
//        else possibleLeaders += ((actor, 1))
//      }
//    case HighestZXID(zxid : ZXID) =>
//      //println("recieved highest zxid from " + sender.path.toString())
//      if(this.drained){
//        if(highestZXID == null){
//          highestZXID = zxid
//        }
//        else{
//          if(highestZXID < zxid){
//            highestZXID = zxid
//            possibleLeader = sender
//          }
//        }
//      }
//    case Drained() => this.drained = true
//
//    //any position cases
//    case SystemChanges(added : Set[ActorRef], removed : Set[ActorRef]) =>
//      if(drained){
//        for(currentNode : ActorRef <- added){
//          currentNode ! GetLeader()
//          currentNode ! GetHighestZXID()
//        }
//        for(currentNode : ActorRef <- removed){
//          if(possibleLeaders.contains(currentNode)){
//            possibleLeaders.remove(currentNode)
//          }
//        }
//      }
//
//    case Phase0Timer() =>
//      if(possibleLeaders.nonEmpty){
//        var nextLeader : ActorRef = null
//        var memberSize : Long = 0
//        for((currentNode : ActorRef, size : Long )<- possibleLeaders.iterator){
//          if(memberSize < size){
//            nextLeader = currentNode
//            memberSize = size
//          }
//        }
//        leaderRef = nextLeader
//      }
//      else if(possibleLeader != null){
//        leaderRef = possibleLeader
//      }
//
//      leader = leaderRef.equals(self)
//      initiatePhase1()
//
//    // possible phase 1 message
//    case FollowerInfo(e : Long, lastZXID : ZXID) =>
//      // places back in mailbox just as it was
//      // until ready to look at in phase 1.
//      self.tell(FollowerInfo(e, lastZXID), sender)
//
//    case _ =>
//  }
//
//  // Discovery
//  def phase1 : Receive = {
//    case SystemMaintenance(condition) => this.handleMaintenance(condition, sender)
//    case MethodCalls(method) => sender ! this.methodResponse(method)
//    case ZabHeartbeat(method) => this.heartBeatDealer(method, sender)
//
//    //leader cases
//    case FollowerInfo(e : Long, notUsedZXID : ZXID) =>
//      if(leader){
//        if(!recievedQuorum && e > this.highestEpoch){
//          this.highestEpoch = e
//        }
//        if(!this.followerList.contains(sender)){
//          this.followerList += ((sender, true))
//          self ! ZabHeartbeat(FollowerBeaterChecker(sender))
//        }
//        if(recievedQuorum){
//          sender ! NewEpoch(highestEpoch)
//        }
//        else{
//          if(this.followerList.size > (this.systemList.size/2)){
//            recievedQuorum = true
//            highestEpoch = highestEpoch + 1
//            for(currentFollower : ActorRef <- this.followerList.keys){
//              currentFollower ! NewEpoch(highestEpoch)
//            }
//            otherHighestEpoch = this.currentEpoch
//            otherHighestZXID = this.lastZXID
//            updatedHistory = this.history
//          }
//        }
//      }
//      else{
//        sender ! NotLeader()
//      }
//    case AckEpoch(currentEpoch : Long, history : ArrayBuffer[Proposal], lastZXID : ZXID) =>
//      if(leader && recievedQuorum){
//        if(!followerZXID.contains(sender) && followerList.contains(sender)){
//          followerZXID += ((sender, lastZXID))
//          if(otherHighestEpoch < currentEpoch || (otherHighestEpoch == currentEpoch && otherHighestZXID < lastZXID)){
//            otherHighestEpoch = currentEpoch
//            otherHighestZXID = lastZXID
//            updatedHistory = history
//          }
//        }
//        if(followerZXID.size >= followerList.size){
//          this.history = updatedHistory
//          initiatePhase2()
//        }
//      }
//    case FollowerLost(follower : ActorRef) =>
//      if(leader){
//        if(followerList.contains(follower))
//          followerList.remove(follower)
//
//        if(followerZXID.contains(follower))
//          followerZXID.remove(follower)
//
//        if(recievedQuorum)
//          if(this.followerList.size <= (this.systemList.size/2))
//            initiatePhase0()
//
//          if(followerZXID.size >= followerList.size){
//            this.history = updatedHistory
//            initiatePhase2()
//          }
//        }
//    //follower cases
//    case NewEpoch(e : Long) =>
//      if(sender == this.leaderRef){
//        if(e >= this.acceptedEpoch){ // different from setup but allows one to force a node into re-election without crashing.
//          this.acceptedEpoch = e
//          leaderHeart = true
//          self ! ZabHeartbeat(LeaderBeaterChecker())
//          leaderRef ! AckEpoch(this.currentEpoch, this.history, this.lastZXID)
//          if(!leader) initiatePhase2()
//        }
//        else initiatePhase0()
//      }
//    case NotLeader() => if(sender == this.leaderRef) initiatePhase0()
//
//
//    //any position cases
//    case SystemChanges(added : Set[ActorRef], removed : Set[ActorRef]) =>
//      if(leader){
//        if(recievedQuorum && this.followerList.size <= (this.systemList.size/2))
//          initiatePhase0()
//        else if(!recievedQuorum && this.followerList.size > this.systemList.size /2){
//            recievedQuorum = true
//            highestEpoch = highestEpoch + 1
//            for(currentFollower : ActorRef <- this.followerList.keys){
//              currentFollower ! NewEpoch(highestEpoch)
//            }
//            otherHighestEpoch = this.currentEpoch
//            otherHighestZXID = this.lastZXID
//            updatedHistory = this.history
//          }
//        else if(recievedQuorum && followerZXID.size >= followerList.size){
//            this.history = updatedHistory
//            initiatePhase2()
//          }
//      }
//      else if(removed.contains(leaderRef))
//          initiatePhase0()
//
//    case Phase1Timer() => initiatePhase0()
//
//    case _ =>
//  }
//
//  // Synchronization
//  def phase2 : Receive = {
//    case SystemMaintenance(condition) => this.handleMaintenance(condition, sender)
//    case MethodCalls(method) => sender ! this.methodResponse(method)
//    case ZabHeartbeat(method) => this.heartBeatDealer(method, sender)
//
//    //leader cases
//    case AckNewLeader() =>
//      if(leader){
//        val acceptedTransactionList : ArrayBuffer[ActorRef] = this.proposedTransactions.apply(holderProposal)
//        if(!acceptedTransactionList.contains(sender)){
//          acceptedTransactionList += sender
//          if(acceptedTransactionList.size > (this.followerList.size/2)){
//            for(currentFollower : ActorRef <- this.followerList.keysIterator){
//              currentFollower ! Commit(0L)
//            }
//            initiatePhase3()
//          }
//        }
//      }
//    case FollowerLost(follower : ActorRef) =>
//      if(leader){
//        if(followerList.contains(follower))
//          followerList.remove(follower)
//        if(this.followerList.size <= (this.systemList.size/2))
//          initiatePhase0()
//        if(this.proposedTransactions.apply(holderProposal).size > (this.followerList.size/2)){
//          for(currentFollower : ActorRef <- this.followerList.keysIterator){
//            currentFollower ! Commit(Unit)
//          }
//          initiatePhase3()
//        }
//      }
//
//    //follower cases
//    case NewLeader(e : Long, updateHistory : ArrayBuffer[Proposal]) =>
//      if(sender == this.leaderRef){
//        if(e != this.acceptedEpoch){
//          initiatePhase0()
//        }
//        else{
//          this.currentEpoch = e
//          for(currentProposal : Proposal <- updateHistory){
//            val currentTransaction : Transaction = currentProposal.getTransaction
//            val newProposal : Proposal = new Proposal(e, currentTransaction)
//            this.propose(newProposal)
//          }
//          sender ! AckNewLeader()
//        }
//      }
//    case Commit(value : Long) =>
//      if(sender == this.leaderRef){
//        var switch : Boolean = false
//        var i : Int = 0
//        while(i < this.history.size){
//          if(switch) this.commit(this.history(i))
//          else if(this.history(i).getTransaction.getZXID > this.lastCommitedZXID){
//            switch = true
//            this.commit(this.history(i))
//          }
//
//          i = i + 1
//        }
//        if(!leader) initiatePhase3()
//      }
//
//    //any position cases
//    case SystemChanges(added : Set[ActorRef], removed : Set[ActorRef]) =>
//      if(leader){
//        for(currentMember : ActorRef <- removed){
//          if(followerList.contains(currentMember))
//            followerList.remove(currentMember)
//          if(this.followerList.size <= (this.systemList.size/2))
//            initiatePhase0()
//          if(this.proposedTransactions.apply(holderProposal).size > (this.followerList.size/2)){
//            for(currentFollower : ActorRef <- this.followerList.keysIterator)
//              currentFollower ! Commit(0L)
//            initiatePhase3()
//          }
//        }
//      }
//      else if(removed.contains(leaderRef))
//          initiatePhase0()
//
//    case _ =>
//  }
//
//  // Broadcast
//  def phase3 : Receive = {
//    case SystemMaintenance(condition) => this.handleMaintenance(condition, sender)
//    case MethodCalls(method) => sender ! this.methodResponse(method)
//    case ZabHeartbeat(method) => this.heartBeatDealer(method, sender)
//
//    // leader cases
//    case Write(message : Any) =>
//      if(this.leader){
//        this.counter = counter + 1
//        val nextZXID : ZXID = new ZXID(this.currentEpoch, this.counter)
//        val nextTransaction : Transaction = new Transaction(message, nextZXID)
//        val nextProposal : Proposal = new Proposal(this.currentEpoch, nextTransaction)
//        this.proposedTransactions += ((nextProposal, new ArrayBuffer[ActorRef]()))
//        for(currentFollower : ActorRef <- this.followerList.keysIterator)
//          currentFollower ! TakeProposal(nextProposal)
//      }
//    case Ack(proposal : Proposal) =>
//      if(this.leader){
//        if(this.proposedTransactions.contains(proposal)){
//          val currentList : ArrayBuffer[ActorRef] = this.proposedTransactions.apply(proposal)
//          if(!currentList.contains(sender)){
//            currentList += sender
//            if(currentList.size > (this.followerList.size/2)){
//              for(currentFollower : ActorRef <- this.followerList.keysIterator){
//                currentFollower ! Commit(proposal)
//              }
//              this.proposedTransactions.remove(proposal)
//            }
//          }
//        }
//      }
//
//    case FollowerInfo(e : Long, lastZXID : ZXID) =>
//      if(leader){
//        val missingKnowledge : ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
//        var switch : Boolean = false
//        var i : Int = 0
//        /**
//         * runs through each element in the complete history until it finds
//         * the last element that the current zab node had. After that, it
//         * adds each proposal after that to a new list which will be sent to
//         * that specific node. This is done for each follower of the leader.
//         */
//        while(i < this.history.size){
//          if(switch) missingKnowledge += this.history(i)
//          else if(this.history(i).getTransaction.getZXID > lastZXID){
//              switch = true
//              missingKnowledge += this.history(i)
//          }
//
//          i = i + 1
//        }
//
//        sender ! NewEpoch(this.currentEpoch)
//        sender ! NewLeader(currentEpoch, missingKnowledge)
//      }
//      else{
//        sender ! NotLeader()
//      }
//    case AckNewLeader() =>
//      if (this.leader){
//        if(!this.followerList.contains(sender)){
//          sender ! Commit(0L)
//          this.followerList += ((sender, true))
//          self ! ZabHeartbeat(FollowerBeaterChecker(sender))
//        }
//        else sender ! Commit(0L)
//      }
//    case FollowerLost(follower : ActorRef) =>
//      if(leader && followerList.contains(follower)){
//        followerList.remove(follower)
//        if(this.followerList.size <= (this.systemList.size/2))
//          initiatePhase0()
//
//        for(proposedProposal : Proposal <- this.proposedTransactions.keys){
//          val currentList : ArrayBuffer[ActorRef] = this.proposedTransactions.apply(proposedProposal)
//          if(currentList.contains(follower)){
//            currentList.remove(currentList.indexOf(follower))
//          }
//          else{
//            if(currentList.size > (this.followerList.size/2)){
//              for(currentFollower : ActorRef <- this.followerList.keysIterator){
//                currentFollower ! Commit(proposedProposal)
//              }
//              this.proposedTransactions.remove(proposedProposal)
//            }
//          }
//        }
//      }
//
//    // follower cases
//    case TakeProposal(proposal : Proposal) =>
//      if(sender == this.leaderRef){
//        this.propose(proposal)
//        sender ! Ack(proposal)
//      }
//    case Commit(proposal : Proposal) => this.commit(proposal)
//
//    //any position cases
//    case SystemChanges(added : Set[ActorRef], removed : Set[ActorRef]) =>
//      if(leader){
//        for(currentMember : ActorRef <- removed){
//          if(followerList.contains(currentMember)){
//            followerList.remove(currentMember)
//            if(this.followerList.size <= (this.systemList.size/2))
//              initiatePhase0()
//
//            for(proposedProposal : Proposal <- this.proposedTransactions.keys){
//              val currentList : ArrayBuffer[ActorRef] = this.proposedTransactions.apply(proposedProposal)
//              if(currentList.contains(currentMember))
//                currentList.remove(currentList.indexOf(currentMember))
//              else{
//                if(currentList.size > (this.followerList.size/2)){
//                  for(currentFollower : ActorRef <- this.followerList.keysIterator){
//                    currentFollower ! Commit(proposedProposal)
//                  }
//                  this.proposedTransactions.remove(proposedProposal)
//                }
//              }
//            }
//          }
//          else if(this.followerList.size <= (this.systemList.size/2)) initiatePhase0()
//        }
//      }
//      else if(removed.contains(leaderRef)) initiatePhase0()
//
//    case _ =>
//  }
//
//  // Initial System Connection
//  def receive: Receive = {
//    case SystemMaintenance(condition) => this.handleMaintenance(condition, sender)
//    case MethodCalls(method) => sender ! this.methodResponse(method)
//
//    case InitialSetupComplete() =>
//      systemList = tempSystemList // updates the list of nodes in system
//      tempSystemList = Set[ActorRef]()
//
//      for(current : ActorRef <- systemList) // sends a heartbeat to each node in system
//        current ! SystemMaintenance(GetSystemInformation())
//
//      // waits awhile until ready to check again
//      context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, SystemMaintenance(UpdateSystemConnection()))
//
//      initiatePhase0()
//
//    case _ =>
//  }
//
//}