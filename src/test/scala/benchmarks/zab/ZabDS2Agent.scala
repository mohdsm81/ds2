package benchmarks.zab

/** **************************************************************************
 * *
 * ZAB_Actor.scala                                                           *
 * -------------------                                                       *
 *
 * @author Mohammed S. Al-Mahfoudh                                           *
 *         Based on Zab implementation by Heath French                               *
 *         date                 : 11.22.2020                                         *
 *         email                : mahfoudh@cs.utah.edu                               *
 *         *
 *         *
 *         ************************************************************************* */

/** *************************************************************************
 * *
 * This program is free software; you can redistribute it and/or modify  *
 * it under the terms of the GNU General Public License as published by  *
 * the Free Software Foundation; either version 2 of the License, or     *
 * (at your option) any later version.                                   *
 * *
 * A copy of the license can be found in the license.txt file supplied   *
 * with this software or at: http://www.gnu.org/copyleft/gpl.html        *
 * *
 * ************************************************************************* */

import akka.actor._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class SystemMaintenance(condition: Any) extends
  Message("SystemMaintenance", Seq(condition))

case class UpdateSystemConnection() extends
  Message("UpdateSystemConnection")

case class GetSystemInformation() extends
  Message("GetSystemInformation")

case class GetLeader() extends
  Message("GetLeader")

case class ReceivedSystemInformation(members: Set[Agent]) extends
  Message("ReceivedSystemInformation", Seq(members))

case class GetHighestZXID() extends
  Message("GetHighestZXID")

//-------------------------------------------------------------------------
// different in every phase but still connected to maintenance
//-------------------------------------------------------------------------
case class SystemChanges(added: Set[Agent], removed: Set[Agent]) extends
  Message("SystemChanges", Seq(added, removed))

case class FollowerLost(removed: Agent) extends
  Message("FollowerLost", Seq(removed))

case class MethodCalls(method: Any) extends
  Message("MethodCalls", Seq(method))

case class SubmitState(value: Any) extends
  Message("SubmitState", Seq(value))

case class GetHistory() extends
  Message("GetHistory")

case class GetCommittedHistory() extends
  Message("GetCommittedHistory")

case class GetProposedHistory() extends
  Message("GetProposedHistory")

case class GetSystemNodes() extends
  Message("GetSystemNodes")

case class GetFollowerNodes() extends
  Message("GetFollowerNodes")

case class GetLeaderRef() extends
  Message("GetLeader")

case class GetCurrentState() extends
  Message("GetCurrentState")

case class ForceReElection() extends
  Message("ForceReElection")

//-------------------------------------------------------------------------
// heart beat classes used by zab protocol
//-------------------------------------------------------------------------
case class ZabHeartbeat(method: Any) extends
  Message("ZabHeartbeat", Seq(method))

case class LeaderHeartbeat() extends
  Message("LeaderHeartbeat")

case class LeaderBoopidiboop() extends
  Message("LeaderBoopidiboop")

case class LeaderBeaterChecker() extends
  Message("LeaderBeaterChecker")

case class FollowerHeartbeat() extends
  Message("FollowerHeartbeat")

case class FollowerBoopidiboop() extends
  Message("FollowerBoopidiboop")

case class FollowerBeaterChecker(follower: Agent) extends
  Message("FollowerBeaterChecker", Seq(follower))

//-------------------------------------------------------------------------
// leader cases
//-------------------------------------------------------------------------
case class FollowerInfo(e: Long, lastZXID: ZXID) extends
  Message("FollowerInfo", Seq(e, lastZXID))

case class AckEpoch(currentEpoch: Long, history: ArrayBuffer[Proposal], lastZXID: ZXID) extends
  Message("AckEpoch", Seq(currentEpoch, history, lastZXID))

case class AckNewLeader() extends
  Message("AckNewLeader")

case class Write(message: Any) extends
  Message("Write")

case class Ack(proposal: Proposal) extends
  Message("Ack")

//-------------------------------------------------------------------------
// follower cases
//-------------------------------------------------------------------------
case class NewEpoch(e: Long) extends
  Message("NewEpoch", Seq(e))

case class NewLeader(e: Long, history: ArrayBuffer[Proposal]) extends
  Message("NewLeader", Seq(e, history))

case class Commit(transaction: Any) extends
  Message("Commit", Seq(transaction))

case class TakeProposal(proposal: Proposal) extends
  Message("TakeProposal", Seq(proposal))

case class NotLeader() extends
  Message("NotLeader")

//-------------------------------------------------------------------------
// phase 0 cases
//-------------------------------------------------------------------------
case class CurrentLeader(actor: Agent) extends
  Message("CurrentLeader", Seq(actor))

case class HighestZXID(zxid: ZXID) extends
  Message("HighestZXID", Seq(zxid))

case class Drained() extends
  Message("Drained")

//-------------------------------------------------------------------------
// special phase timers
//-------------------------------------------------------------------------
case class Phase0Timer() extends
  Message("Phase0Timer")

case class Phase1Timer() extends
  Message("Phase1Timer")

//-------------------------------------------------------------------------
// initialization cases
//-------------------------------------------------------------------------
case class InitialSetupComplete() extends
  Message("InitialSetupComplete")

///////////////////////////////////////////////////////////////////////////
class ZabDS2Agent(bootstrap: String) extends Agent(bootstrap) {
  ///////////////////////////////////////////////////////////////////////////

  // used to send messages
  val MSG_TO_SEND = s"messageToSend${LocalState.DELIM}Message"
  val DST_AGENT = s"dstAgent${LocalState.DELIM}Agent"
  // to be used to store functions parameters dynamically
  val AGENTS_ITER = s"agentsIter${LocalState.DELIM}Iterator[Agent]"

  // values used to handle incoming submission requests
  //  private val writeCommands: mutable.Queue[Any] = new mutable.Queue[Any]()
  val WRITE_COMMANDS = s"writeCommands${LocalState.DELIM}mutable.Queue[Any]"
  localState(WRITE_COMMANDS) = mutable.Queue[Any]()
  type WRITE_COMMANDS_TYPE = mutable.Queue[Any]

  //---------------------------------------------------------------
  // list used to maintain connection with the system
  //---------------------------------------------------------------
  //  private var systemList: Set[Agent] = Set[Agent]()
  val SYSTEM_LIST = s"systemList${LocalState.DELIM}Set[Agent]"
  localState(SYSTEM_LIST) = Set[Agent]()
  //  private var tempSystemList: Set[Agent] = Set[Agent]()
  val TEMP_SYSTEM_LIST = s"tempSystemList${LocalState.DELIM}Set[Agent]"
  localState(TEMP_SYSTEM_LIST) = Set[Agent]()

  type SYSTEM_LIST_TYPE = Set[Agent]

  //---------------------------------------------------------------
  // values specifically used by a leader
  //---------------------------------------------------------------
  //  private var counter: Long = 0
  val COUNTER = s"counter${LocalState.DELIM}Long"
  localState(COUNTER) = 0

  //  private var followerList: mutable.HashMap[Agent, Boolean] = new mutable.HashMap[Agent, Boolean]()
  val FOLLOWER_LIST = s"followerList${LocalState.DELIM}mutable.HashMap[Agent, Boolean]"
  localState(FOLLOWER_LIST) = mutable.HashMap[Agent, Boolean]()
  type FOLLOWER_LIST_TYPE = mutable.HashMap[Agent, Boolean]

  //  private var leader: Boolean = false
  val LEADER = s"leader${LocalState.DELIM}Boolean"
  localState(LEADER) = false

  //  private var receivedQuorum: Boolean = false
  val RECEIVED_QUORUM = s"receivedQuorum${LocalState.DELIM}Boolean"
  localState(RECEIVED_QUORUM) = false

  //  private var followerZXID: mutable.HashMap[Agent, ZXID] = new mutable.HashMap[Agent, ZXID]()
  val FOLLOWER_ZXID = s"followerZXID${LocalState.DELIM}mutable.HashMap[Agent,ZXID]"
  localState(FOLLOWER_ZXID) = mutable.HashMap[Agent, ZXID]()
  type FOLLOWER_ZXID_TYPE = mutable.HashMap[Agent, ZXID]

  //  private var updatedHistory: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
  val UPDATED_HISTORY = s"updateHistory${LocalState.DELIM}ArrayBuffer[Proposal]"
  localState(UPDATED_HISTORY) = ArrayBuffer[Proposal]()
  type UPDATE_HISTORY_TYPE = ArrayBuffer[Proposal]

  //---------------------------------------------------------------
  // values used by a follower
  //---------------------------------------------------------------
  //  private var leaderRef: Agent = _
  val LEADER_REF = s"leaderRef${LocalState.DELIM}Agent"
  localState(LEADER_REF) = null

  //  private var leaderHeart: Boolean = false
  val LEADER_HEART = s"leaderHeart${LocalState.DELIM}Boolean"
  localState(LEADER_HEART) = false

  //  private var ready: Boolean = false
  val READY = s"ready${LocalState.DELIM}Boolean"
  localState(READY) = false

  //---------------------------------------------------------------
  // values used for ZAB protocol
  //---------------------------------------------------------------
  //  private var lastZXID: ZXID = new ZXID(0, 0)
  val LAST_ZXID = s"lastZXID${LocalState.DELIM}ZXID"
  localState(LAST_ZXID) = new ZXID(0, 0)

  //  private var lastCommittedZXID: ZXID = new ZXID(0, 0) // will need to use to specify committed and non-committed proposals.
  val LAST_COMMITTED_ZXID = s"lastCommittedZXID${LocalState.DELIM}ZXID" // will need to use to specify committed and non-committed proposals.
  localState(LAST_COMMITTED_ZXID) = new ZXID(0, 0)

  //  private var history: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
  val HISTORY = s"history${LocalState.DELIM}ArrayBuffer[Proposal]"
  localState(HISTORY) = new ArrayBuffer[Proposal]()
  type HISTORY_TYPE = ArrayBuffer[Proposal]

  //  private var acceptedEpoch: Long = 0
  val ACCEPTED_EPOCH = s"acceptedEpoch${LocalState}Long"
  localState(ACCEPTED_EPOCH) = 0
  type EPOCH_TYPE = Long

  //  private var currentEpoch: Long = 0
  val CURRENT_EPOCH = s"currentEpoch${LocalState.DELIM}Long"
  localState(CURRENT_EPOCH) = 0

  //---------------------------------------------------------------
  // phase 0 values
  //---------------------------------------------------------------
  //  private var drained: Boolean = false
  val DRAINED = s"drained${LocalState}Boolean"
  localState(DRAINED) = false

  //  private var highestZXID: ZXID = _
  val HIGHEST_ZXID = s"highestZXID${LocalState.DELIM}ZXID"
  localState(HIGHEST_ZXID) = null

  //  private var possibleLeader: Agent = _
  val POSSIBLE_LEADER = s"possibleLeader${LocalState.DELIM}Agent"
  localState(POSSIBLE_LEADER) = null

  //  private var possibleLeaders: mutable.HashMap[Agent, Long] = new mutable.HashMap[Agent, Long]()
  val POSSIBLE_LEADERS = s"possibleLeaders${LocalState.DELIM}mutable.HashMap[Agent, Long]"
  localState(POSSIBLE_LEADERS) = mutable.HashMap[Agent, Long]()
  type POSSIBLE_LEADERS_TYPE = mutable.HashMap[Agent, Long]

  //---------------------------------------------------------------
  // phase 1 values
  //---------------------------------------------------------------
  //  private var highestEpoch: Long = 0 // acceptedEpoch
  val HIGHEST_EPOCH = s"highest${LocalState.DELIM}Long"
  localState(HIGHEST_EPOCH) = 0

  //  private var otherHighestEpoch: Long = 0 // currentEpoch
  val OTHER_HIGHEST_EPOCH = s"otherHighestEpoch${LocalState.DELIM}Long"
  localState(OTHER_HIGHEST_EPOCH) = 0

  //  private var otherHighestZXID: ZXID = new ZXID(0, 0)
  val OTHER_HIGHEST_ZXID = s"otherHighestZXID${LocalState.DELIM}ZXID"
  localState(OTHER_HIGHEST_ZXID) = new ZXID(0, 0)

  //---------------------------------------------------------------
  // phase 2&3 values
  //---------------------------------------------------------------
  //  private var holderProposal: Proposal = _
  val HOLDER_PROPOSAL = s"holderProposal${LocalState.DELIM}Proposal"
  localState(HOLDER_PROPOSAL) = null

  //  private var committedZXIDs: ArrayBuffer[ZXID] = new ArrayBuffer[ZXID]()
  val COMMITTED_ZXIDS = s"committedZXIDs${LocalState.DELIM}ArrayBuffer[ZXID]"
  localState(COMMITTED_ZXIDS) = ArrayBuffer[ZXID]()
  type ZXIDs_TYPE = ArrayBuffer[ZXID]

  //  private var proposedTransactions: mutable.HashMap[Proposal, ArrayBuffer[Agent]] = new mutable.HashMap[Proposal, ArrayBuffer[Agent]]()
  val PROPOSED_TRANSACTIONS = s"proposedTransactions${LocalState.DELIM}mutable.HashMap[Proposal,ArrayBuffer[Agent]]"
  localState(PROPOSED_TRANSACTIONS) = new mutable.HashMap[Proposal, ArrayBuffer[Agent]]()
  type PROPOSED_TRANSACTIONS_TYPE = mutable.HashMap[Proposal, ArrayBuffer[Agent]]

  initiateSystemCommunication(bootstrap)

  /* ===================================================================================
      methods used to deal with transactions of data
     =================================================================================== */

  //  private def ready(epoch: Long) {
  //        if (!this.ready) {
  //          this.ready = true
  //          // will start sending update messages to leader
  //          if (this.writeCommands.nonEmpty) {
  //            while (this.writeCommands.nonEmpty) {
  //              this.leaderRef ! Write(this.writeCommands.dequeue())
  //            }
  //          }
  //        }
  //      }

  private def ready(){ //epoch: Long) {
    if (!localState[Boolean](READY)) {
      localState(READY) = true
      // will start sending update messages to leader
      while (localState[WRITE_COMMANDS_TYPE](WRITE_COMMANDS).nonEmpty) {
        ds.send(this, Write(localState[WRITE_COMMANDS_TYPE](WRITE_COMMANDS).dequeue()), localState[Agent](LEADER_REF))
      }
    }
  }


  /**
   * method used to propose messages.
   * proposed messages are added to the history without updating
   * the location of the last committed proposal
   */
  //  private def propose(proposal: Proposal) { // won't add proposal to history if their is another value with larger zxid
  //    if (this.history.isEmpty || this.history.last.getTransaction.getZXID < proposal.getTransaction.getZXID) {
  //      this.history += proposal
  //      this.lastZXID = proposal.getTransaction.getZXID
  //    }
  //  }
  private def propose(proposal: Proposal) { // won't add proposal to history if their is another value with larger zxid
    if (localState[HISTORY_TYPE](HISTORY).isEmpty ||
      localState[HISTORY_TYPE](HISTORY).last.getTransaction.getZXID < proposal.getTransaction.getZXID) {
      localState(HISTORY) = localState[HISTORY_TYPE](HISTORY) :+ proposal
      localState(LAST_ZXID) = proposal.getTransaction.getZXID
    }
  }


  /**
   * method used to commit messages that have already
   * been proposed.
   * if the commit message is for a proposal that is
   * after another message that has been proposed
   * but not yet committed, then this will not commit
   * the current proposal but instead make note that it
   * has been approved for commitment once the program
   * gets to that proposal.
   */
  //  @tailrec
  //  private def commit(proposal: Proposal) {
  //    var i: Int = this.history.size - 1
  //    var break: Boolean = false
  //    var position: Int = -1
  //
  //    while (i >= 0 && !break && this.history(i).getTransaction.getZXID != this.lastCommittedZXID) {
  //
  //      if (this.history(i).getTransaction.getZXID == proposal.getTransaction.getZXID) {
  //        position = i
  //        break = true
  //      }
  //      i = i - 1
  //
  //    }
  //
  //    if (position == -1)
  //      return
  //    else if (position == 0)
  //      this.lastCommittedZXID = proposal.getTransaction.getZXID
  //    else if (this.history(position - 1).getTransaction.getZXID == this.lastCommittedZXID)
  //      this.lastCommittedZXID = proposal.getTransaction.getZXID
  //    else committedZXIDs += proposal.getTransaction.getZXID
  //
  //
  //    // checks to see if the next item in history was previously requested for commitment but couldn't
  //    // if so, runs commit on the next item as well.
  //    if (this.history.size - 1 > position &&
  //      this.committedZXIDs.contains(this.history(position + 1).getTransaction.getZXID)) {
  //
  //      this.committedZXIDs.remove(this.committedZXIDs.indexOf(this.history(position + 1).getTransaction.getZXID))
  //      this.commit(this.history(position + 1))
  //    }
  //  }
  @tailrec
  private def commit(proposal: Proposal) {
    //    var i: Int = this.history.size - 1
    var i: Int = localState[HISTORY_TYPE](HISTORY).size - 1
    var break: Boolean = false
    var position: Int = -1

    //    while (i >= 0 && !break && this.history(i).getTransaction.getZXID != this.lastCommittedZXID) {
    while (i >= 0 && !break &&
      localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID != localState[ZXID](LAST_COMMITTED_ZXID)) {
      if (localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID == proposal.getTransaction.getZXID) {
        position = i
        break = true
      }
      i = i - 1
    }

    if (position == -1)
      return
    else if (position == 0)
      localState(LAST_COMMITTED_ZXID) = proposal.getTransaction.getZXID
    else if (localState[HISTORY_TYPE](HISTORY)(position - 1).getTransaction.getZXID == localState[ZXID](LAST_COMMITTED_ZXID))
      localState(LAST_COMMITTED_ZXID) = proposal.getTransaction.getZXID
    else localState(COMMITTED_ZXIDS) = localState[ZXIDs_TYPE](COMMITTED_ZXIDS) :+ proposal.getTransaction.getZXID

    // checks to see if the next item in history was previously requested for commitment but couldn't
    // if so, runs commit on the next item as well.
    if (localState[HISTORY_TYPE](HISTORY).size - 1 > position &&
      localState[ZXIDs_TYPE](COMMITTED_ZXIDS).contains(localState[HISTORY_TYPE](HISTORY)(position + 1).getTransaction.getZXID)) {

      localState[ZXIDs_TYPE](COMMITTED_ZXIDS).remove(localState[ZXIDs_TYPE](COMMITTED_ZXIDS).indexOf(localState[HISTORY_TYPE](HISTORY)(position + 1).getTransaction.getZXID))
      this.commit(localState[HISTORY_TYPE](HISTORY)(position + 1))
    }
  }

  /**
   * handles all messages related to overall system maintenance
   */
  //  private def handleMaintenance(condition: Any, actor: ActorRef): Unit = {
  //    condition match {
  //      /*
  //       * checks to see which nodes in the system have
  //       * responded to the previous heartbeat in the
  //       * given time. Then will resend the heartbeat to current
  //       * members of the system and start a new countdown
  //       * to run this case.
  //       */
  //      case UpdateSystemConnection() =>
  //        val addedMembers: Set[ActorRef] = tempSystemList -- systemList
  //        val removedMembers: Set[ActorRef] = systemList -- tempSystemList
  //
  //        systemList = Set[ActorRef]() // updates the list of nodes in system
  //
  //        for (currentRef: ActorRef <- tempSystemList) {
  //          systemList += currentRef
  //        }
  //
  //        tempSystemList = Set[ActorRef]()
  //
  //        self ! SystemChanges(addedMembers, removedMembers)
  //
  //        for (current: ActorRef <- systemList) { // sends a heartbeat to each node in system
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
  //        if (!systemList.contains(sender) && !tempSystemList.contains(sender)) {
  //          tempSystemList = tempSystemList + sender
  //        }
  //        sender ! SystemMaintenance(ReceivedSystemInformation(systemList))
  //      /*
  //       * sent when sender wishes to know if a leader currently exists.
  //       * receiver will send a ref to that leader using CurrentLeader()
  //       * ref in response. A message will not be sent if the node
  //       * doesn't currently have a leader.
  //       */
  //      case GetLeader() =>
  //        if (leaderRef != null) {
  //          sender ! CurrentLeader(leaderRef)
  //        }
  //      /*
  //       * Sent when sender is responding to previous GetSystemInformation.
  //       * receiver will first acknowledge the senders response by adding them
  //       * to the tempSystemList. Then, if their are any new members in the senders
  //       * list that aren't in the receivers, will send them a heartbeat.
  //       */
  //      case ReceivedSystemInformation(members: Set[ActorRef]) =>
  //        tempSystemList = tempSystemList + sender
  //        val newMembers: Set[ActorRef] = members -- (tempSystemList ++ systemList)
  //
  //        for (current: ActorRef <- newMembers) { // sends a heartbeat to each node in system
  //          current ! SystemMaintenance(GetSystemInformation())
  //        }
  //      /*
  //       * sent when sender wishes to know the highest transaction
  //       * done by receiver. Receiver responds by sending it's HighestZxid value
  //       */
  //      case GetHighestZXID() =>
  //        sender ! HighestZXID(lastZXID)
  //      case _ =>
  //    }
  //  }
  private def handleMaintenance(condition: Any): Unit = { //, actor: Agent): Unit = {
    condition match {
      /*
       * checks to see which nodes in the system have
       * responded to the previous heartbeat in the
       * given time. Then will resend the heartbeat to current
       * members of the system and start a new countdown
       * to run this case.
       */
      case UpdateSystemConnection() =>
        val addedMembers: Set[Agent] = localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST) -- localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)
        val removedMembers: Set[Agent] = localState[SYSTEM_LIST_TYPE](SYSTEM_LIST) -- localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST)

        localState(SYSTEM_LIST) = Set[Agent]() // updates the list of nodes in system

        for (currentRef: Agent <- localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST)) {
          localState(SYSTEM_LIST) = localState[SYSTEM_LIST_TYPE](SYSTEM_LIST) + currentRef
        }

        localState(TEMP_SYSTEM_LIST) = Set[Agent]()

        //        self ! SystemChanges(addedMembers, removedMembers)
        ds.send(this, SystemChanges(addedMembers, removedMembers), this)

        for (current: Agent <- localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)) { // sends a heartbeat to each node in system
          //          current ! SystemMaintenance(GetSystemInformation())
          ds.send(this, SystemMaintenance(GetSystemInformation()), current)
        }

        // waits awhile until ready to check again
        //        context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, SystemMaintenance(UpdateSystemConnection()))
        ds.send(this, SystemMaintenance(UpdateSystemConnection()), this)
      /*
       * received when sender is looking for a heartbeat or first
       * joining the system. receiver will send its list
       * of nodes in system as a reply using ReceivedSystemInformation
       * If receiver hasn't seen this node in the system before,
       * receiver adds them to their temp list.
       */
      case m: GetSystemInformation =>
        //println("responding to heartbeat from " + sender.path.toString())
        if (!localState[SYSTEM_LIST_TYPE](SYSTEM_LIST).contains(m.sender) && !localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST).contains(m.sender)) {
          localState(TEMP_SYSTEM_LIST) = localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST) + m.sender
        }
        ds.send(this, SystemMaintenance(ReceivedSystemInformation(localState[SYSTEM_LIST_TYPE](SYSTEM_LIST))), m.sender)
      /*
       * sent when sender wishes to know if a leader currently exists.
       * receiver will send a ref to that leader using CurrentLeader()
       * ref in response. A message will not be sent if the node
       * doesn't currently have a leader.
       */
      case m: GetLeader =>
        if (localState[Agent](LEADER_REF) != null) {
          ds.send(this, CurrentLeader(localState[Agent](LEADER_REF)), m.sender)
        }
      /*
       * Sent when sender is responding to previous GetSystemInformation.
       * receiver will first acknowledge the senders response by adding them
       * to the tempSystemList. Then, if their are any new members in the senders
       * list that aren't in the receivers, will send them a heartbeat.
       */
      case m@ReceivedSystemInformation(members: Set[Agent]) =>
        localState(TEMP_SYSTEM_LIST) = localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST) + m.sender
        val newMembers: Set[Agent] = members -- (localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST) ++ localState[SYSTEM_LIST_TYPE](SYSTEM_LIST))

        for (current: Agent <- newMembers) { // sends a heartbeat to each node in system
          ds.send(this, SystemMaintenance(GetSystemInformation()), current)
        }
      /*
       * sent when sender wishes to know the highest transaction
       * done by receiver. Receiver responds by sending it's HighestZxid value
       */
      case m: GetHighestZXID =>
        ds.send(this, HighestZXID(localState[ZXID](LAST_ZXID)), m.sender)
      case _ =>
    }
  }


  /**
   * handles all method related calls from the ZAB_Impl
   * all of these cases will be ? rather than ! so they
   * must return something.
   *
   */
  //  private def methodResponse(method: Any): Any = {
  //    method match {
  //      case SubmitState(value: Any) =>
  //        if (this.ready) this.leaderRef ! Write(value)
  //        else this.writeCommands += value
  //      case GetHistory() => this.history
  //      case GetCommittedHistory() =>
  //        var i: Int = 0
  //        var committedHistory: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
  //        while (i < this.history.size) {
  //          if (this.history(i).getTransaction.getZXID > this.lastCommittedZXID) {
  //            return committedHistory
  //          }
  //          committedHistory += this.history(i)
  //          i = i + 1
  //        }
  //        committedHistory
  //      case GetProposedHistory() =>
  //        var i: Int = this.history.size - 1
  //        var proposedHistory: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
  //        while (i >= 0) {
  //          if (!(this.history(i).getTransaction.getZXID > this.lastCommittedZXID)) {
  //            return proposedHistory.reverse
  //          }
  //          proposedHistory += this.history(i)
  //          i = i - 1
  //        }
  //        proposedHistory.reverse
  //      case GetSystemNodes() => systemList
  //      case GetFollowerNodes() =>
  //        if (this.leader) this.followerList.keySet.toSet
  //        else new ArrayBuffer[ActorRef]().toSet
  //      case GetLeaderRef() => this.leaderRef
  //      case GetCurrentState() =>
  //        var i: Int = this.history.size - 1
  //        while (i >= 0) {
  //          if (this.history(i).getTransaction.getZXID == this.lastCommittedZXID)
  //            return this.history(i)
  //          else i = i - 1
  //        } // while
  //      case ForceReElection() => initiatePhase0()
  //      case _ =>
  //    }
  //  }
  private def methodResponse(method: Any): Message = {
    method match {
      //-----------------------------------------------------------------------
      case SubmitState(value: Any) =>
        if (localState[Boolean](READY)) ds.send(this, Write(value), localState[Agent](LEADER_REF))
        else localState(WRITE_COMMANDS) = localState[WRITE_COMMANDS_TYPE](WRITE_COMMANDS) :+ value

        new Message("")
      //-----------------------------------------------------------------------

      case GetHistory() => new Message("", Seq(localState[HISTORY_TYPE](HISTORY)))

      //-----------------------------------------------------------------------
      case GetCommittedHistory() =>
        var i: Int = 0
        var committedHistory: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
        while (i < localState[HISTORY_TYPE](HISTORY).size) {
          if (localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID > localState[ZXID](LAST_COMMITTED_ZXID)) {

            return new Message("",Seq(committedHistory))

          }
          committedHistory += localState[HISTORY_TYPE](HISTORY)(i)
          i = i + 1
        }

        new Message("", Seq(committedHistory))

      //-----------------------------------------------------------------------
      case GetProposedHistory() =>
        var i: Int = localState[HISTORY_TYPE](HISTORY).size - 1
        var proposedHistory: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
        while (i >= 0) {
          if (!(localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID > localState[ZXID](LAST_COMMITTED_ZXID))) {

            return new Message ("",Seq(proposedHistory.reverse))

          }
          proposedHistory += localState[HISTORY_TYPE](HISTORY)(i)
          i = i - 1
        }

        new Message("", Seq(proposedHistory.reverse))

      //-----------------------------------------------------------------------

      case GetSystemNodes() => new Message("", Seq(localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)))

      //-----------------------------------------------------------------------

      case GetFollowerNodes() =>

        if (localState[Boolean](LEADER)) new Message("", Seq(localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).keySet.toSet))

        else new Message("", Seq(new ArrayBuffer[ActorRef]().toSet))

      //-----------------------------------------------------------------------

      case GetLeaderRef() => new Message("", Seq(localState[Agent](LEADER_REF)))

      //-----------------------------------------------------------------------
      case GetCurrentState() =>
        var i: Int = localState[HISTORY_TYPE](HISTORY).size - 1
        while (i >= 0) {
          if (localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID == localState[ZXID](LAST_COMMITTED_ZXID))

            return new Message("",Seq(localState[HISTORY_TYPE](HISTORY)(i)))

          else i = i - 1
        } // while

        new Message("")

      //-----------------------------------------------------------------------
      case ForceReElection() => initiatePhase0()

      new Message("")

      //-----------------------------------------------------------------------
      case _ => new Message("")
    }
  }


  /**
   * deals with messages related to the heart beats
   * used in the Zab protocol
   *
   */
  private def heartBeatDealer(method: Any, sender: Agent): Unit = {
    method match {
      case m: LeaderHeartbeat =>
        if (localState[Boolean](LEADER)) ds.send(this, ZabHeartbeat(LeaderBoopidiboop()), m.sender)
      case LeaderBoopidiboop() => localState(LEADER_HEART) = true
      case LeaderBeaterChecker() =>
        if (localState[Boolean](LEADER_HEART)) {
          localState(LEADER_HEART) = false
          if (localState[Agent](LEADER_REF) != null) {
            ds.send(this, ZabHeartbeat(LeaderHeartbeat()), localState[Agent](LEADER_REF))
            ds.send(this, ZabHeartbeat(LeaderBeaterChecker()), this)
          }
        }
        else initiatePhase0()
      case m: FollowerHeartbeat => ds.send(this, ZabHeartbeat(FollowerBoopidiboop()), m.sender)
      case m: FollowerBoopidiboop =>
        if (localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).contains(sender)) localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)(m.sender) = true
      case FollowerBeaterChecker(follower: Agent) =>
        if (localState[Boolean](LEADER) && localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).contains(follower)) {
          if (localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)(follower)) {
            localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)(follower) = false
            ds.send(this, ZabHeartbeat(FollowerHeartbeat()), follower)
            ds.send(this, ZabHeartbeat(FollowerBeaterChecker(follower)), this)
          }
          else ds.send(this, FollowerLost(follower), this)
        }
    }
  }

  /* ===================================================================================
                     ******* Phase Initiation Functions *******
     =================================================================================== */

  //    private def initiateSystemCommunication(bootstrap: URL): Unit = {
  //      if (bootstrap != null) {
  //        val address: String = "akka.tcp://ChordSystem" + bootstrap.getPort + "@" + bootstrap.getHost + ":" + bootstrap.getPort + "/user/nodeImpl"
  //        val selection: ActorSelection = context.actorSelection(address)
  //        selection ! SystemMaintenance(GetSystemInformation())
  //      }
  //      self ! SystemMaintenance(GetSystemInformation())
  //
  //      context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, InitialSetupComplete())
  //    }

  private def initiateSystemCommunication(bootstrap: String): Unit = {
    if (bootstrap != null) {
      val address: String = bootstrap
      val selection: Agent = ds.get(address)
      ds.send(this, SystemMaintenance(GetSystemInformation()), selection)
    }
    ds.send(this, SystemMaintenance(GetSystemInformation()), this)
    ds.send(this, InitialSetupComplete(), this)
  }


  //  private def initiatePhase0(): Unit = {
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
  //    for (currentNode: ActorRef <- systemList) {
  //      currentNode ! SystemMaintenance(GetLeader())
  //      currentNode ! SystemMaintenance(GetHighestZXID())
  //    }
  //
  //    context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, Phase0Timer())
  //
  //  }
  private def initiatePhase0(): Unit = {
    //    println("Initiating phase 0")

    localState(LEADER) = true
    localState(LEADER_REF) = null
    localState(HIGHEST_ZXID) = localState[ZXID](LAST_ZXID)
    localState(POSSIBLE_LEADER) = this
    localState(POSSIBLE_LEADERS) = new mutable.HashMap[Agent, Long]()

    ds.become(this, "phase0")

    localState(DRAINED) = false
    ds.send(this, Drained(), this)

    for (currentNode: Agent <- localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)) {
      ds.send(this, SystemMaintenance(GetLeader()), currentNode)
      ds.send(this, SystemMaintenance(GetHighestZXID()), currentNode)
    }
    ds.send(this, Phase0Timer(), this)
  }

  private def initiatePhase1(): Unit = {
    //    println("Initiating phase 1")

    localState(HIGHEST_EPOCH) = localState[Long](ACCEPTED_EPOCH)
    localState(FOLLOWER_LIST) = new mutable.HashMap[Agent, Boolean]()
    localState(FOLLOWER_ZXID) = new mutable.HashMap[Agent, ZXID]()
    localState(LEADER_HEART) = false
    localState(RECEIVED_QUORUM) = false
    localState(UPDATED_HISTORY) = new ArrayBuffer[Proposal]()

    ds.become(this, "phase1")

    if (localState[Boolean](LEADER)) {
      ds.send(this, Phase1Timer(), this)
    }
    else { //Not necessary, if leader times out, follower will need to go back to phase 0.
      ds.send(this, Phase1Timer(), this)
    }
    ds.send(this, FollowerInfo(localState[Long](ACCEPTED_EPOCH), localState[ZXID](LAST_ZXID)), localState[Agent](LEADER_REF))
  }

  private def initiatePhase2(): Unit = {
    //    println("Initiating phase 2")

    localState(HOLDER_PROPOSAL) = new Proposal(0, new Transaction("", new ZXID(0, 0)))
    localState(COMMITTED_ZXIDS) = new ArrayBuffer[ZXID]()
    localState(PROPOSED_TRANSACTIONS) = new mutable.HashMap[Proposal, ArrayBuffer[Agent]]()
    localState(PROPOSED_TRANSACTIONS) = localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS) + (localState[Proposal](HOLDER_PROPOSAL) -> new ArrayBuffer[Agent]())

    ds.become(this, "phase2")

    if (localState[Boolean](LEADER)) {
      for (currentRef: Agent <- localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).keysIterator) {
        var i: Int = 0
        var switch: Boolean = false
        val missingKnowledge: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()

        /**
         * runs through each element in the complete history until it finds
         * the last element that the current zab node had. After that, it
         * adds each proposal after that to a new list which will be sent to
         * that specific node. This is done for each follower of the leader.
         */
        while (i < localState[HISTORY_TYPE](HISTORY).size) {
          if (switch) {
            missingKnowledge += localState[HISTORY_TYPE](HISTORY)(i)
          }
          else if (localState[HISTORY_TYPE](HISTORY)(i).getTransaction.getZXID > localState[ZXID](LAST_ZXID)) {
            switch = true
            missingKnowledge += localState[HISTORY_TYPE](HISTORY)(i)
          }
          i = i + 1
        }
        ds.send(this, NewLeader(localState[Long](ACCEPTED_EPOCH), missingKnowledge), currentRef)
      }
    }
  }

  private def initiatePhase3(): Unit = {
    //    println("Initiating phase 3")

    localState(PROPOSED_TRANSACTIONS) = new mutable.HashMap[Proposal, ArrayBuffer[Agent]]()
    localState(COUNTER) = 0

    ds.become(this, "phase3")

//    this.ready(localState[Long](CURRENT_EPOCH))
    this.ready()
  }

  /* =====================================================================================
  ////////////////////////////////////////////////////////////////////////////////////////
                             ******* Phase Match Cases *******
  ////////////////////////////////////////////////////////////////////////////////////////
  ======================================================================================== */


  ////////////////////////////////////////////////////////////////////////////////////////
  // Leader Election
  ////////////////////////////////////////////////////////////////////////////////////////
  //  def phase0: Receive = {
  behaviors += "phase0" -> new Behavior("phase0")
  // --------------------------------------------------------------------------------------
  //  SystemMaintenance(condition: Any)
  // case SystemMaintenance(condition) =>
  // --------------------------------------------------------------------------------------
  private val systemMaintenanceAction_phase0 = new Action
  systemMaintenanceAction_phase0 + Statement { (m: Message, _: Agent) => this.handleMaintenance(m.payload[Any](0), m.sender) }

  // --------------------------------------------------------------------------------------
  //  MethodCalls(method: Any)
  //  case MethodCalls(method) =>
  // --------------------------------------------------------------------------------------
  private val methodCallsAction_phase0 = new Action
  methodCallsAction_phase0 + Statement { (m: Message, a: Agent) =>
    a.ds.send(a, methodResponse(m.payload[Any](0)), m.sender)
  }

  // --------------------------------------------------------------------------------------
  //  CurrentLeader(actor: Agent)
  //  case CurrentLeader(actor: ActorRef) =>
  // --------------------------------------------------------------------------------------
  private val currentLeaderAction_phase0 = new Action + Statement { (m: Message, a: Agent) =>
    //println("current leader from " + sender.path.toString())
    val mm = m.asInstanceOf[CurrentLeader]
    val actor = mm.actor
    val possibleLeaders = a.localState[POSSIBLE_LEADERS_TYPE](POSSIBLE_LEADERS) // it mutates, doesn't change

    if (a.localState[Boolean](DRAINED)) {
      if (actor != null && possibleLeaders.contains(actor))
        possibleLeaders(actor) = possibleLeaders(actor) + 1
      else possibleLeaders += (actor -> 1)
    }
  } // Statement

  // --------------------------------------------------------------------------------------
  //  case HighestZXID(zxid: ZXID)
  //  =>
  // --------------------------------------------------------------------------------------
  private val highestZXIDAction_phase0 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[HighestZXID]
    val drained = a.localState[Boolean](DRAINED)
    val zxid = mm.zxid
    val sender = mm.sender

    //println("received highest zxid from " + sender.path.toString())
    if (drained) {
      if (a.localState[ZXID](HIGHEST_ZXID) == null) {
        a.localState(HIGHEST_ZXID) = zxid
      }
      else {
        if (a.localState[ZXID](HIGHEST_ZXID) < zxid) {
          a.localState(HIGHEST_ZXID) = zxid
          a.localState(POSSIBLE_LEADER) = sender
        }
      }
    }
  }


  // --------------------------------------------------------------------------------------
  //  case Drained() =>
  // --------------------------------------------------------------------------------------
  private val drainedAction_phase0 = new Action + Statement { (_: Message, a: Agent) => a.localState(DRAINED) = true }


  // --------------------------------------------------------------------------------------
  //any position cases
  //  case SystemChanges(added: Set[ActorRef], removed: Set[ActorRef])
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemChangesAction_phase0 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[SystemChanges]
    val drained = a.localState[Boolean](DRAINED)
    val added = mm.added
    val removed = mm.removed
    val possibleLeaders = a.localState[POSSIBLE_LEADERS_TYPE](POSSIBLE_LEADERS)

    if (drained) {
      for (currentNode: Agent <- added) {
        ds.send(a, GetLeader(), currentNode)
        ds.send(a, GetHighestZXID(), currentNode)
      }
      for (currentNode: Agent <- removed) {
        if (possibleLeaders.contains(currentNode)) {
          possibleLeaders.remove(currentNode)
        }
      }
    }
  }


  // --------------------------------------------------------------------------------------
  //  case Phase0Timer()
  //  =>
  // --------------------------------------------------------------------------------------
  private val phase0TimerAction_phase0 = new Action + Statement { (_: Message, a: Agent) =>
    val possibleLeaders = a.localState[POSSIBLE_LEADERS_TYPE](POSSIBLE_LEADERS)
    val possibleLeader = a.localState[Agent](POSSIBLE_LEADER)

    if (possibleLeaders.nonEmpty) {
      var nextLeader: Agent = null
      var memberSize: Long = 0
      for ((currentNode: Agent, size: Long) <- possibleLeaders.iterator) {
        if (memberSize < size) {
          nextLeader = currentNode
          memberSize = size
        }
      }
      a.localState(LEADER_REF) = nextLeader
    }
    else if (possibleLeader != null) {
      a.localState(LEADER_REF) = possibleLeader
    }

    a.localState(LEADER) = a.localState[Agent](LEADER_REF).equals(a)
    initiatePhase1()
  }


  // --------------------------------------------------------------------------------------
  // possible phase 1 message
  //  case FollowerInfo(e: Long, lastZXID: ZXID)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerInfoAction_phase0 = new Action + Statement { (m: Message, a: Agent) =>
    // places back in mailbox just as it was
    // until ready to look at in phase 1.
    //    self.tell(FollowerInfo(e, lastZXID), sender)
    val mm = m.asInstanceOf[FollowerInfo]
    val e = mm.e
    val lastZXID = mm.lastZXID

    a.ds.send(a, FollowerInfo(e, lastZXID), mm.sender)
  }


  // --------------------------------------------------------------------------------------
  //  case _ =>
  // --------------------------------------------------------------------------------------

  ////////////////////////////////////////////////////////////////////////////////////////
  // Discovery
  ////////////////////////////////////////////////////////////////////////////////////////
  //def phase1: Receive = {


  behaviors += "discovery" -> new Behavior("discovery")
  // --------------------------------------------------------------------------------------
  //  case SystemMaintenance(condition)
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemMaintenanceAction_discovery = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[SystemMaintenance]
    val condition = mm.condition
    val sender = mm.sender
    handleMaintenance(condition, sender)
  }


  // --------------------------------------------------------------------------------------
  //  case MethodCalls(method)
  //  =>
  // --------------------------------------------------------------------------------------
  private val methodCalls_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[MethodCalls]
    val sender = mm.sender
    val method = mm.method
    a.ds.send(a, methodResponse(method), sender)
  }


  // --------------------------------------------------------------------------------------
  //  case ZabHeartbeat(method)
  //  =>
  // --------------------------------------------------------------------------------------
  private val zabHeartbeat_discovery = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[ZabHeartbeat]
    val method = mm.method
    val sender = mm.sender
    heartBeatDealer(method, sender)
  }


  // --------------------------------------------------------------------------------------
  //leader cases
  //  case FollowerInfo(e: Long, notUsedZXID: ZXID)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerInfoAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[FollowerInfo]
    val sender = mm.sender
    val e = mm.e
    //    val highestEpoch = a.localState[Long](HIGHEST_EPOCH) // it changes so we access it from a.localState all the time
    //    val receivedQuorum = a.localState[Boolean](RECEIVED_QUORUM) // same as highestEpoch, it changes
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val leader = a.localState[Boolean](LEADER)
    val self = a

    if (leader) {
      if (!a.localState[Boolean](RECEIVED_QUORUM) && e > a.localState[Long](HIGHEST_EPOCH)) {
        a.localState(HIGHEST_EPOCH) = e
      }
      if (!followerList.contains(sender)) {
        a.localState(FOLLOWER_LIST) = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST) + (sender -> true)
        a.ds.send(a, ZabHeartbeat(FollowerBeaterChecker(sender)), self)
      }
      if (a.localState[Boolean](RECEIVED_QUORUM)) {
        a.ds.send(a, NewEpoch(a.localState[Long](HIGHEST_EPOCH)), sender)
      }
      else {
        if (a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).size > (a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST).size / 2)) {
          a.localState(RECEIVED_QUORUM) = true
          a.localState(HIGHEST_EPOCH) = a.localState[Long](HIGHEST_EPOCH) + 1
          for (currentFollower: Agent <- a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).keys) {
            a.ds.send(a, NewEpoch(a.localState[Long](HIGHEST_EPOCH)), currentFollower)
          }
          a.localState(OTHER_HIGHEST_EPOCH) = a.localState[Long](CURRENT_EPOCH)
          a.localState(OTHER_HIGHEST_ZXID) = a.localState[ZXID](LAST_ZXID)
          a.localState(UPDATED_HISTORY) = a.localState[HISTORY_TYPE](HISTORY)
        }
      }
    }
    else {
      a.ds.send(a, NotLeader(), sender)
    }
  }


  // --------------------------------------------------------------------------------------
  //  case AckEpoch(currentEpoch: Long, history: ArrayBuffer[Proposal], lastZXID: ZXID)
  //  =>
  // --------------------------------------------------------------------------------------
  private val ackEpochAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[AckEpoch]
    val sender = mm.sender
    val currentEpoch = mm.currentEpoch
    val history = mm.history
    val lastZXID = mm.lastZXID
    val leader = a.localState[Boolean](LEADER)
    val receivedQuorum = a.localState[Boolean](RECEIVED_QUORUM)

    if (leader && receivedQuorum) {
      if (!a.localState[FOLLOWER_ZXID_TYPE](FOLLOWER_ZXID).contains(sender) && a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).contains(sender)) {
        a.localState(FOLLOWER_ZXID) = a.localState[FOLLOWER_ZXID_TYPE](FOLLOWER_ZXID) + (sender -> lastZXID)
        if (a.localState[Long](OTHER_HIGHEST_EPOCH) < currentEpoch ||
          (a.localState[Long](OTHER_HIGHEST_EPOCH) == currentEpoch && a.localState[ZXID](OTHER_HIGHEST_ZXID) < lastZXID)) {
          a.localState(OTHER_HIGHEST_EPOCH) = currentEpoch
          a.localState(OTHER_HIGHEST_ZXID) = lastZXID
          a.localState(UPDATED_HISTORY) = history
        }
      }
      if (a.localState[FOLLOWER_ZXID_TYPE](FOLLOWER_ZXID).size >= a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).size) {
        localState(HISTORY) = a.localState[HISTORY_TYPE](UPDATED_HISTORY)
        initiatePhase2()
      }
    }
  }


  // --------------------------------------------------------------------------------------
  //  case FollowerLost(follower: ActorRef)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerLostAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[FollowerLost]
    val follower = mm.removed
    val leader = a.localState[Boolean](LEADER)
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST) // mutates only
    val followerZXID = a.localState[FOLLOWER_ZXID_TYPE](FOLLOWER_ZXID) // mutates only
    val receivedQuorum = a.localState[Boolean](RECEIVED_QUORUM)
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)

    if (leader) {
      if (followerList.contains(follower))
        followerList.remove(follower)

      if (followerZXID.contains(follower))
        followerZXID.remove(follower)

      if (receivedQuorum)
        if (followerList.size <= (systemList.size / 2))
          initiatePhase0()

      if (followerZXID.size >= followerList.size) {
        a.localState(HISTORY) = a.localState[HISTORY_TYPE](UPDATED_HISTORY)
        initiatePhase2()
      }
    }
  }


  // --------------------------------------------------------------------------------------
  //follower cases
  //  case NewEpoch(e: Long)
  //  =>
  // --------------------------------------------------------------------------------------
  private val newEpochAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[NewEpoch]
    val sender = mm.sender
    val e = mm.e
    val self = a

    if (sender == a.localState[Agent](LEADER_REF)) {
      if (e >= a.localState[Long](ACCEPTED_EPOCH)) { // different from setup but allows one to force a node into re-election without crashing.
        a.localState(ACCEPTED_EPOCH) = e
        a.localState(LEADER_HEART) = true
        a.ds.send(a, ZabHeartbeat(LeaderBeaterChecker()), self)
        a.ds.send(a, AckEpoch(a.localState[Long](CURRENT_EPOCH),
          a.localState[HISTORY_TYPE](HISTORY), a.localState[ZXID](LAST_ZXID)),
          a.localState[Agent](LEADER_REF))

        if (!a.localState[Boolean](LEADER)) initiatePhase2()
      }
      else initiatePhase0()
    }
  }


  // --------------------------------------------------------------------------------------
  //  case NotLeader()
  //  =>
  // --------------------------------------------------------------------------------------
  private val notLeaderAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    if (m.sender == a.localState[Agent](LEADER_REF)) initiatePhase0()
  }

  // --------------------------------------------------------------------------------------
  //any position cases
  //  case SystemChanges(added: Set[ActorRef], removed: Set[ActorRef])
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemChangesAction_discovery = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[SystemChanges]
    val removed = mm.removed
    val leader = a.localState[Boolean](LEADER)
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)
    val followerZXID = a.localState[FOLLOWER_ZXID_TYPE](FOLLOWER_ZXID)
    val leaderRef = a.localState[Agent](LEADER_REF)

    if (leader) {
      if (a.localState[Boolean](RECEIVED_QUORUM) && followerList.size <= (systemList.size / 2))
        initiatePhase0()
      else if (!a.localState[Boolean](RECEIVED_QUORUM) && followerList.size > (systemList.size / 2)) {
        a.localState(RECEIVED_QUORUM) = true
        a.localState(HIGHEST_EPOCH) = a.localState[Long](HIGHEST_EPOCH) + 1
        for (currentFollower: Agent <- followerList.keys) {
          a.ds.send(a, NewEpoch(a.localState[Long](HIGHEST_EPOCH)), currentFollower)
        }
        a.localState(OTHER_HIGHEST_EPOCH) = a.localState[Long](CURRENT_EPOCH)
        a.localState(OTHER_HIGHEST_ZXID) = a.localState[ZXID](LAST_ZXID)
        a.localState(UPDATED_HISTORY) = a.localState[HISTORY_TYPE](HISTORY)
      } else if (a.localState[Boolean](RECEIVED_QUORUM) && followerZXID.size >= followerList.size) {
        a.localState(HISTORY) = a.localState[HISTORY_TYPE](UPDATED_HISTORY)
        initiatePhase2()
      }
    } else if (removed.contains(leaderRef))
      initiatePhase0()
  }

  // --------------------------------------------------------------------------------------
  //  case Phase1Timer()
  //  =>
  // --------------------------------------------------------------------------------------
  private val phase1TimerAction_discovery = new Action + Statement { (_: Message, _: Agent) => initiatePhase0() }

  // --------------------------------------------------------------------------------------
  //  case _ =>
  // --------------------------------------------------------------------------------------


  ////////////////////////////////////////////////////////////////////////////////////////
  // Synchronization
  ////////////////////////////////////////////////////////////////////////////////////////
  //  def phase2: Receive = {
  behaviors += "phase2" -> new Behavior("phase2")
  // --------------------------------------------------------------------------------------
  //    case SystemMaintenance(condition) =>
  // --------------------------------------------------------------------------------------
  private val systemMaintenanceAction_phase2 = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[SystemMaintenance]
    val condition = mm.condition
    val sender = mm.sender
    handleMaintenance(condition, sender)
  }

  // --------------------------------------------------------------------------------------
  //    case MethodCalls(method) =>
  // --------------------------------------------------------------------------------------
  private val methodCallsAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[MethodCalls]
    val method = mm.method
    val sender = mm.sender
    a.ds.send(a, methodResponse(method), sender)
  }

  // --------------------------------------------------------------------------------------
  //    case ZabHeartbeat(method) =>
  // --------------------------------------------------------------------------------------
  private val zabHeartbeatAction_phase2 = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[ZabHeartbeat]
    val method = mm.method
    val sender = mm.sender
    heartBeatDealer(method, sender)
  }

  // --------------------------------------------------------------------------------------
  //    //leader cases
  //    case AckNewLeader() =>
  // --------------------------------------------------------------------------------------
  private val ackNewLeaderAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[AckNewLeader]
    val leader = a.localState[Boolean](LEADER)
    val sender = mm.sender
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS)
    val holderProposal = a.localState[Proposal](HOLDER_PROPOSAL)
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)

    if (leader) {
      val acceptedTransactionList: ArrayBuffer[Agent] = proposedTransactions.apply(holderProposal)
      if (!acceptedTransactionList.contains(sender)) {
        acceptedTransactionList += sender
        if (acceptedTransactionList.size > (followerList.size / 2)) {
          for (currentFollower: Agent <- followerList.keysIterator) {
            a.ds.send(a, Commit(0L), currentFollower)
          }
          initiatePhase3()
        }
      }
    }
  }

  // --------------------------------------------------------------------------------------
  //  case FollowerLost(follower: ActorRef)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerLostAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[FollowerLost]
    val leader = a.localState[Boolean](LEADER)
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)
    val follower = mm.removed
    val holderProposal = a.localState[Proposal](HOLDER_PROPOSAL)
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS)

    if (leader) {
      if (followerList.contains(follower))
        followerList.remove(follower)
      if (followerList.size <= (systemList.size / 2))
        initiatePhase0()
      if (proposedTransactions.apply(holderProposal).size > (followerList.size / 2)) {
        for (currentFollower: Agent <- followerList.keysIterator) {
          a.ds.send(a, Commit(Unit), currentFollower)
        }
        initiatePhase3()
      }
    }
  }

  // --------------------------------------------------------------------------------------
  //follower cases
  //  case NewLeader(e: Long, updateHistory: ArrayBuffer[Proposal])
  //  =>
  // --------------------------------------------------------------------------------------
  private val newLeaderAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[NewLeader]
    val sender = mm.sender
    val e = mm.e
    val updateHistory = mm.history
    val leaderRef = a.localState[Agent](LEADER_REF)
    val acceptedEpoch = a.localState[Long](ACCEPTED_EPOCH)

    if (sender == leaderRef) {
      if (e != acceptedEpoch) {
        initiatePhase0()
      }
      else {
        a.localState(CURRENT_EPOCH) = e
        for (currentProposal: Proposal <- updateHistory) {
          val currentTransaction: Transaction = currentProposal.getTransaction
          val newProposal: Proposal = new Proposal(e, currentTransaction)
          propose(newProposal)
        }
        a.ds.send(a, AckNewLeader(), sender)
      }
    }
  }


  // --------------------------------------------------------------------------------------
  //  case Commit(value: Long)
  //  =>
  // --------------------------------------------------------------------------------------
  private val commitAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[Commit]
    val sender = mm.sender
    val leaderRef = a.localState[Agent](LEADER_REF)
    val history = a.localState[HISTORY_TYPE](HISTORY)
    val lastCommittedZXID = a.localState[ZXID](LAST_COMMITTED_ZXID)
    val leader = a.localState[Boolean](LEADER)

    if (sender == leaderRef) {
      var switch: Boolean = false
      var i: Int = 0
      while (i < history.size) {
        if (switch) commit(history(i))
        else if (history(i).getTransaction.getZXID > lastCommittedZXID) {
          switch = true
          commit(history(i))
        }

        i = i + 1
      }
      if (!leader) initiatePhase3()
    }
  }

  // --------------------------------------------------------------------------------------
  //any position cases
  //  case SystemChanges(added: Set[ActorRef], removed: Set[ActorRef])
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemChangesAction_phase2 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[SystemChanges]
    val removed = mm.removed
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS)
    val holderProposal = a.localState[Proposal](HOLDER_PROPOSAL)
    val leader = a.localState[Boolean](LEADER)
    val leaderRef = a.localState[Agent](LEADER_REF)

    if (leader) {
      for (currentMember: Agent <- removed) {
        if (followerList.contains(currentMember))
          followerList.remove(currentMember)
        if (followerList.size <= (systemList.size / 2))
          initiatePhase0()
        if (proposedTransactions.apply(holderProposal).size > (followerList.size / 2)) {
          for (currentFollower: Agent <- followerList.keysIterator) {
            a.ds.send(a, Commit(0L), currentFollower)
          }
          initiatePhase3()
        }
      }
    }
    else if (removed.contains(leaderRef))
      initiatePhase0()
  }

  // --------------------------------------------------------------------------------------
  //  case _ =>
  // --------------------------------------------------------------------------------------


  ////////////////////////////////////////////////////////////////////////////////////////
  // Broadcast
  ////////////////////////////////////////////////////////////////////////////////////////
  //def phase3: Receive = {
  behaviors += "phase3" -> new Behavior("phase3")
  // --------------------------------------------------------------------------------------
  //  case SystemMaintenance(condition)
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemMaintenanceAction_phase3 = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[SystemMaintenance]
    val sender = mm.sender
    val condition = mm.condition
    handleMaintenance(condition, sender)
  }

  // --------------------------------------------------------------------------------------
  //  case MethodCalls(method)
  //  =>
  // --------------------------------------------------------------------------------------
  private val methodCallsAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[MethodCalls]
    val sender = mm.sender
    val method = mm.method

    a.ds.send(a, methodResponse(method), sender)
  }

  // --------------------------------------------------------------------------------------
  //  case ZabHeartbeat(method)
  //  =>
  // --------------------------------------------------------------------------------------
  private val zabHeartbeatAction_phase3 = new Action + Statement { (m: Message, _: Agent) =>
    val mm = m.asInstanceOf[ZabHeartbeat]
    val method = mm.method
    val sender = mm.sender
    heartBeatDealer(method, sender)
  }

  // --------------------------------------------------------------------------------------
  // leader cases
  //  case Write(message: Any)
  //  =>
  // --------------------------------------------------------------------------------------
  private val writeAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[Write]
    val leader = a.localState[Boolean](LEADER)
    val message = mm.message
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)

    if (leader) {
      a.localState(COUNTER) = a.localState[Long](COUNTER) + 1
      val nextZXID: ZXID = new ZXID(a.localState[Long](CURRENT_EPOCH), a.localState[Long](COUNTER))
      val nextTransaction: Transaction = new Transaction(message, nextZXID)
      val nextProposal: Proposal = new Proposal(a.localState[Long](CURRENT_EPOCH), nextTransaction)
      a.localState(PROPOSED_TRANSACTIONS) = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS) + (nextProposal -> new ArrayBuffer[ActorRef]())
      for (currentFollower: Agent <- followerList.keysIterator) {
        a.ds.send(a, TakeProposal(nextProposal), currentFollower)
      }
    }
  }

  // --------------------------------------------------------------------------------------
  //  case Ack(proposal: Proposal)
  //  =>
  // --------------------------------------------------------------------------------------
  private val ackAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[Ack]
    val sender = mm.sender
    val proposal = mm.proposal
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS) // only mutates
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val leader = a.localState[Boolean](LEADER)

    if (leader) {
      if (proposedTransactions.contains(proposal)) {
        val currentList: ArrayBuffer[Agent] = proposedTransactions.apply(proposal)
        if (!currentList.contains(sender)) {
          currentList += sender
          if (currentList.size > (followerList.size / 2)) {
            for (currentFollower: Agent <- followerList.keysIterator) {
              a.ds.send(a, Commit(proposal), currentFollower)
            }
            proposedTransactions.remove(proposal)
          }
        }
      }
    }
  }

  // --------------------------------------------------------------------------------------
  //  case FollowerInfo(e: Long, lastZXID: ZXID)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerInfoAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[FollowerInfo]
    val sender = mm.sender
    val lastZXID = mm.lastZXID
    val leader = a.localState[Boolean](LEADER)
    val history = a.localState[HISTORY_TYPE](HISTORY)
    val currentEpoch = a.localState[Long](CURRENT_EPOCH)

    if (leader) {
      val missingKnowledge: ArrayBuffer[Proposal] = new ArrayBuffer[Proposal]()
      var switch: Boolean = false
      var i: Int = 0

      /**
       * runs through each element in the complete history until it finds
       * the last element that the current zab node had. After that, it
       * adds each proposal after that to a new list which will be sent to
       * that specific node. This is done for each follower of the leader.
       */
      while (i < history.size) {
        if (switch) missingKnowledge += history(i)
        else if (history(i).getTransaction.getZXID > lastZXID) {
          switch = true
          missingKnowledge += history(i)
        }

        i = i + 1
      }
      a.ds.send(a, NewEpoch(currentEpoch), sender)
      a.ds.send(a, NewLeader(currentEpoch, missingKnowledge), sender)
    }
    else {
      a.ds.send(a, NotLeader(), sender)
    }
  }

  // --------------------------------------------------------------------------------------
  //  case AckNewLeader()
  //  =>
  // --------------------------------------------------------------------------------------
  private val ackNewLeaderAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[AckNewLeader]
    val sender = mm.sender
    val leader = a.localState[Boolean](LEADER)

    if (leader) {
      if (!a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST).contains(sender)) {
        a.ds.send(a, Commit(0L), sender)
        a.localState(FOLLOWER_LIST) = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST) + (sender -> true)
        a.ds.send(a, ZabHeartbeat(FollowerBeaterChecker(sender)), a)
      }
      a.ds.send(a, Commit(0L), sender)
    }
  }

  // --------------------------------------------------------------------------------------
  //  case FollowerLost(follower: ActorRef)
  //  =>
  // --------------------------------------------------------------------------------------
  private val followerLostAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[FollowerLost]
    val leader = a.localState[Boolean](LEADER)
    val follower = mm.removed
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS) // only mutates
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)

    if (leader && followerList.contains(follower)) {
      followerList.remove(follower)
      if (followerList.size <= (systemList.size / 2))
        initiatePhase0()

      for (proposedProposal: Proposal <- proposedTransactions.keys) {
        val currentList: ArrayBuffer[Agent] = proposedTransactions.apply(proposedProposal)
        if (currentList.contains(follower)) {
          currentList.remove(currentList.indexOf(follower))
        }
        else {
          if (currentList.size > (followerList.size / 2)) {
            for (currentFollower: Agent <- followerList.keysIterator) {
              a.ds.send(a, Commit(proposedProposal), currentFollower)
            }
            proposedTransactions.remove(proposedProposal)
          }
        }
      }
    }
  }

  // --------------------------------------------------------------------------------------
  // follower cases
  //  case TakeProposal(proposal: Proposal)
  //  =>
  // --------------------------------------------------------------------------------------
  private val takeProposalAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[TakeProposal]
    val sender = mm.sender
    val proposal = mm.proposal
    val leaderRef = a.localState[Agent](LEADER_REF)

    if (sender == leaderRef) {
      this.propose(proposal)
      a.ds.send(a, Ack(proposal), sender)
    }
  }

  // --------------------------------------------------------------------------------------
  //  case Commit(proposal: Proposal)
  //  =>
  // --------------------------------------------------------------------------------------
  private val commitAction_phase3 = new Action + Statement { (m: Message, _: Agent) =>
    val proposal = m.asInstanceOf[Commit].transaction.asInstanceOf[Proposal]
    this.commit(proposal)
  }

  // --------------------------------------------------------------------------------------
  //any position cases
  //  case SystemChanges(added: Set[ActorRef], removed: Set[ActorRef])
  //  =>
  // --------------------------------------------------------------------------------------
  private val systemChangesAction_phase3 = new Action + Statement { (m: Message, a: Agent) =>
    val mm = m.asInstanceOf[SystemChanges]
    val removed = mm.removed
    val leader = a.localState[Boolean](LEADER)
    val leaderRef = a.localState[Agent](LEADER_REF)
    val followerList = a.localState[FOLLOWER_LIST_TYPE](FOLLOWER_LIST)
    val systemList = a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)
    val proposedTransactions = a.localState[PROPOSED_TRANSACTIONS_TYPE](PROPOSED_TRANSACTIONS) // only mutates

    if (leader) {
      for (currentMember: Agent <- removed) {
        if (followerList.contains(currentMember)) {
          followerList.remove(currentMember)
          if (followerList.size <= (systemList.size / 2))
            initiatePhase0()

          for (proposedProposal: Proposal <- proposedTransactions.keys) {
            val currentList: ArrayBuffer[Agent] = proposedTransactions.apply(proposedProposal)
            if (currentList.contains(currentMember))
              currentList.remove(currentList.indexOf(currentMember))
            else {
              if (currentList.size > (followerList.size / 2)) {
                for (currentFollower: Agent <- followerList.keysIterator) {
                  a.ds.send(a, Commit(proposedProposal), currentFollower)
                }
                proposedTransactions.remove(proposedProposal)
              }
            }
          }
        }
        else if (followerList.size <= (systemList.size / 2)) initiatePhase0()
      }
    }
    else if (removed.contains(leaderRef)) initiatePhase0()
  }

  // --------------------------------------------------------------------------------------
  //  case _ =>
  // --------------------------------------------------------------------------------------


  ////////////////////////////////////////////////////////////////////////////////////////
  // Initial System Connection
  ////////////////////////////////////////////////////////////////////////////////////////
  //  def receive: Receive = {
  // --------------------------------------------------------------------------------------
  // defaultBehavior
  // --------------------------------------------------------------------------------------
  //    case SystemMaintenance(condition) =>
  // --------------------------------------------------------------------------------------
  private val systemMaintenance_default = new Action + Statement{(m:Message, _:Agent) =>
    val mm = m.asInstanceOf[SystemMaintenance]
    val sender = mm.sender
    val condition = mm.condition
    this.handleMaintenance(condition, sender)
  }

  // --------------------------------------------------------------------------------------
  //    case MethodCalls(method) =>
  // --------------------------------------------------------------------------------------
  private val methodCalls_default = new Action + Statement{(m:Message, a:Agent)=>
    val mm = m.asInstanceOf[MethodCalls]
    val sender = mm.sender
    val method = mm.method

    a.ds.send(a, methodResponse(method),sender )
  }

  // --------------------------------------------------------------------------------------
  //    case InitialSetupComplete() =>
  // --------------------------------------------------------------------------------------
  private val initialSetupCompleteAction_default = new Action + Statement { (_: Message, a: Agent) =>
    a.localState(SYSTEM_LIST) = a.localState[SYSTEM_LIST_TYPE](TEMP_SYSTEM_LIST) // updates the list of nodes in system
    a.localState(TEMP_SYSTEM_LIST) = Set[Agent]()

    for (current: Agent <- a.localState[SYSTEM_LIST_TYPE](SYSTEM_LIST)) { // sends a heartbeat to each node in system
      a.ds.send(a, SystemMaintenance(GetSystemInformation()), current)
    }

    // waits awhile until ready to check again
//    context.system.scheduler.scheduleOnce(Duration.create(1000, TimeUnit.MILLISECONDS), self, SystemMaintenance(UpdateSystemConnection()))
    a.ds.send(a,SystemMaintenance(UpdateSystemConnection()),a)

    initiatePhase0()
  }

  // --------------------------------------------------------------------------------------
  //    case _ =>
  // --------------------------------------------------------------------------------------


  // --------------------------------------------------------------------------------------
  //                                construction Behaviors
  // --------------------------------------------------------------------------------------
  behaviors("phase0") += new Message("SystemMaintenance") -> systemMaintenanceAction_phase0
  behaviors("phase0") += new Message("MethodCalls") -> methodCallsAction_phase0
  behaviors("phase0") += new Message("CurrentLeader") -> currentLeaderAction_phase0
  behaviors("phase0") += new Message("HighestZXID") -> highestZXIDAction_phase0
  behaviors("phase0") += new Message("Drained") -> drainedAction_phase0
  behaviors("phase0") += new Message("SystemChanges") -> systemChangesAction_phase0
  behaviors("phase0") += new Message("Phase0Timer") -> phase0TimerAction_phase0
  behaviors("phase0") += new Message("FollowerInfo") -> followerInfoAction_phase0
  behaviors("phase0") += new Message("") -> new Action // empty block executed for the message whose name is empty string
  behaviors("discovery") += new Message("SystemMaintenance") -> systemMaintenanceAction_discovery
  behaviors("discovery") += new Message("MethodCalls") -> methodCalls_discovery
  behaviors("discovery") += new Message("ZabHeartbeat") -> zabHeartbeat_discovery
  behaviors("discovery") += new Message("FollowerInfo") -> followerInfoAction_discovery
  behaviors("discovery") += new Message("AckEpoch") -> ackEpochAction_discovery
  behaviors("discovery") += new Message("FollowerLost") -> followerLostAction_discovery
  behaviors("discovery") += new Message("NewEpoch") -> newEpochAction_discovery
  behaviors("discovery") += new Message("NotLeader") -> notLeaderAction_discovery
  behaviors("discovery") += new Message("SystemChanges") -> systemChangesAction_discovery
  behaviors("discovery") += new Message("Phase1Timer") -> phase1TimerAction_discovery
  behaviors("discovery") += new Message("") -> new Action // empty block executed for the message whose name is empty string
  behaviors("phase2") += new Message("SystemMaintenance") -> systemMaintenanceAction_phase2
  behaviors("phase2") += new Message("MethodCalls") -> methodCallsAction_phase2
  behaviors("phase2") += new Message("ZabHeartbeat") -> zabHeartbeatAction_phase2
  behaviors("phase2") += new Message("AckNewLeader") -> ackNewLeaderAction_phase2
  behaviors("phase2") += new Message("FollowerLost") -> followerLostAction_phase2
  behaviors("phase2") += new Message("NewLeader") -> newLeaderAction_phase2
  behaviors("phase2") += new Message("Commit") -> commitAction_phase2
  behaviors("phase2") += new Message("SystemChanges") -> systemChangesAction_phase2
  behaviors("phase2") += new Message("") -> new Action // empty block executed for the message whose name is empty string
  behaviors("phase3") += new Message("SystemMaintenance") -> systemMaintenanceAction_phase3
  behaviors("phase3") += new Message("SystemMaintenance") -> methodCallsAction_phase3
  behaviors("phase3") += new Message("ZabHeartbeat") -> zabHeartbeatAction_phase3
  behaviors("phase3") += new Message("Write") -> writeAction_phase3
  behaviors("phase3") += new Message("Ack") -> ackAction_phase3
  behaviors("phase3") += new Message("FollowerInfo") -> followerInfoAction_phase3
  behaviors("phase3") += new Message("AckNewLeader") -> ackNewLeaderAction_phase3
  behaviors("phase3") += new Message("FollowerLost") -> followerLostAction_phase3
  behaviors("phase3") += new Message("TakeProposal") -> takeProposalAction_phase3
  behaviors("phase3") += new Message("Commit") -> commitAction_phase3
  behaviors("phase3") += new Message("SystemChanges") -> systemChangesAction_phase3
  behaviors("phase3") += new Message("") -> new Action // empty block executed for the message whose name is empty string
  defaultBehavior += new Message("SystemMaintenance") -> systemMaintenance_default
  defaultBehavior += new Message("MethodCalls") -> methodCalls_default
  defaultBehavior += new Message("InitialSetupComplete") -> initialSetupCompleteAction_default
  defaultBehavior += new Message("") -> new Action // empty block executed for the message whose name is empty string

  reactions = defaultBehavior // initially

  specialReactions(new Start) + Statement{(m:Message, Agent) =>
    // TODO Start action (src: https://distributedalgorithm.wordpress.com/2015/06/20/architecture-of-zab-zookeeper-atomic-broadcast-protocol/)
    /*
    - Start with electing a leader - phase0 (by sending a message that start
    that, running it to completion).
    - Only leader accepts modification requests, all (incl. leader) accept read requests...
    However, requests can be submitted to any node and they will forward it to leader.
    - After the election, the discovery phase (phase1) and subsequent ones should start on their own.
    - no need for "synchronization" (phase2) at the start.
     */

  }

}