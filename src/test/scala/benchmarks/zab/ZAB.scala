package benchmarks.zab

/***************************************************************************
 *                                                                         *
 *                           ZAB.scala                                     *
 *                            -------------------                          *
 *   date                 : 9.30.2015                                      *
 *   email                : heath.french@utah.edu                          *
 *                                                                         *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   A copy of the license can be found in the license.txt file supplied   *
 *   with this software or at: http://www.gnu.org/copyleft/gpl.html        *
 *                                                                         *
 ***************************************************************************/

import scala.collection.mutable.ArrayBuffer

trait ZAB {
  /**
   * creates new node that is in its own ZAB system
   */
  def createNode(local : URL) : Unit

  /**
   * creates a new node that joins a pre-existing ZAB system
   */
  def joinNode(local : URL, bootstrap : URL) : Unit

  /**
   * attempts to submit a message to the entire
   * ZAB system. Will only be sent once the system
   * is ready to receive new messages.
   */
  def submitState(message : Any) : Unit

  /**
   * gets the Proposal history from the current
   * node. Will be empty if no transactions have
   * taken place.
   */
  def getHistory: ArrayBuffer[Proposal]

  /**
   * gets only the portion of the history that has
   * been successfully proposed and committed to
   * the history
   */
  def getCommittedHistory: ArrayBuffer[Proposal]

  /**
   * gets the portion of the history that has been
   * proposed but not yet committed.
   */
  def getProposedHistory: ArrayBuffer[Proposal]

  /**
   * returns a set of strings showing what other
   * nodes are currently in the system
   */
  def getSystemNodes: Set[String]

  /**
   * returns a set of strings showing all
   * nodes that are following this node.
   * Will be empty if not leader.
   */
  def getFollowerNodes: Set[String]

  /**
   * returns a string the represents
   * the location of the leader
   * if one exists.
   */
  def getLeaderString: String

  /**
   * returns the latest transaction submitted by the current node.
   */
  def getCurrentState: Proposal

  /**
   * forces the current node to go back to the
   * re-election phase. May cause entire system
   * to recover if the current node was the leader.
    */
  def forceReElection() : Unit

  /**
   * forces the current node to crash entirely.
   * The user will need to re-enter the system again.
   */
  def forceCrash() : Unit
}