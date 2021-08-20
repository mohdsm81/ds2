package benchmarks.paxos

import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent

/**
 * A Multi-Paxos instance is identified by a triple: the client issuing the request, the key of that request, and the
 * operation type
 *
 * @param client    the client issuing the WRITE/READ request
 *
 * @param key       the key to READ/WRITE
 *
 * @param operation the type of operation this instance represents: READ/WRITE
 *
 */
case class Instance( client    : Agent,
                     key       : String,
                     id        : Int,
                     operation : OperationType.Value,
                     quorumSize: Int ) {
  private var proposalNum: Int = 0
  
  def getProposalNum: Int = proposalNum
  
  /**
   * This equality doesn't include the ProposalID.num, it only compares the identifying factors of a transaction
   * (a.k.a. instance in Multi-Paxos). For correctly and precisely comparing ProposalID's, use the other three methods.
   *
   * In brief: this exists so that instances of this class can be used as a "key" in maps.
   */
  override def equals( obj: Any ): Boolean = {
    obj match {
      case x: Instance =>
        x.client.name == client.name &&
          x.key == key &&
          x.operation == operation
      case _ => false
    }
  }
  
  def maxProposal(that: Instance): Int = Math.max(proposalNum,that.proposalNum)
  
  def nextProposalNumber( higherProposalNumber: Int = 0 ): Int = {
    proposalNum = higherProposalNumber + 1
    proposalNum
  }
  
  def resetProposalNumber( ): Unit = proposalNum = 0
  
  def copy: Instance = {
    val newOne = Instance( client, key, id, operation, quorumSize )
    newOne.proposalNum = proposalNum
    newOne
  }
  
  def > ( that: Instance ): Boolean = proposalNum > that.proposalNum
  
  def >= ( that: Instance ): Boolean = proposalNum >= that.proposalNum
  
  def < ( that: Instance ): Boolean = proposalNum < that.proposalNum
  
}
