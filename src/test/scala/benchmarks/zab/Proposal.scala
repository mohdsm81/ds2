package benchmarks.zab

/** *************************************************************************
 * *
 * Proposal.scala                                *
 * -------------------                          *
 * date                 : 9.30.2015                                      *
 * email                : heath.french@utah.edu                          *
 * *
 * *
 * ************************************************************************* */

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

@SerialVersionUID(113L)
class Proposal(private val epoch: Long, private val transaction: Transaction) extends Ordered[Proposal] with Serializable {

  // grants access to the state and zxid values within the current transaction
  def getEpoch: Long = epoch

  def getTransaction: Transaction = transaction

  override def toString: String = "<<" + epoch + ">, " + transaction.toString() + ">"

  override def hashCode(): Int = {
    var epochHashCode: Int = 0
    var transactionHashCode: Int = 0
    //    if(epoch != null || transaction != null){
    epochHashCode = epoch.hashCode()
    transactionHashCode = transaction.hashCode()
    //    }
    17 * epochHashCode + transactionHashCode
  }

  override def equals(that: Any): Boolean = {
    that match {
      case _: Proposal => this.hashCode() == that.hashCode()
      case _ => false
    }
  }

  // overrides Ordered implementation for compare
  override def compare(other: Proposal): Int = {
    transaction.compare(other.getTransaction)
  }
}