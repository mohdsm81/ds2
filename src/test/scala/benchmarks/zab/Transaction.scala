package benchmarks.zab

/***************************************************************************
 *                                                                         *
 *                           Transaction.scala                             *
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

@SerialVersionUID(112L)
class Transaction(private val state : Any, private val zxid : ZXID) extends Ordered[Transaction] with Serializable{
  
  // grants access to the state and zxid values within the current transaction
  def getState: Any = state
  def getZXID: ZXID = zxid
  
  override def toString: String = "<" + state.toString + ", " + zxid.toString() + ">"
  
  override def hashCode() : Int = {
    var stateHashCode : Int = 0
    var zxidHashCode : Int = 0
    if(state != null || zxid != null){
      stateHashCode = state.hashCode()
      zxidHashCode = zxid.hashCode()
    }
    17 * stateHashCode + zxidHashCode
  }
  
  override def equals(that : Any) : Boolean = {
    that match {
      case _: Transaction => this.hashCode() == that.hashCode()
      case _ => false
    }
  }
  
  // overrides Ordered implementation for compare
  override def compare(other : Transaction) : Int = {
    zxid.compare(other.getZXID)
  }
}