package benchmarks.zab

/** *************************************************************************
 * *
 * ZXID.scala                                    *
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

@SerialVersionUID(111L)
class ZXID(private val epoch: Long, private val counter: Long) extends Ordered[ZXID] with Serializable {

  // grants access to the epoch or counter of the current zxid
  def getEpoch: Long = epoch

  def getCounter: Long = counter

  // returns new zxid based off of the required update
  def getNextEpochZXID: ZXID = {
    new ZXID(epoch + 1, 0)
  }

  def getNextCounterZXID: ZXID = {
    new ZXID(epoch, counter + 1)
  }

  override def toString: String = {
    val result: String = "<" + epoch + ", " + counter + ">"
    result
  }

  override def hashCode(): Int = {
    var epochHashCode: Int = 0
    var counterHashCode: Int = 0
    //    if(epoch != null || counter != null){
    epochHashCode = epoch.hashCode()
    counterHashCode = counter.hashCode()
    //    }
    17 * epochHashCode + counterHashCode
  }

  override def equals(that: Any): Boolean = {
    that match {
      case _: ZXID => this.hashCode() == that.hashCode()
      case _ => false
    }
  }

  // overrides compare method for Ordered
  override def compare(other: ZXID): Int = {
    if (this.getEpoch == other.getEpoch)
      this.counter.compare(other.getCounter)
    else
      this.epoch.compare(other.getEpoch)
  }
}