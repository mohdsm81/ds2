package benchmarks.paxos

case class Record( key: String, var value: Any, private val agentsCount: Int ) {
  private var countAccepted    : Int         = 0
  private var countPromises    : Int         = 0
  // prepareAcks are either Nack or a Promise
  private var prepareAcksCount : Int         = 0
  private var countNacks       : Int         = 0
  private var accumulatedValues: List[ Any ] = List()
  private val quorumSize       : Int         = agentsCount / 2 + 1
  
  def reachedQuorumAcceptance: Boolean = countAccepted >= quorumSize
  
  def reachedQuorumPromises: Boolean = countPromises >= quorumSize
  
  def reachedPrepareAcksLimit: Boolean = prepareAcksCount == agentsCount
  
  def reachedNackMajority: Boolean = countNacks >= quorumSize
  
  def nack( ): Boolean = {
    prepareAcksCount += 1
    countNacks += 1
    true
  }
  
  def accept( value: Any ): Boolean = {
    accumulateValues( value )
    if( value == this.value ) {
      countAccepted += 1
      true
    }
    else false
  }
  
  def accumulateValues( value: Any ): Unit = {
    accumulatedValues = accumulatedValues :+ value
  }
  
  def reachedMajorityValues: Boolean = {
    val maxEntry = accumulatedValues.groupBy( identity ).toList.reduce { ( x, y ) =>
      if( x._2.size >
        y._2.size ) x else y
    }
    maxEntry._2.size >= quorumSize
  }
  
  def getMajorityValue: Any = {
    val maxEntry = accumulatedValues.groupBy( identity ).toList.reduce { ( x, y ) =>
      if( x._2.size >
        y._2.size ) x else y
    }
    maxEntry._1
  }
  
  def promise( value: Any ): Boolean = {
    prepareAcksCount += 1
    if( value == this.value ) {
      countPromises += 1
      true
    }
    else false
  }
  
  def copy: Record = {
    val newOne = Record( key, value, agentsCount )
    newOne.countAccepted = countAccepted
    newOne.countPromises = countPromises
    newOne.prepareAcksCount = prepareAcksCount
    newOne.countNacks = countNacks
    newOne.accumulatedValues = accumulatedValues
    newOne
  }
  
  def resetCopy: Record = {
    val newOne = copy
    newOne.countAccepted = 0
    newOne.countPromises = 0
    newOne.prepareAcksCount = 0
    newOne.countNacks = 0
    newOne.accumulatedValues = List[ Any ]()
    newOne
  }
}
