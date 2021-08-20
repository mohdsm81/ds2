package benchmarks.paxos

/*
 * proposalID instance to represent a unique propose among all paxoses
 * 
 * @author Zepeng Zhao
 * modified by Mohammed S. Al-Mahfoudh for better re-use
 */
case class ProposalID(id:Int, private var num:Int = 0) extends Serializable{
  
  def nextProposalNum(higherPid: ProposalID = null): Int = {
    if(higherPid == null) num +=1
    else num = higherPid.num + 1
    num
  }
  
  def resetProposalNum: Unit = num = 0
  
  def isGreaterThan(other:ProposalID):Boolean = num > other.num || (num == other.num && id > other.id)
  
  def isSmallerThan(other:ProposalID):Boolean = num < other.num || (num == other.num && id < other.id)

  def isEqualTo(other:ProposalID):Boolean = num == other.num && id == other.id
  
  override def equals( obj: Any ): Boolean = this.isEqualTo(obj.asInstanceOf[ProposalID])

  override def toString:String = "[number:" + num +", pid:" + id +"]"
  
  def copy: ProposalID = ProposalID(id,num)
}