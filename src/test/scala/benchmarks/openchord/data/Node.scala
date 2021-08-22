package benchmarks.openchord.data




class Node(val id:ID,val url:String) extends Serializable{
  
  protected var nodeID:ID = id;
  
  protected var nodeURL:String = url;
  
  def getID:ID = nodeID
  
  def getURL:String = nodeURL
  
  
  def equals(other:Node):Boolean = {
      if(other == null)
        false
      else 
        nodeID.equals(other.id)
    }
}