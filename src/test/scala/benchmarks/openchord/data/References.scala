package benchmarks.openchord.data



import java.util.logging.Logger
import java.util.logging.Level

class References(val ln:Node, val ft:FingerTable, val sl:SuccessorList, val pre:Node) {
  
  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
  private var localNode = ln
  private var fingerTable = ft
  private var successorList = sl
  private var predecessor = pre
  private val lock = new Object()

  def copy: References = new References(ln, fingerTable.copy,successorList.copy,pre)
  
  def getClosestPrecedingNode(id:ID):Node = {
    
    if(id==null){
       logger.log(Level.SEVERE, "references null doesn't have closest preceding node",  new NullPointerException()) 
      return null
    }
    lock.synchronized{      
      var cn:Node = null
      var cnFT = fingerTable.getClosestPrecedingNode(id)
      var cnSL = successorList.getClosestPrecedingNode(id)
      var extra:Node = if(predecessor !=null && id.isInInterval(predecessor.getID, localNode.getID)) predecessor else null
      
      cn = cnFT
      if(cn == null || (cnSL != null && cn.getID.compare(cnSL.getID) < 0))
        cn = cnSL
        
      if(cn == null || (extra != null && cn.getID.compare(extra.getID) < 0))
        cn = extra         
      cn
    } 
  }
  
  
  def addReference(newReference:Node){      
    if (newReference == null) 
      logger.log(Level.SEVERE, "references to add to fingerTable should not be null",  
          new NullPointerException())
    else{
      this.lock.synchronized{      
        this.updatePredecessor(newReference)      
        fingerTable.addReference(newReference)
        successorList.addSuccessor(newReference)
      }   
    }
  }
  
  
  def removeReference(toRemove:Node){
    if (toRemove == null){ 
      logger.log(Level.SEVERE, "references to add to fingerTable should not be null",  
          new NullPointerException())      
       return 
    }
    this.lock.synchronized{   
      this.successorList.removeSuccessor(toRemove)
      this.fingerTable.removeReference(toRemove)
      
      if(this.predecessor!=null && toRemove.equals(this.predecessor))
        this.predecessor = null 
    }        
  }
  
  def setPredecessor(node:Node){
    this.predecessor = node    
  }
  
  def updatePredecessor(node:Node){
    if(node == null){
      logger.log(Level.SEVERE, "node should not be null",  
          new NullPointerException())   
       return      
    }
    this.lock.synchronized{
      if(this.predecessor == null||this.predecessor.equals(localNode) || node.getID.isInInterval(this.predecessor.getID, localNode.getID))
        this.setPredecessor(node)  
    }  
  }
  
  
  
  def contains(node:Node):Boolean = this.successorList.contains(node) || this.fingerTable.contains(node)
  
  def getFingerTable:FingerTable = this.lock.synchronized{ this.fingerTable } 
  
  def getSuccessorList = this.lock.synchronized{this.successorList}
  
  def getSuccessor:Node = this.lock.synchronized{this.successorList.getSuccessor}
  
  def getPredecessor:Node = this.lock.synchronized{this.predecessor}
  
  
  
  
  
  
   
}