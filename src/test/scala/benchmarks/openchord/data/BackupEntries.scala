package benchmarks.openchord.data

import java.util.logging.Logger


/**
 * object uses to back up entries for other nodes in case that when the other nodes fail, the entries it store won't
 * be lost.in this implementation, a node only back up entries for its predecessor. In fact, a node can back up entries
 * for all the other nodes in its reference table for further security.
 * @author zepeng zhao
 */
class BackupEntries {
  private var entries:Map[Node,List[Set[Entry]]] = Map()
  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)

  def getEntries(node:Node):List[Set[Entry]] = {
    var re:List[Set[Entry]] = null
    this.entries.synchronized{
      entries.keySet.foreach { x => if(x.equals(node)) re = entries(x) }
    }
    re
  }
  
  /**
   * set back up entries of a node.
   */
  def setEntries(node:Node, entr:List[Set[Entry]]) ={ 
    var re:Node = null
    logger.info("backing up for Node["+node.getID+"]")
    this.entries.synchronized{
      entries.keySet.foreach { x => if(node.equals(x)) re = x }
      if(re != null) entries.filterKeys(!_.equals(re))
      entries = entries++Map(node->entr)
    }
    entr.foreach { x => x.foreach { y => logger.info(y.key+":"+y.value)} }
  }
  
  /**
   * remove the backup entries for node and return the backup entries of this node
   */
  def remove(node:Node):List[Set[Entry]] = {
    var re:Node = null
    var backup:List[Set[Entry]] = null
    this.entries.synchronized{
      entries.keySet.foreach { x => if(node.equals(x)) re = x }
      if(re != null){ 
        backup = this.entries(re).toList
        entries = entries.filterKeys(!_.equals(re))
      }
    }
    //backup.foreach { x => x.foreach { y => logger.info(y.getKey()+":"+y.getValue())} }
    backup
  }
  
}