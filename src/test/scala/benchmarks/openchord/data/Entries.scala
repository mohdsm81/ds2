package benchmarks.openchord.data

/**
 * This class represents the entries that a node store. data is store in a map which maps the key(id) to values.
 * @author zepeng zhao
 */

class Entries {
  
  private var entries:Map[ID, Set[Entry]] = Map()
  
  //set to true if this object is changed, otherwise stay false
  private var status = false
  
  def copy: Entries = {
    val newOne = new Entries
    newOne.entries = entries.map{case (k,v) => k -> v.map{identity}}
    newOne.status = status
    newOne
  }
  
  def addAll(toAdds:Set[Entry]){
    if(toAdds == null){
      throw new NullPointerException(
          "Set of entries to be added to the local hash table may not be null!")     
    }    
    toAdds.foreach(x=>this.add(x))    
  }
  
   
  def add(entryToAdd:Entry){
    if (entryToAdd == null) {
      throw new NullPointerException("Entry to add may not be null!")
    }   
    this.entries.synchronized{
      try{
        entries = entries++Map(entryToAdd.id -> (entries(entryToAdd.id)++Set(entryToAdd) ) )
      } 
      catch{
        case e:NoSuchElementException=>entries = entries++Map(entryToAdd.id -> Set(entryToAdd ) )
      }
      this.status = true;
    }   
  }
  
  
  def removeAll(entriesToRemove:Set[Entry]){
    if(entriesToRemove == null){
      throw new NullPointerException("Set of entries may not have value null!")
    }    
    entriesToRemove.foreach { x => remove(x) }
  }
  
  
  def remove(entryToRemove:Entry){
    if(entryToRemove == null){
      throw new NullPointerException("Entry to remove may not be null!");
    }
    this.entries.synchronized{
      if(entries.contains(entryToRemove.id)){
        var temp:Set[Entry] = entries(entryToRemove.id)--Set(entryToRemove)
        entries = entries++( if(temp.size>0) Map(entryToRemove.id->temp) else Map() )
        this.status = true;
      }     
    }    
  }
  
  def removeID(toRm:ID):Set[Entry] ={
    this.entries.synchronized{
      var backup = this.getEntries(toRm)
      this.entries = this.entries.filterKeys(!_.equals(toRm))
      this.status = true
      backup  
    }
  }
  
  
  
  
  def getEntries(id:ID):Set[Entry]={
    if(id == null){
      throw new NullPointerException("ID to find entries for may not be null!");
    }
    var result:Set[Entry] = Set()
    this.entries.synchronized{
      if(entries.contains(id))
        result = result++entries(id) 
    }
    return result   
  }
  
  def getKeys:Set[ID] = entries.keySet
  
  def getValues:List[Set[Entry]] = entries.valuesIterator.toList 
  
  
  def getEntriesInInterval(fromID:ID, toID:ID):Set[Entry]={
    if(fromID == null || toID == null){
      throw new NullPointerException(
          "Neither of the given IDs may have value null!")
    }
    var result:Set[Entry] = Set()
    for((k,v) <- entries){
      if(k.isInInterval(fromID, toID))
        result = result++v
    }
    return result
  }
 
  def getNumberOfStoredEntries():Int = this.entries.size
  
  def resetStatus(){
    this.status = !this.status
  }
  
  def getStatus:Boolean = this.status
  
  
}