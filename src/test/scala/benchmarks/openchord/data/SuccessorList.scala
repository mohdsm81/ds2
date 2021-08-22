package benchmarks.openchord.data


import scala.util.control.Breaks._


/**
 * this class represents a list of successors of a node.
 */
class SuccessorList(val ln: Node, val cap: Int = 50) {
  private val localNode = ln
  private var successors: List[Node] = List()
  private val capacity = cap
  
  def copy: SuccessorList = {
    val newOne = new SuccessorList( ln, cap )
    newOne.successors = successors.map{identity}
    newOne
  }

  def getCopy: List[Node] = successors

  def addSuccessor(toAdd: Node) {
    if (toAdd == null)
      throw new NullPointerException("Parameter may not be null!")

    if (successors.size >= capacity && !toAdd.getID.isInInterval(localNode.getID, successors.last.getID))
      return

    if (!contains(toAdd) && !toAdd.equals(localNode)) {
      var inserted = false
      breakable {
        for (i <- successors.indices) {
          if (toAdd.getID.isInInterval(localNode.getID, successors(i).getID)) {
            println("mark:" + toAdd.getURL)
            successors = (successors.dropRight(successors.length - i) :+ toAdd) ::: successors.drop(i)
            inserted = true
            break
          }
        }
      }
      if (!inserted)
        successors = successors :+ toAdd
    }

    if (successors.size > capacity)
      successors = successors.dropRight(1)
  }


  def removeSuccessor(toRemove: Node) {
    if (toRemove == null)
      throw new NullPointerException("Parameter may not be null!")

    breakable {
      for (i <- successors.indices) {
        if (toRemove.equals(successors(i))) {
          successors = successors.dropRight(successors.length - i) ::: successors.drop(i + 1)
          break
        }
      }
    }

  }


  def getClosestPrecedingNode(id: ID): Node = {
    var cn: Node = null
    breakable {
      for (i <- successors.indices) {
        if (!id.isInInterval(localNode.getID, successors(successors.length - 1 - i).getID)) {
          cn = successors(successors.length - 1 - i)
          break
        }
      }
    }
    cn
  }

  def getImmediateSuccessor(id: ID): Node = {
    var im: Node = null
    breakable {
      for (i <- successors.indices) {
        if (id.isInInterval(localNode.getID, successors(i).getID)) {
          im = successors(i)
          break
        }
      }
    }

    im
  }


  def contains(n: Node): Boolean = {
    var c = false
    if (n.equals(localNode))
      return true
    successors.foreach { x => if (x.equals(n)) c = true }
    c
  }


  def getSuccessor: Node = if (successors.isEmpty) null else successors.head

  def getLast: Node = if (successors.isEmpty) null else successors.last

  def getRandomNode: Node = {
    if (successors.nonEmpty) {

      val r = new scala.util.Random(System.currentTimeMillis())
      val num = r.nextInt(successors.size)
      this.successors(num)
    }
    else
      null
  }
}