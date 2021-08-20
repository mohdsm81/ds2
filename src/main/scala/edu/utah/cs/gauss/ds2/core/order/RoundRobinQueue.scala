package edu.utah.cs.gauss.ds2.core.order

/**
 * @author Mohammed S. Al-Mahfoudh
 * 		   mahfoudh@cs.utah.edu
 * 		   Gauss Group - SoC
 * 		   The University of Utah
 *  This is the Round Robin trait.
 */
trait RoundRobinQueue[T]{
  protected var items: Seq[T] = Seq() // has to have at least one element, otherwise the round-robin is undefined

  protected var circularQ: Iterator[T] = _
  protected var current: T = _

  def getCurrent: T = current
  def getNext: T = {
    current = circularQ.next()
    current
  }
  def getSize: Int = items.size

  def setItems(xs: Seq[T]): Unit ={
    items = xs
    current = _:T
    circularQ = Iterator.continually(items).flatten
  }

  def reset: Unit = setItems(items)

  def enqueue(item: T): Unit = setItems(items :+ item)
  def deque: T = {
    val removed = items.head
    setItems(items.tail)
    removed
  }
}


