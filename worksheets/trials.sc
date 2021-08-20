object trials {
//  import scala.collection.mutable._
//
//  class GenericHistory[T <: AnyRef, Op](entries: Seq[T]) extends ArrayBuffer[T]{
//
//  	val lifted = Stack[Op]()
//
//  	this ++= entries
//
//  	def liftEntry(idx: Int) = {
//  	this.lift(idx)
//  	}
//  }
//
//
//  val history = new GenericHistory[Int,String](Seq(1,2,3,4,5))
//
//  history.liftEntry(1)
//
//  println(history)
//
//
//  history.lifted.contains(2)
//
//  history.toString()
  
  
  val resultsTuple = List( "one", "one", "one", "two", "five", "five", "five", "five" ).groupBy( identity ).toList.reduce{ ( x, y) => if( x._2.size > y._2.size) x else y}
  
  
  println( resultsTuple )
  
}