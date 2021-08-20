import scala.collection.mutable._
import scala.collection.parallel.mutable.ParArray
import scala.collection.generic._
import scala.collection.CustomParallelizable

object ExplicitHistories {

  case class History[T <: AnyRef](entries: Seq[T]) extends ArrayBuffer[T](entries.length)

    with IndexedSeq[T]
    with IndexedSeqOptimized[T, ArrayBuffer[T]]
    with CustomParallelizable[T, ParArray[T]]
    with Serializable {

    //      override def foreach[U](f: String with T => U): Unit = {}

    this ++= entries

    var lifted = Stack[T]()

    def liftIt(idx: Int): Unit = {
      lifted = lifted.push(this(idx))
      val (part1, part2) = splitAt(idx)
      clear()
      ++= (part1 ++ part2.tail)
    }

    override def toString: String = "History" + super.toString.substring("ArrayBuffer".size)
  }

  val myHistory = History(Seq("one", "two", "three"))
                                                  //> myHistory  : ExplicitHistories.History[String] = History(one, two, three)

  myHistory                                       //> res0: ExplicitHistories.History[String] = History(one, two, three)
  myHistory.liftIt(1)
	myHistory.lifted                          //> res1: scala.collection.mutable.Stack[String] = Stack(two)

  myHistory                                       //> res2: ExplicitHistories.History[String] = History(one, three)
}