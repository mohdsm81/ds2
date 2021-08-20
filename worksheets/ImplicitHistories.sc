import scala.collection.mutable.Seq

object ImplicitHistories {

  implicit class History(var log: Seq[Int]){
    val lifted = Seq[Int]()
    def liftIt(idx: Int): Unit = {
      lifted :+ log(idx)
      val (part1, part2) = log.splitAt(idx)
      log = part1 ++ part2.tail
    }
  }
  
  
  var log = Seq(1,2,3)                            //> log  : scala.collection.mutable.Seq[Int] = ArrayBuffer(1, 2, 3)
  
  log.liftIt(1)
  
  log.lifted                                      //> res0: scala.collection.mutable.Seq[Int] = ArrayBuffer()
  
  log.log                                         //> res1: scala.collection.mutable.Seq[Int] = ArrayBuffer(1, 2, 3)
  
  // damn, it doesn't work for mutating implicitly aded attributes...
  
}