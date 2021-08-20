package edu.utah.cs.gauss

import scala.collection.parallel.ParSet

object Helpers{

  object Discrete{
    object Math{
      /**
       Return ALL possible subsets of a sequence of size 2 or more.
       */
      def subsetsOf[T](seq: Iterable[T]): Set[Seq[T]] = seq.toSet.subsets.filter(_.size >= 2).map{x => x.toSeq}.toSet

      /**
       compute all-sizes permutations of size 2 or more.
       */
      def permutationsOf[T](set: Set[Seq[T]]): Set[Seq[T]] = set.flatMap { x => x.permutations.toSet }

      /**
       Computes the 2 elements or greater sized subsets of the set of sequences provided as parameter
       */
      def combinationsOf[T1](set: Set[Seq[T1]]): Set[Seq[T1]] = set.flatMap { x => subsetsOf(x) }

      /**
       constructs the colletion of pairs of the cartisian product of the
       two collections passed as arguments.
       */
      def cartisianProductOf[T1,T2](collection1: Iterable[T1], collection2: Iterable[T2]): Iterable[(T1,T2)] = {
        for{
          item1 <- collection1
          item2 <- collection2
        } yield (item1, item2)
      }
    }
  }

  object DecoratorMethods{

    /**
     to add the "something in Set[SomeThing]" method
     */
    implicit class SetContains[SomeType](something: SomeType){
      def in(set: Set[SomeType]): Boolean = set.contains(something)
    }

    /**
     to add the "something in Set[SomeThing]" method
     */
    implicit class ParSetContains[SomeType](something: SomeType){
      def in(set: ParSet[SomeType]): Boolean = set.contains(something)
    }
  }
  object CollectionsUtils{
    def zip3[T1,T2,T3](xs: Iterable[T1], ys: Iterable[T2], zs: Iterable[T3]): Iterable[(T1,T2,T3)] = xs zip ys zip zs map {x => (x._1._1,x._1._2,x._2)}
  }
}
