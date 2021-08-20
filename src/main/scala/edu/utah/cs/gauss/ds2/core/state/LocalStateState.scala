package edu.utah.cs.gauss.ds2.core.state

import java.util.UUID

import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState
import scala.collection.mutable.{Map => MMap}

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

case class LocalStateState(ls: LocalState) extends State[LocalState,LocalStateState] {
  override var instanceToRestore: LocalState = ls

  val agentName = instanceToRestore.agentName
  val varToMem = instanceToRestore.varToMem.clone()
  val memToMem = instanceToRestore.memToMem.clone()
  val garbageCollection = instanceToRestore.garbageCollection.clone()

  val memToVal = MMap[UUID,Any]()

    instanceToRestore.memToVal map{
    case (k, iterator: Iterator[Any]) => // note if ls isn't provided there is no way iterators are copied
      val (iterator1,iterator2) = iterator.duplicate // these two iterators evolve independently, the original however doesn't
      instanceToRestore.memToVal(k) = iterator1
      memToVal(k) = iterator2
    case (k,v) => memToVal(k) = v
      // anything is referenced, except for the iterator above, it is a very special case!
  }

  override def restore: Unit = {
    instanceToRestore.agentName = agentName
    instanceToRestore.varToMem = varToMem
    instanceToRestore.memToMem = memToMem
    instanceToRestore.memToVal = memToVal
  }

  override def toString: String = {
    varToMem.keysIterator.map{ x =>
     x.split("""\$\$""")(0) + " = " + memToVal(memToMem(varToMem(x)))
    }.mkString("\n")
  }
  def valOfVar[T](str: String): T = memToVal(memToMem(varToMem(str))).asInstanceOf[T]
}
