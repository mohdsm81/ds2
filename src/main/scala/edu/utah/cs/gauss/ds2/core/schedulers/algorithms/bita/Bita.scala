package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita

// data structures
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction.CausalNode
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */

object Criterion extends Enumeration {
  type Criterion = Value

  /**
   Pair-of-Behavior-Receives = PBR
   Pair-of-Receives = PR
   Pair-of-Consecutive-Receives = PCR
   */
  val PBR, PR, PCR = Value
}
object AbstractTypes{
  /**
   Message sent to the agent specify is a receive event
   */
  type Receive = (Message, Agent)
  type Schedule = Seq[Receive]
}

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.AbstractTypes._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.bita.Criterion._

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
case class BitaAlgorithm(var causalGraph: Graph[CausalNode, LDiEdge], var cr: Criterion = PR)
{
  /**
   All stuff in this class is taken from the Bita paper for coverage
   scheduling for actor programs.
   */

  implicit private var initSched: Schedule = Seq()

  var pairs: Set[(Receive,Receive)] = allPairs

  def setInitSched(sch: Schedule): Unit = {
    initSched = sch
    pairs = allPairs
  }
  //----------------------------------------
  //  Implicit class(es)
  //----------------------------------------

  /**
   Implicitly adds the ordered-by-criterion operator to all integers.
   */
  implicit class OrderedByCriterion(i: Int){
    def ->(j: Int)(implicit s: Schedule): Option[(Receive,Receive)] = {
      // FIXME need the inital schedule (as argument) to decide according to each criterion

      cr match{
        //------------------------------------------------------------
        case PCR =>
          val cond =
            rec(i.toReceive(s)) == rec(j.toReceive(s)) &&
              i < j &&
          !s.exists { rk => rec(rk) == rec(i.toReceive(s)) }
          cond match{
            case true => Some((i.toReceive(s),j.toReceive(s)))
            case false => None
          }
        //------------------------------------------------------------
        case PR =>
          val cond =
            rec(i.toReceive(s)) == rec(j.toReceive(s)) &&
              i < j
          cond match{
            case true => Some((i.toReceive(s),j.toReceive(s)))
            case false => None
          }
        //------------------------------------------------------------
        case PBR =>
          val cond =
            rec(i.toReceive(s)) == rec(j.toReceive(s)) &&
          (cb(j.toReceive(s)) || cb(i.toReceive(s))) &&
          !s.exists{rk =>
            val k = rk.toIndex(s)
            i < k && k < j &&
            rec(rk) == rec(i.toReceive(s)) &&
            cb(rk)
          }

          cond match{
            case true => Some((i.toReceive(s),j.toReceive(s)))
            case false => None
          }
      }
    }
  }

  /**
   Ordering goals acheived by a schedule
   */

  implicit class OrderingGoalsBySchedule(s: Schedule) {
    def orderingGoals: Set[(Receive,Receive)] = {
      pairs.filter{ p => isCrRelated(p._1, p._2)}
    }
  }

  /**
   to add the "something in Set[SomeThing]" method
   */
  implicit class SetContains[SomeType](something: SomeType){
    def in(set: Set[SomeType]): Boolean = set.contains(something)
  }
  implicit class SetContainsOption(something: Option[(Receive,Receive)]){
    def in(set: Set[(Receive,Receive)]): Boolean = something match{
      case None => false
      case Some(x) => x in set
    }
  }

  /**
   Conversions between Receive <-> Index-in-schedule
   */
  implicit class IndexConversions(r: Receive) {
    def toIndex(implicit s: Schedule): Int = s.indexOf(r)
  }
  implicit class ReceiveConversions(ri: Int) {
    def toReceive(implicit s: Schedule): Receive = s(ri)
  }

  implicit class ReceivePairConversions(p: (Receive, Receive))
  {
    def toIndices(implicit s: Schedule): (Int,Int) = (s.indexOf(p._1), s.indexOf(p._2))
  }
  implicit class IndicesPairConversions(p: (Int,Int)){
    def toReceives(implicit s: Schedule ): (Receive,Receive) = (s(p._1),s(p._2))
  }
 

  //----------------------------------------
  // Accessors and mutators
  //----------------------------------------
  def setGraph(g: Graph[CausalNode, LDiEdge]): Unit = causalGraph = g

  //----------------------------------------
  // Helpers
  //----------------------------------------

  /**
   checks if it is change behavior receive
   */
  def cb(r: Receive): Boolean = {
    /**
     STEPS:
     1- starting from this receive (Message name) on the causal graph
     2- find a "path" that has "become/unbecome" as an edge, for this specific agent.
     3- if found, return <code>true</code>. Otherwise, return <code>false</code>
     */
    val sourceAgentNode = causalGraph get new CausalNode(r._2)

    // subgraph focusing on the agent action invoked by the message, without the causal/communication edges
    val subGraph = causalGraph.filter{
      case n: CausalNode =>
        (n.isAgentNode && n.a.get.name == rec(r)) ||
        (n.isStatementNode && n.s.get.a.name == rec(r))

      case e: LDiEdge[CausalNode] =>
        (e.source.isStatementNode && !e.target.isAgentNode) ||
        (e.source.isAgentNode &&
           e.target.isStatementNode &&
           e.source.a.get.name == e.target.s.get.a.name)

      case _ => false
    } 
    
    subGraph.nodes.exists { n => n.isBecome || n.isUnBecome }
  }
  
  /**
   The message received
   */
  def msg(r: Receive): Message = r._1

  /**
   The name of the receiver of the message 
   */
  def rec(r: Receive): String = r._2.name

  /**
   Who sent this message (causing this receive)
   */
  def sender(r: Receive): String = r._1.sender.name

  /**
   Messages that are sent due to this receive <code>r</code>
   */
  def sent(r: Receive): Set[String] = {
    /**
     Same as cb(Receive)
     */
    val sourceAgentNode = causalGraph get new CausalNode(r._2)

    /** subgraph focusing on the agent action invoked by the message,
      * without the causal/communication edges
     */
    val subGraph = causalGraph.filter{ 
      case n: CausalNode =>
        (n.isAgentNode && n.a.get.name == rec(r)) ||
        (n.isStatementNode && n.s.get.a.name == rec(r))

      case e: LDiEdge[CausalNode] =>
        (e.source.isStatementNode && !e.target.isAgentNode) ||
        (e.source.isAgentNode &&
           e.target.isStatementNode &&
           e.source.a.get.name == e.target.s.get.a.name)

      case _ => false
    } 
    
    subGraph.nodes.filter { n => n.isSend || n.isAsk }.map{x =>
      val stmt = x.s.get.asInstanceOf[Send]
      if(!stmt.isDynamic) stmt.msgOut.name
      else stmt.localStateForRead(stmt.msgOutVar).asInstanceOf[Message].name
    }.toSet
  }

  /**
   Agents names created due to this receive <code>r</code>
   */
  def created(r: Receive): Set[String] = {
    /**
     Same as cb(Receive)
     */

    val sourceAgentNode = causalGraph get new CausalNode(r._2)

    /** subgraph focusing on the agent action invoked by the message,
      * without the causal/communication edges
     */
    val subGraph = causalGraph.filter{ 
      case n: CausalNode =>
        (n.isAgentNode && n.a.get.name == rec(r)) ||
        (n.isStatementNode && n.s.get.a.name == rec(r))

      case e: LDiEdge[CausalNode] =>
        (e.source.isStatementNode && !e.target.isAgentNode) ||
        (e.source.isAgentNode &&
           e.target.isStatementNode &&
           e.source.a.get.name == e.target.s.get.a.name)

      case _ => false
    }
    
    subGraph.nodes.filter { n => n.isCreate }.map{x =>
      val stmt = x.getValue.asInstanceOf[Create]
      if(!stmt.isDynamic) stmt.childAgentName
      else stmt.localStateForRead(stmt.childAgentNameVar).asInstanceOf[String]
    }.toSet
  }

  def allPairs(implicit s: Schedule ): Set[(Receive,Receive)] = {
      for{ ri <- s.toSet[Receive]
           rj <- s.splitAt(s.indexOf(ri))._2.tail.toSet[Receive]
      } yield (ri,rj)
  }

  //----------------------------------------
  // MustHB party
  //----------------------------------------

  /**
   The causal must-happen-before pairs
   */
  def mustHappenBefore_Causal: Set[(Receive,Receive)] = {
    /**
     This is a "translitration" from the Bita paper to Scala!
     */
    var result = pairs.filter {
      pair =>
      val (ri,rj) = pair

      (msg(rj).name in sent(ri)) ||
      (rec(rj) in created(ri)) ||
      initSched.exists{ rk =>

        val k = rk.toIndex
        val i = ri.toIndex
        val j = rj.toIndex

        i < k && k < j && rec(ri) == rec(rk) && (msg(rj).name in sent(rk)) } ||
      initSched.exists{ rk =>
        val k = rk.toIndex
        val i = ri.toIndex
        val j = rj.toIndex

        i < k && k < j && rec(ri) == rec(rk) && (msg(rj).name in created(rk))}
    } // end of filter

    result
  }

  /**
   The sender-receiver must-happen-before pairs
   */
  def mustHappenBefore_SenderReceiver: Set[(Receive,Receive)] = {

    var result = pairs.filter {
      pair: (Receive,Receive) =>
      val (ri,rj) = pair

      sender(ri) == sender(rj) &&
      rec(ri) == rec(rj)
    }
    result
  }

  /**
   The generic method that uses both causal and sender-receiver must
   happen before methods
   */
  def mustHappenBefore: Set[(Receive,Receive)] =
    mustHappenBefore_Causal union mustHappenBefore_SenderReceiver

  //----------------------------------------
  // Algorithm(s) methods
  //----------------------------------------
  var O = Set[(Receive,Receive)]() // ordering goals

  /**
   This algorithm is Algorithm 1 in the paper.
   NOTE: the second argument "cr" (criterion) is passed from the class
   constructor.
   */
  def generateSchedules: Seq[Schedule] = {
    var S = Seq[Schedule]() // empty list of schedules
    // The set of ordering goals 'O' is object attribute so that it is
    // accessible from another algorithm.
    for{
      (ri,rj) <- pairs
    }{
      if(isCrRelated(ri, rj)){
        var s: Schedule = Nil

        if(ri.toIndex -> rj.toIndex in O) s = schedule(initSched, ri.toIndex, rj.toIndex, false)
        if(!(rj.toIndex -> ri.toIndex in O)) s = schedule(initSched, ri.toIndex, rj.toIndex, true)

        S = S :+ s
        O = O ++ s.orderingGoals
      } // end if-cr-related
    }
    S
  }

  /**
   Checks if the two receives ordering is related to the criterion specified.
   NOTE: the second argument "cr" (criterion) is passed from the class
   constructor.
   */
  def isCrRelated(ri: Receive, rj: Receive): Boolean = {
    if(cr == PR || cr == PCR) rec(ri) == rec(rj)
    else if (cr == PBR) rec(ri) == rec(rj) && (cb(ri) || cb(rj))
    else false
  }

  var mustHB: Set[(Receive,Receive)] = _
  /**
   This algorithm is Algorithm 2 in the paper.
   NOTE: the second argument "cr" (criterion) is passed from the class
   constructor.
   */
  def schedule(implicit feasibleSchedule: Schedule, i: Int, j: Int, swap: Boolean): Schedule = {
    var s: Schedule = Seq[Receive]()
    mustHB = mustHappenBefore // will be initialized before any other algorithm uses it!

    for {k <- Range(i + 1, j-1)}
    { if((i,j).toReceives(feasibleSchedule) in mustHB) s = s :+ k.toReceive(feasibleSchedule) }

    if(swap) s = s :+ j.toReceive(feasibleSchedule) :+ i.toReceive(feasibleSchedule)
    else s = s :+ i.toReceive(feasibleSchedule) :+ j.toReceive(feasibleSchedule)

    val tail = s.tail
    scheduleTail(s, tail)
  }

  /**
   This algorithm is Algorithm 3 in the paper
   NOTE: the second argument "cr" (criterion) is passed from the class
   constructor.
   */
  def scheduleTail(p: Schedule, t: Schedule): Schedule = {
    var s_prime: Schedule = Seq()
    for{ ri <- t.dropRight(1)
         rj <- t.splitAt(ri.toIndex(t))._2.tail }
    {
      if(isCrRelated(ri, rj) && ((ri,rj) in mustHB)) {
        if(ri.toIndex -> rj.toIndex in O) {
          s_prime = schedule(p ++ t, ri.toIndex(t) + p.size, rj.toIndex(t) + p.size, false)
          return s_prime
        }
        else if(!(rj.toIndex -> ri.toIndex in O)) {
          s_prime = schedule(p ++ t, ri.toIndex(t) + p.size, rj.toIndex(t) + p.size, true)
          return s_prime
        }
      } // end of outer if
    } // end of for-loop
    return p
  }
}
