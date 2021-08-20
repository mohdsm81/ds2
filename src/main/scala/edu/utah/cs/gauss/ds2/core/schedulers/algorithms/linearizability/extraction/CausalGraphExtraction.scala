package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction

import java.util.UUID

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
// import scalax.collection.edge.LBase.LEdgeImplicits
import scalax.collection.io.dot._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import scala.sys.process._
import scalax.collection.edge.Implicits._
import implicits._

import scala.collection.mutable.{Seq => MSeq}
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Behavior
import java.io.File
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DummyFuture
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Message
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */
case class CausalGraphExtraction(val ds: DistributedSystem) {

  // calls extract for us on construction
  private var graph = extract

  def extract: Graph[CausalNode, LDiEdge] = {
    graph = Extractors.extractGraph(ds)

    // DEBUG
    // if(graph.edges.toOuterEdges.filter{ e => e._1.isStatementNode && e._2.isStatementNode && e._2.asInstanceOf[StatementNode].isAsk}.isEmpty) throw new Error("CausalGraphExtraction.extract -- NOOOOO")

    graph = Extractors.expandControlFlow(graph)
    graph = Extractors.expandCausalEdges(graph)
    graph
  }

  /**
    * Filters the cached static graph to include only causal paths
    */
  def getCausalOnly: Graph[CausalNode, LDiEdge] = Extractors.extractCausal(extract)

  /**
    * Returns the cached static graph
    */
  def getGraph: Graph[CausalNode, LDiEdge] = graph

  def toDot(f: => Graph[CausalNode, LDiEdge] = extract): String = {

    val root = DotRootGraph(
      directed = true,
      id = Some("Causal Legend"))

    def edgeTransformer(innerEdge: Graph[CausalNode, LDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
      innerEdge.edge match {
        case LDiEdge(src, target, label) =>
          Some((root,
            DotEdgeStmt(src.toString,
              target.toString,
              List(DotAttr("label", label.toString)))))
      }
    }

    extract toDot(root, edgeTransformer)
    f toDot(root, edgeTransformer)
  }
}

//------------------------------------------------------------
// Label types (note they don't need to extend common interface)
//------------------------------------------------------------
trait MyLabel{
  type LabelType1
  type LabelType2
  def value1: LabelType1
  def value2: LabelType2

  def isStatementLabel = false
  def isMessageLabel = false
  def isActionLabel = false
  def isDynamicLabel = false
}

case class MessageLabel(m: Message) extends MyLabel {
  type LabelType1 = Message
  type LabelType2 = Message
  override def value1 = m
  override def value2 = m
  override def isMessageLabel = true
  override def toString: String = m.name
}

case class ActionLabel(b: Behavior, m: Message) extends MyLabel // an action is identified by Agent.behavior.m tuple
{
  type LabelType1 = Message
  type LabelType2 = Behavior
  override def value1 = m
  override def value2 = b
  override def isActionLabel = true
  override def toString: String = s"${b.name}:${m.name}"
}

case class StatementLabel(s: Statement) extends MyLabel // edges between statements is labeled with previous statement
{
  type LabelType1 = Statement
  type LabelType2 = Statement
  override def value1 = s
  override def value2 = s
  override def isStatementLabel = true
  override def toString: String = s"${s.getClass.getSimpleName}: ${if (s.m != null) s.m.name}"
}

case class DynamicLabel(variable: String) extends MyLabel{
  // this is a label for edges whose destination is to be determined dynamically (not available statically)
  type LabelType1 = String
  type LabelType2 = String
  override def value1 = variable
  override def value2 = variable
  override def isDynamicLabel = true
  override def toString: String = s"variable = ${variable}"
} 
//------------------------------------------------------------
// NODE types/hierarchy
//------------------------------------------------------------
/**
  * Acts like a C++ union. Thanks to the way Graph[N,EdgeLike] is coded, i.e. no
  * co-variance on N.
  */
case class CausalNode(d: Option[String], a: Option[Agent], s: Option[Statement]) {
  def this(n: String) = this(Some(n), None, None)
  def this(n: Agent) = this(None, Some(n), None)
  def this(n: Statement) = this(None, None, Some(n))


  override def hashCode(): Int = {
    val hc = d.hashCode() +
    a.hashCode() +
    s.hashCode()
    hc
  }

  override def toString: String = {
    if(isAgentNode) a.get.toString
    else if(isStatementNode) s.get.toString
    else if(isDynamicNode) d.get.toString
    else ""
  }

//  override def equals(obj: Any): Boolean =
//    super.equals(obj)

  def getValue: Any = {
    if (d != None) return d.get
    if (a != None) return a.get
    if (s != None) return s.get
  }

  def isAgentNode: Boolean = a match {
    case Some(a) => true
    case None => false
  }

  def isStatementNode: Boolean = s match {
    case Some(s) => true
    case None => false
  }

  def isDynamicNode: Boolean = d match {
    case Some(d) => true
    case None => false
  }

  def isCausal: Boolean = {
    isStatementNode && (
      isSend ||
        isAsk ||
        // isGet || // for now we will ignore them
        // isTGet ||
        isStart ||
        isStop ||
        isKill ||
        isCreate ||
        isBecome ||
        isUnBecome)
  }

  def isReverseCausal: Boolean = {
    isStatementNode && (
      isGet ||
        isTGet)
  }

  def isStart: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Start] => true
      case _ => false
    })
  }

  def isStop: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Start] => false
      case Some(x) if x.isInstanceOf[Stop] => true
      case _ => false
    })
  }

  def isKill: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Stop] => false
      case Some(x) if x.isInstanceOf[Start] => false
      case Some(x) if x.isInstanceOf[Kill] => true
      case _ => false
    })
  }

  def isCreate: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Create] => true
      case _ => false
    })
  }

  def isSend: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Ask] => false // because Ask extends send so matchable
      case Some(x) if x.isInstanceOf[Send] => true
      case _ => false
    })
  }

  def isAsk: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[Ask] => true
      case _ => false
    })
  }

  def isGet: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[TimedGet] => false
      case Some(x) if x.isInstanceOf[Get] => true
      case _ => false
    })
  }

  def isTGet: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[TimedGet] => true
      case _ => false
    })
  }

  def isBecome: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[UnBecome] => false
      case Some(x) if x.isInstanceOf[Become] => true
      case _ => false
    })
  }

  def isUnBecome: Boolean = {
    isStatementNode && (s match {
      case Some(x) if x.isInstanceOf[UnBecome] => true
      case _ => false
    })
  }
}

