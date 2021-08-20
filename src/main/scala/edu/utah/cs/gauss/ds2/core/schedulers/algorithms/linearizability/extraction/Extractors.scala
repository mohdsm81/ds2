package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
// import scalax.collection.edge.LBase.LEdgeImplicits
import scalax.collection.io.dot._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.DistributedSystem
import scala.sys.process._
import scalax.collection.edge.Implicits._
import implicits._

import scala.collection.mutable.{ Seq => MSeq }
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
object Extractors {

  def printGraphToFile(graph: CausalGraphExtraction, filePath: String): Unit = {
    import edu.utah.cs.gauss.serialization.IO.writeLinesToFile
    writeLinesToFile(Seq(graph.toDot().toString()), new File(filePath))
  }
  def printCausalOnlyGraphToFile(graph: CausalGraphExtraction, filePath: String): Unit = {
    import edu.utah.cs.gauss.serialization.IO.writeLinesToFile
    writeLinesToFile(Seq(graph.toDot(graph.getCausalOnly).toString()), new File(filePath))
  }

  def compileDotToPDF(filePathToCompile: String): Unit = s"dot -Tpdf $filePathToCompile -o $filePathToCompile.pdf".!

  /**
   * This function has to be called on every "instance/copy" of a
   * DistributedSystem that just has been created and linked. Not
   * calling this one on each DistributedSystem instance just created
   * may lead to weird stuff and will result in both static and causal
   * graph (sub graph of static graph extracted) to be stale
   * (i.e. useless/out-of-context).
   */
  private def extractBehaviors(a: Agent): Seq[Behavior] = a.specialReactions +: (a.reactions +: a.behaviors.values.toSeq)

  def extractGraph(ds: DistributedSystem): Graph[CausalNode, LDiEdge] = {
    import scalax.collection.GraphPredef._

    // sets to remove duplication
    var nodes: Set[CausalNode] = Set()
    var edges: Set[LDiEdge[CausalNode]] = Set()
    var lastStmt: Statement = null
    var reset: Boolean = false

    // the agent/action/stmts nodes+edges construction.
    for {
      agent <- ds.agents.toSeq
      behavior <- extractBehaviors(agent)
      (message, action) <- behavior.reactions
      statement <- action.stmts
    } {
      // update the nodes
      nodes = nodes + new CausalNode(agent) + new CausalNode(statement)
      edges = edges + { // construction of the edge starts here

        if(statement == action.stmts.head)
          LDiEdge[CausalNode, MyLabel](new CausalNode(agent), new CausalNode(statement))(ActionLabel(behavior, message))
        else {
          val edge = LDiEdge[CausalNode, MyLabel](new CausalNode(lastStmt), new CausalNode(statement))(StatementLabel(lastStmt))
          if(statement == action.stmts.last) reset = true
          edge
        }
      } // block returns an edge instance

      reset match{
        case true =>
          lastStmt = null
          reset = false
        case false => lastStmt = statement
      }
    }
    Graph.from(nodes, edges)
  }

  def stmtOf(node: CausalNode): Statement = node match {
    case CausalNode(_,_,Some(x)) => x
    case _ =>
      println(s"PASSED ARG: $node")
      throw new Error("expandControlFlow()#stmtOf() -- couldn't extract the statement from the StatementNode")
  }

  def toEdges[T <: CausalNode](stmtsNodes: Seq[T]): Seq[LDiEdge[T]] =
    (stmtsNodes zip stmtsNodes.tail) map { case (src, dst) => LDiEdge(src, dst)(src) }

  def process(node: CausalNode): Seq[LDiEdge[CausalNode]] = {
    val edges = stmtOf(node) match {
      case x: Function =>
        toEdges((x +: x.body).map { y => new CausalNode(y) })
      case x: If => // includes while, else, elseif
        LDiEdge(new CausalNode(x), new CausalNode(x.body.head))(true) +: toEdges((x.body).map { y => new CausalNode(y) })
      case _ => Seq() // no more edges! it must have been added to an edge in one of above cases or initial static graph
    }
    edges
  } // end of process(node)

  def recursivelyProcessFlowControl(node: CausalNode): Seq[Seq[LDiEdge[CausalNode]]] = {
    val seqOfEdges: Seq[Seq[LDiEdge[CausalNode]]] = stmtOf(node) match {
      case x: Function =>
        process(new CausalNode(x)) +: x.body.flatMap { st => recursivelyProcessFlowControl(new CausalNode(st)) }
      case x: While => // special case because the body.last stmt is itself (infinite recursion)
        process(new CausalNode(x)) +: x.body.dropRight(1).flatMap { st => recursivelyProcessFlowControl(new CausalNode(st)) }
      case x: If => // includes  else, elseif
        process(new CausalNode(x)) +: x.body.flatMap { st => recursivelyProcessFlowControl(new CausalNode(st)) }
      case _ => Seq() // no more edges! it must have been added to an edge in one of above cases or initial static graph
    }
    seqOfEdges
  }

//  private def stmtEdgesToMyNodeEdges(edges: Seq[LDiEdge[CausalNode]]): Seq[LDiEdge[CausalNode]] = {
//    val moreEdges: Seq[LDiEdge[CausalNode]] = edges.map { x =>
//      LDiEdge[CausalNode,CausalNode](x._1, x._2)(x.label)
//    }
//    moreEdges
//  }

  def expandControlFlow(g: Graph[CausalNode, LDiEdge]): Graph[CausalNode, LDiEdge] = {
    // val nodesToExtend = g.nodes.toOuterNodes.filter { x => x.isStatementNode}
    val nodesToExtend = g.nodes.toOuter.filter { x => x.isStatementNode}
    val moreEdges = nodesToExtend.flatMap { x => recursivelyProcessFlowControl(x) }.flatten.toSeq
    g ++ moreEdges
  } // end of method expandControlflow()

  // private def removeSelfEdges(edges: Seq[LDiEdge[MyNode]]): Seq[LDiEdge[MyNode]] = {
  //   ???
  // }

  def expandCausalEdges(g: Graph[CausalNode, LDiEdge]): Graph[CausalNode, LDiEdge] = {
    val stmts = g.nodes.toOuter.filter { x => x.isStatementNode }.map { y => stmtOf(y) }.map {
      case x: Ask =>
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgentVar))(new CausalNode(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgent))(StatementLabel(x))
      case x: Send =>
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgentVar))(new CausalNode(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgent))(StatementLabel(x))
      /* get and t-get are both after an Ask is being executed and
         * has temporary-agents involved. So, we are skipping them for
         * now till we figure out we really need them*/
      // case x: Get =>
      //   if(x.isDynamic) LDiEdge(StatementNode(x), DynamicNode(x.dstAgentVar))(StatementNode(x))
      //   else LDiEdge(StatementNode(x), AgentNode(x.dstAgent))(StatementLabel(x))
      // case x: TimedGet => 
      case x: Kill =>
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgentVar))(new CausalNode(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgent))(StatementLabel(x))
      case x: Start =>
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgentVar))(new CausalNode(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgent))(StatementLabel(x))
      case x: Stop =>
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgentVar))(new CausalNode(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.dstAgent))(StatementLabel(x))
      case x: Create => // creating an agent is ALWAYS dynamic, the only static vs dynamic in it is getting the actual Agent once created
        if (x.isDynamic) LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.childAgentNameVar))(StatementLabel(x))
        else LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x.childAgentName))(StatementLabel(x))
      case x: Statement => LDiEdge[CausalNode, Any](new CausalNode(x), new CausalNode(x))(StatementLabel(x))
    }

    // removing self-edges
    val saneStmts = stmts.filterNot {
      case edge if (edge._1.isStatementNode && edge._2.isStatementNode) => edge._1.s.get.id == edge._2.s.get.id
      case _ => false
    }

    g ++ saneStmts
  }

  def extractCausal(g: Graph[CausalNode, LDiEdge]): Graph[CausalNode, LDiEdge] = {
    /* This is what we need from this method: we need to transform the
     * graph so that it contains causal paths only (i.e. containing
     * edges to and from Statements such as Send, Ask, Get, TimedGet, ...etc
     * from/to Agent/Statement Nodes) */
    val causalNodes = g.nodes.toOuter.filter(_.isCausal) // isCausal means isStatementNode too
    val agentNodes = g.nodes.toOuter.filter(_.isAgentNode)

    var pathsToCausalNodes =
      for {
        agentNode <- agentNodes
        causalNode <- causalNodes
      } yield {
        (g get agentNode) pathTo (g get causalNode)
      }

    var pathsFromCausalNodes =
      for {
        causalNode <- causalNodes
        agentNode <- agentNodes
      } yield {
        (g get causalNode) pathTo (g get agentNode)
      }

    def processPath(path: g.Path): Set[LDiEdge[CausalNode]] = path.edges.map { x => x.toOuter }.toSet

    val paths = (pathsToCausalNodes ++ pathsFromCausalNodes).flatMap { x => x }
    val outerEdges = paths.flatMap { x => processPath(x) }

    Graph.from(Seq(), outerEdges.toSeq) // the causal-only graph, i.e. the graph containing ONLY paths that involve at least one causal node e.g. send/ask/...etc
  }
}
