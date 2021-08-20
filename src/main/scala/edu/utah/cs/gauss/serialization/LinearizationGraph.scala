package edu.utah.cs.gauss.serialization

import java.io.File
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes.Event
import scala.collection.mutable.{ Map => MMap, Seq => MSeq }
import scalax.collection.edge.LBase.LEdgeImplicits

import scala.sys.process._

object MyImplicit extends LEdgeImplicits[MyLabel]; import MyImplicit._

/**
 * <code>what</code> can be any symbol to indicate Invocation => I', or Response = R'
 */
case class MyNode(spec: MMap[Any, Any]) {
  override def toString: String = s"SPEC = \n ${spec.mkString("\n")}"
}
// case class MyNode(eventId: Int, what: Symbol, spec: MMap[Any, Any]) {
//   override def toString: String = s"$eventId => $what \n SPEC = \n ${spec.mkString("\n")}"
// }

case class MyLabel(var entry: Event, var optionalSeqNo: Int = 0, var isLinearizable: Boolean = false, var isCached: Boolean = false) {
  override def toString: String =
    s"(${optionalSeqNo}) - ${entry.getID}. ${entry.getClass.getSimpleName()(0)} => ${entry.getKey} ${entry.getOperation} ${entry.getArgs.head}\n(L = $isLinearizable, C = $isCached)"

  // overriding to exclode the "optionalSeqNo" field from comparisons (As it is just labeling for the order to traverse the graph)
  override def hashCode: Int = getClass.getSimpleName.hashCode() + entry.hashCode() + isLinearizable.hashCode() + isCached.hashCode()
  override def equals(that: Any): Boolean = hashCode() == that.hashCode()
}
case class MyEdge(var src: MyNode, var label: MyLabel, var dst: MyNode){
  def toLDiEdge: LDiEdge[MyNode] = LDiEdge(src,dst)(label)
}

object LinearizationGraph {
  // NOTE:
  //  (1 ~+> 2)(x) is LDiEdge(1,2)(x). That is, a directed edge from 1 to 2 labeled with x

  import scalax.collection.edge.LDiEdge, scalax.collection.edge.Implicits._
  import implicits._

  def makeGraph(transitions: MSeq[MyEdge]): Graph[MyNode, LDiEdge] = {
    // val listOfEdges: Seq[LDiEdge[MyNode]] = transitions map { x: MyEdge => LDiEdge(x.src, x.dst)(x.label) }
        val listOfEdges: Seq[LDiEdge[MyNode]] = transitions map {_.toLDiEdge }
    Graph(listOfEdges:_*)
  }

  def printGraphToFile(graph: Graph[MyNode, LDiEdge], filePath: String): Unit = {
    import IO._
    val root = DotRootGraph(
      directed = true,
      id = Some("graph_to_help_debug"))
    def edgeTransformer(innerEdge: Graph[MyNode, LDiEdge] #EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
      innerEdge.edge match{
        case LDiEdge(src,target,label) =>
          Some((root,
                DotEdgeStmt(src.toString,
                            target.toString,
                            List(DotAttr("label", label.toString)))))
      }
    }

    writeLinesToFile(Seq(graph.toDot(root, edgeTransformer)), new File(filePath))
  }

  def compileDotToPDF(filePathToCompile: String): Unit = s"dot -Tpdf $filePathToCompile -o $filePathToCompile.pdf".!
}
