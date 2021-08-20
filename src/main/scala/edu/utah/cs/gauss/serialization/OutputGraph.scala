package edu.utah.cs.gauss.serialization

import java.io.File

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction.CausalNode
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._

import scala.collection.mutable.{Map => MMap, Seq => MSeq}
import scalax.collection.edge.LBase.LEdgeImplicits

import scala.sys.process._

object MyImplicit2 extends LEdgeImplicits[MyLabel];

object OutputGraph {
  // NOTE:
  //  (1 ~+> 2)(x) is LDiEdge(1,2)(x). That is, a directed edge from 1 to 2 labeled with x

  import scalax.collection.edge.LDiEdge, scalax.collection.edge.Implicits._
  import implicits._

  def makeGraph(transitions: MSeq[LDiEdge[CausalNode]]): Graph[CausalNode, LDiEdge] = {
//     val listOfEdges: Seq[LDiEdge[MyNode]] = transitions map { x: MyEdge => LDiEdge(x.src, x.dst)(x.label) }
//    val listOfEdges: Seq[LDiEdge[CausalNode]] = transitions map {_.toLDiEdge }
//    Graph[CausalNode,LDiEdge](listOfEdges:_*)
    Graph[CausalNode,LDiEdge](transitions:_*)
  }

  def printGraphToFile(graph: Graph[CausalNode, LDiEdge], filePath: String): Unit = {
    import IO._
    val root = DotRootGraph(
      directed = true,
      id = Some("CausalGraph"))
    def edgeTransformer(innerEdge: Graph[CausalNode, LDiEdge] #EdgeT): Option[(DotGraph,DotEdgeStmt)] = {
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
