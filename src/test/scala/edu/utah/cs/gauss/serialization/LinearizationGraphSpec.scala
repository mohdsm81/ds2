package edu.utah.cs.gauss.serialization

import edu.utah.cs.gauss.serialization.LinearizationGraph._
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVector
import edu.utah.cs.gauss.serialization._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import scala.collection.mutable.{Map => MMap, Seq}

class LinearizationGraphSpec extends MyTestSpecs {

  // fixtures
  def inv: Event = Invocation(Some("1"), "5", ":append", Seq("x 1 0 y"))
  def res: Event = new Response(Some("1"), "5", ":append", Seq("x 1 0 y"))

  def node1: MyNode = MyNode(MMap[Any,Any]())
  def node2: MyNode = MyNode(MMap[Any,Any](Some("1") -> "x 1 0 y"))
  def label: MyLabel = MyLabel(inv,0,true,true)

  // generate trace as in the algorithm
  def stupidTrace: Seq[MyEdge] = Seq(MyEdge(node1,label,node2))

  test("Test Stupid Transitions"){
    printGraphToFile(makeGraph(stupidTrace), "data/graph.test.dot")
  }
}
