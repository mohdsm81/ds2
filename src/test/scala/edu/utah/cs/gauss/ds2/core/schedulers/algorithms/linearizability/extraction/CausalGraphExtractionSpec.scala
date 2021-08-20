package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.extraction.Extractors._
import edu.utah.cs.gauss.ds2.core.MyTestSpecs // specs
import edu.utah.cs.gauss.ds2.core.MyTestSpecs._ // tags
import edu.utah.cs.gauss.serialization.IO
import org.scalatest._
import java.io.File
@Ignore
class CausalGraphExtractionSpec extends MyTestSpecs {

  import edu.utah.cs.gauss.ds2.core.integration.TestBeds._

  test("extracting graph 1") {
    val g = CausalGraphExtraction(echoServerInstance)
    printGraphToFile(g, "data/causal.graph.1.dot")
    // compileDotToPDF("data/causal.graph.1.dot")
    /*
     Works as expected, there is no otehr kind of statements used
     except a generic statement in this one, so naturally the next
     test-case should show nothing (because no causal statements at
     all)
     */
  }


  test("extracting causal-only graph 1") {
    val g = CausalGraphExtraction(echoServerInstance)
    printCausalOnlyGraphToFile(g, "data/causal.graph.2.dot")
    // compileDotToPDF("data/causal.graph.2.dot")
    /*
     This shows nothing as expected
     */
  }


  test("extracting graph 2") {
    val g = CausalGraphExtraction(echoServerInstanceCopy)
    val FILE = "data/causal.graph.3.dot"
    printGraphToFile(g, FILE)
    compileDotToPDF(FILE)
  }

  test("extracting causal-only graph 2") {
    val g = CausalGraphExtraction(echoServerInstanceCopy)
    val FILE = "data/causal.graph.4.dot"
    printCausalOnlyGraphToFile(g, FILE)
    compileDotToPDF(FILE)
  }


  test("extracting graph 3 DEBUGGING", This) {
    val g = CausalGraphExtraction(echoServerWithAskInstance)
    val FILE = "data/causal.graph.5.DEBUG.dot"
    val graph = g.toDot().toString
    IO.writeLinesToFile(Seq(graph), new File(FILE))
    // printGraphToFile(g, FILE)
    compileDotToPDF(FILE)
  }

  test("extracting graph 3") {
    val g = CausalGraphExtraction(echoServerWithAskInstance)
    val FILE = "data/causal.graph.5.dot"
    printGraphToFile(g, FILE)
    compileDotToPDF(FILE)
  }

  test("extracting causal-only graph 3") {
    val g = CausalGraphExtraction(echoServerWithAskInstance)
    val FILE = "data/causal.graph.6.dot"
    printCausalOnlyGraphToFile(g, FILE)
    compileDotToPDF(FILE)
  }

  

}
