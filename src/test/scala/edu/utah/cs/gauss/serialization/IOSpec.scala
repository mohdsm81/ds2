package edu.utah.cs.gauss.serialization

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.serialization.IO._
import java.io._

class IOSpec extends MyTestSpecs {

  test("delete file") {
    // create a file then delete it!
    val file = new File("data/test.dat")
    writeLinesToFile(Seq(""), file)
    assert(file.exists)
    deleteFile(file.getPath)
    assert(!file.exists)
  }


  test("append a string to file")
  {
    val file = new File("data/test.dat")
    writeLinesToFile(Seq("one","two"), file)
    appendToFile(file.getPath, "whatever")
    assert(readLinesFromFile(file).size == 3)
    deleteFile(file.getPath)
    assert(!file.exists)
  }

  test("append lines to file") {
    val file = new File("data/test.dat")
    writeLinesToFile(Seq("one","two"), file)
    appendToFile(file.getPath, "whatever", "whatever2")
    assert(readLinesFromFile(file).size == 4)
    deleteFile(file.getPath)
    assert(!file.exists)
  }
}
