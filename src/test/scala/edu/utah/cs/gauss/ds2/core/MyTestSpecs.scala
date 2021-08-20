package edu.utah.cs.gauss.ds2.core
import org.scalatest._
import prop._

/**
 * @author <br>
 *	Mohammed S. Al-Mahfoudh <br/>
 *	mahfoudh@cs.utah.edu <br/>
 *	SoC - Gauss Group <br/>
 * This is the test spec used in all the tests. They basically specify what scalatest styles we use.
 */
trait MyTestSpecs extends FunSuite with PropertyChecks with Matchers{
  import Matchers._
}
object MyTestSpecs {
  object PCompAlgTest extends Tag("PCompAlgTest")
  object MicroTest1 extends Tag("MicroTest1")
  object MicroTest2 extends Tag("MicroTest2")
  object MicroInfinity extends Tag("MicroInfinity")
  object GetTest extends Tag("GetTest")
  object P extends Tag("P") // P for partitioned
  object D extends Tag("D") // D for Debug
  object S extends Tag("S") // S for Sequential (i.e. not partitioned -- not to have to specify an exclude tag/tag-list)
  object Snippet extends Tag("Snippet")
  object C01K extends Tag("C01K") // K for OK
  object C01B extends Tag("C01B") // B for Bad
  object C10K extends Tag("C10K")
  object C10B extends Tag("C10B")
  object C50K extends Tag("C50K")
  object C50B extends Tag("C50B")
  object This extends Tag("This")
  object Fail extends Tag("Fail")
  object Infinity extends Tag("Infinity")
  object FastTest extends Tag("FastTest")
  object SlowTest extends Tag("SlowTest")
}

trait MySimpleSpecs extends FunSuite with Matchers {
  import Matchers._
}
