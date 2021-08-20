package edu.utah.cs.gauss.ds2.core.time.versionvectors
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVectorTypes._

class DotSpec extends MyTestSpecs {

  //----------------------------------------
  //   Fixtures
  //----------------------------------------
  def aDot: Dot = Dot("store", 4, 5)


  //----------------------------------------
  //   tests
  //----------------------------------------
  
  test("json") {
    val dot = aDot
    val dotCopy = Dot.fromJson(dot.toJson)
    assert(dot == dotCopy)
  }

  test("==="){
    val dot = aDot
    val dotCopy = aDot

    dotCopy.counter = -1 // I know wrong but i didn't want them to be equal and still the === method shouldn't fail

    assert(dot === dotCopy)
  }

  test("<") {
    val dot = aDot
    val dotCopy = aDot

    dotCopy.version += 1

    assert(dotCopy > dot)

  }

  test("<=") {
    val dot = aDot
    val dotCopy = aDot

    assert(dot <= dotCopy)

    dot.version += 1

    assert(!(dot <= dotCopy))
  }

  test(">") {
    val dot = aDot
    val dotCopy = aDot

    assert(!(dot > dotCopy))
    dot.version += 1

    assert(dot > dotCopy)
  }

  test(">="){
    val dot = aDot
    val dotCopy = aDot

    assert(dot >= dotCopy && dotCopy >= dot)
    dot.version += 10
    assert(dot >= dotCopy && !(dotCopy >= dot))
  }

  test("overlapsWith"){
    val dot = aDot
    val dotCopy = aDot

    dot.version += 1

    assert((dot overlapsWith dotCopy) && (dotCopy overlapsWith dot))
  }

  test("update"){
    val dot = aDot

    assert(dot.version == 5)
    dot update 10
    assert(dot.version == 10)
  }

  test("merge") {
    val dot = aDot
    val dotAnother = aDot

    dotAnother.++
    dotAnother.incrCounter // updateCounter is used and hence tested using this statement

    dotAnother merge dot

    assert( dotAnother > dot && dotAnother.counter > dot.counter)
  }

  test("maxCounter"){

    val dot = aDot
    val dotAnother = aDot

    dotAnother.++
    dotAnother.incrCounter // updateCounter is used and hence tested using this statement

    assert(dotAnother.maxCounter(dot) == dot.counter + 1)
  }

  test("max version") {
    val dot = aDot
    val dotAnother = aDot

    dotAnother.++
    dotAnother.incrCounter // updateCounter is used and hence tested using this statement

    assert(dotAnother.max(dot) == dot.version + 1)
    
  }

} // end of Dot-spec suite
