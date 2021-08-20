package edu.utah.cs.gauss.ds2.core.time.versionvectors
import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.MyTestSpecs._
import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVectorTypes._

class DottedVersionVectorTypesSpec extends MyTestSpecs {

  //----------------------------------------
  //  DVV Fixtures
  //----------------------------------------
  def dvv1: DVV = {
    val dvv = DottedVersionVector("dvv1")
    dvv update Dot("dvv1" , version = 3)
    dvv update Dot("dvv2",version = 2)
    dvv
  }

  def dvv2: DVV = {
    val dvv = DottedVersionVector("dvv2")
    dvv update Dot("dvv1" , version = 10)
    dvv
  }

  def dvvsConcurrent: (DVV,DVV,DVV) = {

    /**
     This example is taken from Basho's Blog post:
     http://basho.com/posts/technical/vector-clocks-revisited-part-2-dotted-version-vectors/
     */

    val vnodeA = DottedVersionVector("VnodeA")

    val clientX = DottedVersionVector("ClientX")
    vnodeA update clientX.dot

    val clientY = DottedVersionVector("ClientY")
    vnodeA update clientY.dot

    clientX.incr(clientX.dot.id)
    clientX.incr(clientX.dot.id) // version is 2, counter is 0

    clientY.incr(clientY.dot.id)
    clientY.incr(clientY.dot.id) // version is 2, counter is 0

    (vnodeA,clientX,clientY)
  }

  //----------------------------------------
  //  Tests
  //----------------------------------------
  test("merge(a DVV)"){
    val big = dvv1
    big merge dvv2
    assert(big >= dvv1 && big >= dvv2)
  }

  test("merge(DVVs)"){
    val big = dvv1
    val somethingElse = dvv1
    somethingElse.updateVersionTo("whatever", 1000)
    big merge Set[DVV](dvv1, dvv2, somethingElse)

    assert(big >= dvv1 && big >= dvv2 && big >= somethingElse)
  }

  test("update"){
    val dvv = DottedVersionVector("mo")
    dvv update dvv1.dot
    assert(dvv(dvv1.nodeName) == dvv1.dot)
  }

  test("follows") {
    val dvv = dvv1
    dvv update Dot("hello", 4,5)
    
    dvv follows ("world", "hello")
    
    assert(dvv("world").version > dvv("hello").version &&
        dvv("world").counter == dvv("hello").counter)
        
    assert(dvv.toVV.size == 4)
    
  }
  

  //----------------------------------------
  //  Other Tests
  //----------------------------------------

  test("DVV1 is less than DVV2") {
    assert(dvv1 < dvv2)
    // why the following should give false? because these DVV's belong to different replica's!
    assert(!dvv2.descends(dvv1))
    assert(!dvv2.dominates(dvv1))
    assert(!dvv2.supersedes(dvv1))
    assert(!dvv1.happensBefore(dvv2))
  }

  test("DVV1 in one replica diff versions"){
    val dvv1pre: DVV = dvv1
    val dvv1post: DVV = {
      val dvv: DVV = dvv1
      dvv update Dot("dvv1", version = 100)
      dvv
    }
    assert(dvv1post.descends(dvv1pre))
    assert(dvv1post.dominates(dvv1pre))
    assert(dvv1post.supersedes(dvv1pre))
    assert(dvv1pre.happensBefore(dvv1post))
  }

  test("DVV3 and DVV4 are concurrent"){
    val (vnodeA: DVV,clientX:DVV, clientY: DVV) = dvvsConcurrent
    println(vnodeA)
    println(clientX)
    println(clientY)

    assert(clientX.dot.overlapsWith(clientY.dot))
    assert(clientX.isConcurrentWith(clientY))

    assert(vnodeA.isConcurrentWith(clientX))
    assert(vnodeA.isConcurrentWith(clientY))
  }

  test("Merging the concurrent clocks"){
    val (vnode: DVV,clientX: DVV,clientY: DVV) = dvvsConcurrent
    clientX.merge(clientY)
    assert(clientX(clientY.dot.id) == clientY.dot)
    // println(clientX)
    // println(clientY)

    assert(clientX > clientY)
  }

}
