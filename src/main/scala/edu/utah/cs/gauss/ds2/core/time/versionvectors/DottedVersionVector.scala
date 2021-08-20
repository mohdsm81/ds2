package edu.utah.cs.gauss.ds2.core.time.versionvectors

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.JsonSerializable
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.JsonDeSerializable

import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.collection.mutable.Map

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 * This is the default optimized Dotted Version Vector implemented by Basho Riak 2.0.
 * It tracks causality accurately by just relying on server-ids
 * instead of relying on client ids or a mix of both. This keeps the
 * size of the clock very small and doesn't allow "false concurrency".
 *
 * In this implementation, we relied on the github repo info and the
 * paper in it: https://github.com/ricardobcl/Dotted-Version-Vectors
 */

import DottedVersionVectorTypes._

object DottedVersionVectorTypes {
  type DVV = DottedVersionVector
  type VV = Map[String, Dot]
}

object Dot extends JsonDeSerializable[Dot] {
  // override def fromJson(js: JValue) = {
  //   implicit val formats = DefaultFormats
  //   js.extract[Dot]
  // } // fromJson

  override def fromJson(js: JValue) = {
    // ("Dot" ->
    //   ("id" -> id) ~
    //   ("counter" -> counter) ~
    //    ("version" -> version))

    val newOne = Dot("place-holder",0,0)
    js \ "Dot" \ "id" match{
      case JString(x) => newOne.id = x
      case _ => throw new Error("Dot.fromJson - can't extract 'id'")
    }

    js \ "Dot" \ "counter" match{
      case JInt(x) => newOne.counter = x
      case _ => throw new Error("Dot.fromJson - can't extract 'counter'")
    }

    js \ "Dot" \ "version" match {
      case JInt(x) => newOne.version = x
      case _ => throw new Error("Dot.fromJson - can't extract 'version'")
    }
    newOne
  }
} // Dot object

case class Dot(var id: String = "", var counter: BigInt = 0, var version: BigInt = 0) extends JsonSerializable {
  require(version >= counter,"Dot.constructor -- counter can never be bigger than version")
  //----------------------------------------
  // Comparators
  //----------------------------------------
  // equal
  def ===(that: Dot): Boolean = id == that.id && version == that.version
  // dominates this
  def <(that: Dot): Boolean = version < that.version
  // descends this
  def <=(that: Dot): Boolean = version <= that.version
  // dominates that
  def >(that: Dot): Boolean = version > that.version
  // descends that
  def >=(that: Dot): Boolean = version >= that.version
  // make sure this is right
  def overlapsWith(that: Dot) =
    (counter <= that.counter && that.counter < version) ||
      (counter <= that.version && that.version <= version)

  def update(ver: BigInt): Unit = {
    require(ver >= counter, "Dot.update -- 'version' has to be equal or greater than 'counter'")
    version = ver
  }
  def merge(dot: Dot): Unit = {
    counter = this maxCounter dot // max counter
    version = this max dot // max version
  }
  /**
   increment version
   */
  def incr = update (version + 1)
  def ++ = incr

  def updateCounter(counter: BigInt): Unit = this.counter = counter
  def incrCounter = updateCounter(counter + 1)
  /**
   * computes the max counter of both dots
   */
  def maxCounter(that: Dot) =
    if (counter < that.counter) that.counter
    else if (counter > that.counter) counter
    else counter // any of them is the same

  /**
   * max based on version number only, doesn't consider the counter
   */
  def max(that: Dot): BigInt = {

    if (version < that.version) that.version
    else if (version > that.version) version
    else version // same anyways
  }

  override def toJson: JValue =
    ("Dot" ->
      ("id" -> id) ~
      ("counter" -> counter) ~
      ("version" -> version))

  override def toString: String = s"(${id},${counter},${version})"
} // Dot



// Dotted Version Vector
case class DottedVersionVector(nodeName: String) extends JsonSerializable {

  // version vector, a map from node-id to version number
  protected var vv: VV = Map[String, Dot]()
  var dot: Dot = Dot(nodeName)
  protected var semanticMergeFunction: Function1[DVV, Unit] = (dvvUpdate: DVV) => {
    // counter (history)
    dot.counter = dot.version // update my history to current version before updating the version!

    // version
    dot.version = DottedVersionVector.max(dot.version, dvvUpdate(dot.id).version)

    dvvUpdate.toVV.values.map(this merge _)
  }

  //----------------------------------------
  // Automic modifiers
  //----------------------------------------
  def setSemanticMergeFunction(func: Function1[DVV, Unit]): Unit = semanticMergeFunction = func

  def merge(dot: Dot): DVV = {
    update(dot)
    this
  }

  def merge(that: DVV): Unit = semanticMergeFunction(that)
  
  def mergeAndAdvance (that: DVV): Unit = {
    merge(that)
    incr(nodeName)
  }

  def merge(dvvs: Set[DVV]): Unit = dvvs.map(merge(_))

  def toVV: VV = {
    val vvCopy = Map[String, Dot]()
    vv.map { d => vvCopy(d._1) = d._2.copy() }
    vvCopy(dot.id) = dot
    vvCopy
  }
  

  //----------------------------------------
  // Convenience function
  //----------------------------------------

  def copy: DVV = {
    val newOne = DottedVersionVector(nodeName)
    newOne.dot = dot.copy(dot.id, dot.counter, dot.version)
    newOne.vv = vv map {x => (x._1, x._2.copy(x._2.id,x._2.counter, x._2.version))}
    newOne
  }
  
  /**
   * Gets the dot belonging to the node owning this DVV
   */
  def apply: Dot = dot

  /**
   * gets the indicated component
   */
  def apply(id: String): Dot = component(id)

  /**
   * This is merely an update method, overrides the entry/component (doesn't union the clocks)
   */
  def update(dot: Dot): Unit = {
    var targetDot: Dot = component(dot.id) // may create a dot with dot.id and return it

    if (this.dot.id == dot.id) this.dot merge dot
    else {
      targetDot merge dot
      vv(dot.id) = targetDot // just in case it was a newly created dot
    }
  }

  /**
   * A convenience method to merge and advance using only the id's of follower
   * and followed dots.
   * 
   * The follower is the process/client that does an action strictly after the
   * followed has finished its action and it took effect.
   * 
   */
  def follows(followerID: String, followedID: String): Dot = {
    val followerDot = component(followerID)
    val followedDot = component(followedID)
    
    followerDot.merge(followedDot)
    update(followerDot)
    advance(followerID)
    component(followerID) 
  }

  /**
   * Another convenience method that advances the component identified by the
   * 'id' and the dot of this clock.
   * 
   */
  def advance(id: String): Unit = {
    val theDot = component(id)

    if(theDot.id == this.dot.id)
      dot.counter = dot.version

    theDot.incr // incr version

    update(theDot) // update the current DVV with the Dot
  }
  def advance: Unit = advance(dot.id)
  
  def causalHistory: VV = vv
  //----------------------------------------
  // Comparators
  //----------------------------------------
  /*
   Note how the DDV is more optimized in computing the partial order
   compared to the traditional VV (which necessiate iterating over all
   VV components) to compute it.
   */

  // equal everything
  def ===(that: DVV): Boolean = dot === that(dot.id) // && vv.values.par.forall{x => x == that.component(x.id)}
  // dominates this
  def <(that: DVV): Boolean = dot < that(dot.id) // && vv.values.par.forall{x => x < that.component(x.id)}
  // descends this
  def <=(that: DVV): Boolean = this < that || this === that
  // dominates that
  def >(that: DVV): Boolean = dot > that(dot.id) // && vv.values.par.forall{x => x > that.component(x.id)}
  // descends that
  def >=(that: DVV): Boolean = this > that || this === that
  /** Concurrent Dots returned, None otherwise*/
  def ||(that: DVV): Boolean = !(this < that) && !( that < this)

  // {
  //   /* 
  //    It is easier than I thought, but definitly not as easy as I
  //    stated it earlier: !(this < that) && !(this > that)
  //    */
  //   // dot.overlapsWith(that(dot.id)) match {
  //   //   case true  => Some((dot, that.dot))
  //   //   case false => None
  //   // }
  // }

  /**
   * <code>true</code> if the they are concurrent DVVs,
   * <code>false</code> otherwise
   */
  def isConcurrentWith(that: DVV): Boolean = this || that
  // descends is LE
  def descends(that: DVV): Boolean = this >= that
  // dominates is strictly smaller
  def dominates(that: DVV): Boolean = this > that
  // strictly descends is also dominates
  def strictDescends(that: DVV): Boolean = dominates(that)

  def supersedes(that: DVV): Boolean = this >= that
  def happensBefore(that: DVV): Boolean = that.descends(this)

  //----------------------------------------
  // Auxiliary
  //----------------------------------------
  def ids: Set[String] = Set(dot.id) union vv.keySet

  def contains(id: String): Boolean = vv.contains(id) || dot.id == id

  def component(id: String): Dot =
    if (id == dot.id) dot
    else if (contains(id)) vv(id)
    else Dot(id)

  def counter(id: String): BigInt = component(id).counter
  def incrementCounter(id: String): BigInt = {
    val aDot = component(id)
    aDot.counter += BigInt(1)
    update(aDot)
    component(id).counter
  }

  def version(id: String): BigInt = component(id).version

  /**
   * Increments the version number of the component identified by id
   */
  def incr(id: String): BigInt = {
    val aDot = component(id)
    aDot.version += BigInt(1)
    update(aDot)
    component(id).version
  }

  def ++(id: String): BigInt = incr(id)

  def updateVersionTo(id: String, newVersion: BigInt): Unit = component(id).version = newVersion
  def updateCounterTo(id: String, newCounter: BigInt): Unit = component(id).counter = newCounter
  //----------------------------------------
  // printing and logs
  //----------------------------------------
  override def toString: String = s"<<<${dot}, ${vv.values.mkString("[", ",", "]")}>>>"

  override def toJson: JValue =
    ("DottedVersionVector" ->
      ("dot" -> dot.toJson) ~
      ("vv" -> vv.values.map { x: Dot => x.toJson }))

} // DVV class

object DottedVersionVector {

  def ids(dot: Dot): Set[String] = Set(dot.id)
  def ids(vv: VV): Set[String] = vv.keySet.toSet // weird but have to call it
  def ids(dvv: DVV): Set[String] = ids(dvv.dot) union ids(dvv.vv)
  def ids(vvs: Set[DVV]): Set[String] = vvs.par.flatMap(ids(_)).seq.toSet
  // define the rest of ids variatoin

  //----------------------------------------
  // Utility functions
  //----------------------------------------
  def max(n1: BigInt, n2: BigInt): BigInt =
    if (n1 < n2) n2 else n1
  /**
   * Returns the latest Dot of two based on 'version' field
   */
  def max(d1: Dot, d2: Dot): Dot = if (d1 < d2) d2 else d1
  def max(dvv: DVV, replica: Dot): BigInt = max(replica, dvv.component(replica.id)).version
  def max(dvvs: Set[DVV], replica: Dot): BigInt = dvvs.par.map { dvv => max(dvv, replica) }.par.reduce(max(_, _))

  //----------------------------------------
  // Automic modifiers
  //----------------------------------------

  /**
   * Returns the set of concurrent DVVs in the two sets
   */
  def sync(s1: Set[DVV], s2: Set[DVV]): Set[DVV] = (s1.par.flatMap { x => s2.par.filter { x < _ } } union s2.par.flatMap { x => s1.par.filter { x < _ } }).seq

  /**
   * Returns a new DVV containing all the causal history of the replica's DVV set and Client DVV set
   */
  def update(s: Set[DVV], sr: Set[DVV], replica: Dot): DVV = {
    val id = replica.id
    val version = max(sr, replica) + 1

    val idees = ids(s)
    val mappedSR = sr.par.map { x => (x.dot.id -> x) }.toMap
    val vvIntermediate = s.par.map { dvv =>
      (dvv.dot.id ->
        Dot(dvv.dot.id,
          max(dvv.dot.counter,
            mappedSR(dvv.dot.id).dot.counter),
          max(dvv.dot,
            mappedSR(dvv.dot.id).dot).version))
    }.seq.toMap

    val dvv = DottedVersionVector(id)
    dvv.vv = Map()
    vvIntermediate.map { x => dvv.vv(x._1) = x._2 }
    dvv.dot = Dot(id, replica.counter, version)
    dvv
  } // update

  //----------------------------------------
  // Dottifying the version vector
  //----------------------------------------
  def fromVV(vv: VV, id: String): DVV = {
    require(vv.contains(id), "DottedVersionVector.fromVV -- can't extract the dot to compose a DottedVersionVector from a Version Vector")
    val dvv = DottedVersionVector(id)
    vv.values map (dvv update _)
    dvv
  }

  //----------------------------------------
  // Serialization
  //----------------------------------------
  def fromJson(js: JValue): DVV = {
    implicit val formats = DefaultFormats
    js.extract[DottedVersionVector]
  } // fromJson
} // object DVV

