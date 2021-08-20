package edu.utah.cs.gauss.ds2.core.ir.datastructures

import java.util.UUID

import edu.utah.cs.gauss.ds2.core.state.{LocalStateState, Snapshot}
import edu.utah.cs.gauss.serialization.IO.{fromBytes, toBytes}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.collection.mutable.{Map => MMap}

/**
 * @author  	Mohammed S. Al-Mahfoudh <br/>
 * 		   	mahfoudh@cs.utah.edu <br/>
 * 		   	Gauss Group - SoC <br/>
 * 		   	The University of Utah <br/>
 *
 * @author  	Heath French<br/>
 * 		   	Gauss Group - SoC <br/>
 * 		   	The University of Utah <br/>
 */
@SerialVersionUID(111)
case class LocalState(var agentName: String = "") extends Snapshot[LocalStateState] {

  var varToMem = MMap[String, UUID]()
  var memToMem = MMap[UUID, UUID]()
  var memToVal = MMap[UUID, Any]()

  var garbageCollection = MMap[UUID, BigInt]()

  //----------------------------------------
  // Decoration/proxy methods
  //----------------------------------------
  def apply[T](varName: String): T = {
    getVal(varName).asInstanceOf[T]
  }

  /**
   * This update method is the same as "=" in mutable maps.
   * It maps <code>varName</code> to <code>value</code>.
   * @param varName the variable name to be assigned a value
   * @param value the value to be assigned to the varName
   */
  def update[T](varName: String, value: T): Unit = {
//    val className = value.getClass.getSimpleName
//    val variableType = varName.split(LocalState.DELIM)(1)
//    require(className ==  variableType || value == null,
//      () => s"LocalState.update(...) - can't assign a value that is not an instance of ${className} to variable: ${varName}")
    setVar(varName, value)
  }

  def +=(entry: (String, Any)): Unit = {
    setVar(entry._1, entry._2)
  }

  def +(entry: (String, Any)): LocalState = {
    +=(entry)
    this
  }

  //----------------------------------------
  def setVar(varName: String, value: Any): Unit = {
    // require(None != (pattern findFirstIn varName),"LocalState.setVar -- variable name doesn't conform to restricted pattern")
    LocalState.validateVariable(varName)
    /*
     * search for var.
     * 
     * not-exists? create new 
     * 
     * exists? modify it
     */

    if (!varToMem.contains(varName)) {
      // allocate new memory and bind var-to-val
      val id1 = UUID.randomUUID()
      val id2 = UUID.randomUUID()

      varToMem(varName) = id1
      memToMem(id1) = id2
      memToVal(id2) = value

      // count references
      garbageCollection(id1) = 1
    }
    else if (varToMem.contains(varName) &&
      memToVal(memToMem(varToMem(varName))) != value) {

      // allocate new memory and bind var-to-val
      val id1 = UUID.randomUUID()
      val id2 = UUID.randomUUID()

      val oldmemToMemID = varToMem(varName)

      varToMem(varName) = id1
      memToMem(id1) = id2
      memToVal(id2) = value

      // count references
      garbageCollection(id1) = 1

      // older ref to-de-ref GC count update
      garbageCollection(oldmemToMemID) = garbageCollection(oldmemToMemID) - 1

      garbageCollect(varName)

    }
    else // just update the binding from var-to-val
    {

      // finally do it
      memToVal(memToMem(varToMem(varName))) = value

    }
  }

  def getVal(varName: String): Any = {
    // require(None != (pattern findFirstIn varName),"LocalState.getVar -- variable name doesn't conform to restricted pattern")
    LocalState.validateVariable(varName)
    require(varToMem.contains(varName), "LocalState.getVal - variable \"" + varName + "\" is not declared yet.")

    memToVal(memToMem(varToMem(varName)))
  }

  def setRef(varNameToSet: String, varNameToRef: String): Unit = {
    LocalState.validateVariable(varNameToSet)
    LocalState.validateVariable(varNameToRef)

    require(varToMem.contains(varNameToSet), "LocalState.setRef - variable to set \"" + varNameToSet + "\" is not declared.")
    require(varToMem.contains(varNameToRef), "LocalState.setRef - variable to reference \"" + varNameToRef + "\" is not declared.")

    // to-set GC count update
    garbageCollection(varToMem(varNameToSet)) = garbageCollection(varToMem(varNameToSet)) - 1
    // to-ref GC count update
    garbageCollection(varToMem(varNameToRef)) = garbageCollection(varToMem(varNameToRef)) + 1
    // collect garbage
    garbageCollect(varNameToSet) // varNameToRef always increases

    // do it
    varToMem(varNameToSet) = varToMem(varNameToRef)

  }

  def getType(varName: String): Option[String] = {
    val pattern = LocalState.delimToString
    val keys = varToMem.filterKeys { x =>
      val k = x.split(pattern)
      k(0) == varName
    }
    if (!keys.isEmpty) Some(keys.head._1.split(pattern)(1)) else None
  }

  private def garbageCollect(varName: String): Unit = {
    LocalState.validateVariable(varName)
    // require(None != (pattern findFirstIn varName),"LocalState.garbageCollect -- variable name doesn't conform to restricted pattern")

    val refCount = garbageCollection(varToMem(varName))
    if (refCount <= 0) {
      // remove from all maps
      val id1 = varToMem(varName)
      memToVal -= memToMem(id1) // get rid of the value
      memToMem -= id1 // get rid of the mem-to-mem mapping
      // variable can not be un-declared, only non-referenced memory is freed

      garbageCollection -= id1 // even garbage collection is garbage collected :)
    }
  }

  private def garbageCollect(memToMemID: UUID) = {
    val refCount = garbageCollection(memToMemID)
    if (refCount <= 0) {
      // remove from all maps
      val id1 = memToMemID //varToMem(varName)
      memToVal -= memToMem(id1) // get rid of the value
      memToMem -= id1 // get rid of the mem-to-mem mapping
      // variable can not be un-declared, only non-referenced memory is freed

      garbageCollection -= id1 // even garbage collection is garbage collected :)
    }
  }

  def toJson: JValue = {
    ("LocalState" ->
      ("agentName" -> agentName) ~
      ("varToMem" -> varToMem.map { case (x: String, y: UUID) => JField(x, y.toString) }) ~
      ("memToMem" -> memToMem.map { case (x, y) => JField(x.toString, y.toString) }) ~
      ("memToVal" -> memToVal.map { case (x, y) => JField(x.toString, LocalState.serialize(y)) }) ~
      ("garbageCollection" -> garbageCollection.map { case (x, y) => JField(x.toString, toBytes(y) map { z => JInt(z) }) }))
  }

  def traceCopy: LocalState = {
    LocalState.fromJson(toJson)
  }

//  def copy: LocalState = {
//    val newOne = new LocalState(agentName)
//
//    // Mo: Do we really need to copy UUID's while they never change (maybe to run on a different JVM?)
//    varToMem map { p: (String, UUID) => newOne.varToMem += (p._1 -> UUID.fromString(p._2.toString)) }
//    memToMem map { p: (UUID, UUID) => newOne.memToMem += (UUID.fromString(p._1.toString) -> UUID.fromString(p._2.toString)) }
//    memToVal map { p: (UUID, Any) => newOne.memToVal += (UUID.fromString(p._1.toString) -> LocalState.copySpecific(p._2, this)) }
//    garbageCollection map { p: (UUID, BigInt) => newOne.garbageCollection += (UUID.fromString(p._1.toString) -> LocalState.copySpecific(p._2, this).asInstanceOf[BigInt]) }
//
//    newOne
//  }

//  def link(ds: DistributedSystem): Unit = {
//    memToVal map { x => LocalState.linkSpecific(ds, x._2, this) }
//  }

  //----------------------------------------
  //  Utility functions
  //----------------------------------------
  override def hashCode: Int = {
    varToMem.hashCode +
      memToMem.hashCode +
      memToVal.hashCode +
      garbageCollection.hashCode
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }

  override def snapshot: LocalStateState = LocalStateState(this)

  override def restore(state: LocalStateState): Unit = {
    state.instanceToRestore = this
    state.restore
  }

  override def toString: String = {
    varToMem.keysIterator.map{ k =>
      k.split("""\$\$""")(0) + " = " + apply(k).toString
    }.mkString("\n")
  }
}

object LocalState {
  val DELIM = "$$"
  
  def delimToString: String = DELIM.toList.map(x => s"${"\\" + x}").mkString
  
  val pattern = "(\\S+)\\$\\$(\\S+)".r
  def fromJson(js: JValue): LocalState = {
    implicit val formats = net.liftweb.json.DefaultFormats

    //    println("LocalState Obj: " + pretty(render(js \ "LocalState" \ "agentName")))

    val agentName = js \ "LocalState" \ "agentName" match {
      case JString(x) => x
      case null       => null
      case JNothing   => null
      case _          => throw new Error("LocalState.fromJson - No agentName OR no JObject representing LocalState passed.")
    }

    val localState = new LocalState(agentName)

    js \ "LocalState" \ "varToMem" match {
      case JArray(listOfPairs) => listOfPairs map {
        case JField(x, y) => localState.varToMem += (x.toString -> UUID.fromString(y.extract[String]))
        case _            => throw new Error("matching varToMem pairs yielded an outlier")
      }
      case _ => throw new Error("LocalState.fromJson - can't restore varToMem map")
    }

    js \ "LocalState" \ "memToMem" match {
      case JArray(listOfPairs) => listOfPairs map {
        case JField(x, y) => localState.memToMem += (UUID.fromString(x.extract[String]) -> UUID.fromString(y.extract[String]))
        case _            => throw new Error("matching memToMem pairs yielded an outlier")
      }
      case _ => throw new Error("LocalState.fromJson - can't restore memToMem map")
    }

    js \ "LocalState" \ "memToVal" match {
      case JArray(listOfPairs) => listOfPairs map {
        case JField(x, y) => localState.memToVal += (UUID.fromString(x.extract[String]) -> LocalState.deSerialize(y))
        case _            => throw new Error("matching memToVal pairs yielded an outlier")
      }
      case _ => throw new Error("LocalState.fromJson - can't restore memToVal map")
    }

    js \ "LocalState" \ "garbageCollection" match {
      case JArray(listOfPairs) => listOfPairs map {
        case JField(x, y) => localState.garbageCollection += (UUID.fromString(x.extract[String]) -> LocalState.deSerialize(y).asInstanceOf[BigInt])
        case _            => throw new Error("matching garbageCollection pairs yielded an outlier")
      }
      case _ => throw new Error("LocalState.fromJson - can't restore garbageCollection map")
    }

    localState
  }

  def serialize(value: Any): JValue = {
    value match {
      case x: Agent       => x.toJson
      case f: DummyFuture => f.toJson
      case a: Action      => a.toJson
      case agents: Iterable[Any] if(!agents.isEmpty && agents.head.isInstanceOf[Agent])=>
        agents.asInstanceOf[Iterable[Agent]].map(_.toJson)
      case futures: Iterable[Any] if(!futures.isEmpty && futures.head.isInstanceOf[DummyFuture]) =>
        futures.asInstanceOf[Iterable[DummyFuture]].map(_.toJson)
      case acts: Iterable[Any] if(!acts.isEmpty && acts.head.isInstanceOf[Action])=>
        acts.asInstanceOf[Iterable[Action]].map(_.toJson)
      case messages: Iterable[Any] if(!messages.isEmpty && messages.head.isInstanceOf[Message])=>
        messages.asInstanceOf[Iterable[Message]].map(_.toJson)
      case _              => toBytes(value) map { x => JInt(x) }
    }
  }

  def deSerialize(js: JValue): Any = {

    js match {
      case JObject(List(JField("Agent", JObject(_))))       => Agent.fromJson(js)
      case JObject(List(JField("DummyFuture", JObject(_)))) => DummyFuture.fromJson(js)
      case JObject(List(JField("Action", JObject(_))))      => Action.fromJson(js)
      case JArray(list @ JObject(List(JField("Agent", JObject(_))))::rest) =>
        // println(js(0))
        list map(Agent.fromJson(_))
      case JArray(list @ JObject(List(JField("DummyFuture", JObject(_))))::rest) =>
        list map{ DummyFuture.fromJson(_)}
      case JArray(list @ JObject(List(JField("Action", JObject(_))))::rest) =>
        list map{ Action.fromJson(_)}
      case JArray(list @ JObject(List(JField("MessageDummyFuture", JObject(_))))::rest) =>
        list map{ Message.fromJson(_)}
      case JArray(x) =>
        val bytesArray: Seq[Byte] = (x map {
          case JInt(y) => y toByte
          case _       => throw new Error("LocalState.deSerialize - yielded an outlier while matching a JInt()")
        })
        fromBytes(bytesArray)
      case _ => throw new Error("LocalState.deSerialize - unknown case")
    }
  }

  def snapshotSpecific(toCopy: Any, ls: LocalState): Any = {
    toCopy match {
      case iter: Iterator[Any] if ls != null => // note if ls isn't provided there is no way iterators are copied
        val (iter1,iter2) = iter.duplicate // these two iterators evolve independently, the original however doesn't
        val found: Option[(UUID, Any)] = ls.memToVal.find{case (_,v) => v == iter}
        if(found.isDefined) ls.memToVal(found.get._1) = iter1
        else throw new Error("How can this method used to copy something that isn't in the local-state object passed to it?")
        // iter1 // should be stored in this local state instead of "iter"
        iter2
      case x              => x // just a reference not a copy as in: fromBytes(toBytes(toCopy))
    }
  }

//  def linkSpecific(ds: DistributedSystem, toCopy: Any, ls: LocalState): Any = {
//    toCopy match {
//      case a: Agent => ds.get(a.name)
//      case f: DummyFuture => f.link(ds)
//      case a: Action      => a
//      case m: Message     => m.link(ds)
//      case agents: Iterable[Any] if(!agents.isEmpty && agents.head.isInstanceOf[Agent])=> agents.asInstanceOf[Iterable[Agent]].map{x => ds.get(x.name)} //.link(ds))
//      case futures: Iterable[Any] if(!futures.isEmpty && futures.head.isInstanceOf[DummyFuture]) => futures.asInstanceOf[Iterable[DummyFuture]].map(_.link(ds))
//      case acts: Iterable[Any] if(!acts.isEmpty && acts.head.isInstanceOf[Action])=> acts.asInstanceOf[Iterable[Action]].map(_) // previously was map(_.link(ds))
//      case messages: Iterable[Any] if(!messages.isEmpty && messages.head.isInstanceOf[Message])=> messages.asInstanceOf[Iterable[Message]].map(_.link(ds))
//      case _              => ; // do nothing
//    }
//  }

  def isValidVarName(variableName: String): Boolean = {
    pattern.findFirstIn(variableName) != None
  }

  def variableValidationMessage(variableProvided: String): String = {

    val variableSupplied = variableProvided match {
      case null => null
      case ""   => "EMPTY STRING"
      case _    => variableProvided
    }

    s"variables should be of the form 'variableName${DELIM}TypeName, e.g. myID${DELIM}Seq[Int]'. You provided ${variableSupplied}"
  }

  def validateVariable(variableName: String): Unit = {
    require(isValidVarName(variableName), variableValidationMessage(variableName))
  }

}
