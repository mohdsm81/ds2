package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

/**
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 *
 */

@SerialVersionUID(400)
case class Behavior(var name: String, dropping: Boolean = false) extends Serializable {

  var reactions = Map[Message, Action]()
  var a: Agent = _

  //==============================
  // Equality handling
  //==============================
//  override def hashCode: Int = {
//
//    val agntName = a match {
//      case null => null
//      case x: Agent => x.name
//    }
//
//    val agentNameHash = agntName match {
//      case null => 0
//      case x: String => x.hashCode
//    }
//
//    name.hashCode +
//      reactions.keys.hashCode +
//      //      reactions.values.hashCode + // this makes it fail why?!!
//      agentNameHash
//    //    aHash +
//
//  }
//
//  override def equals(that: Any): Boolean = {
//    hashCode == that.hashCode
//  }

  //==============================

  def apply(m: Message): Action = {
//    if (null != m && null != a)
//      a.sender = m.sender
//    else throw new Error("agent a can't be null when using the behavior")
    // debug
    //    resetActions
    val found = reactions.find(p => p._1.name.trim == m.name.trim)
    if (found == None && dropping) {
      val act = new Action
      act.setMessage(m)
      act.setAgent(a)
      act.runtimeCopy // return an empty action to execute nothing
    } else if (found == None && !dropping) {
      println(s"a: ${a.name}" )
      println(a.localState)
      throw new Error(s"Message not found in behavior: ${m};\n\n while behavior is ${reactions.keySet map ("\n" + _.name)}\n\n")
    } else {
      val pair = found.get
      pair._2.runtimeCopy
      //    reactions(m).copy
    }
  }

  def toJson: JValue = {
    val agntName = a match {
      case null => null
      case x: Agent => x.name
    }

    ("Behavior" ->
      ("name" -> name) ~
        ("dropping" -> dropping) ~
        ("reactions" -> reactions.map { x => ("message" -> x._1.toJson) ~ ("action" -> x._2.toJson) }) ~
        ("a" -> null) ~
        ("agentName" -> agntName))
  }

  // ===============================
  //  Utility
  // ===============================

  override def toString: String = {

    "Behavior '" + name + s"', with dropping=$dropping: \n" + reactions.mkString("(", ",", ")")

  }

  def is(that: Behavior): Boolean = {
    name == that.name
  }

  def !=(that: Behavior): Boolean = {
    !(this == that)
  }

  def +=(reaction: (Message, Action)): Behavior = {

    // old version (over writing the sequence)
    //    reactions = reactions + (reaction._1 -> reaction._2)


    // new version, appending to the sequence in case the key exists)
    val msg = reaction._1
    val act = reaction._2

    if (reactions.contains(msg)) {
      val oldAction = reactions(msg)
      oldAction.stmts = oldAction.stmts ++ act.stmts // added the stmts to the old action, retaining oldAction id, a, m ... etc
      oldAction.setAgent(a)
      oldAction.reset
    }
    else {
      reactions = reactions + (msg -> act)
      act.setAgent(a)
    }

    this
  }

  def +(m: Message, st: Statement): Behavior = {
    if (reactions.contains(m)) {
      val oldAction = reactions(m)
      val newAct = oldAction + st
      reactions = reactions + (m -> newAct)
    }
    else {
      val newAct = new Action
      newAct + st
      reactions = reactions + (m -> newAct)
    }

    this
  }


  def -=(msg: Message): Behavior = {
    reactions.synchronized {
      reactions - msg
    }
    this
  }

  def in(behaviors: Set[Behavior]): Boolean = {
    require(behaviors != null, "Behavior.in() method - doesn't accept null arguments")
    behaviors.find { x => x.name == name } != None
  }

  //  def in(ds: DistributedSystem): Boolean = {
  //    require(ds != null, "Behavior.in(ds) method - doesn't accept null arguments")
  //    ds.behaviors.values.find { x => x.name == name } != None
  //  }

  def setAgent(a: Agent): Unit = {
    this.a = a
    reactions.map { x => x._2.setAgent(a) }
  }

  def resetActions: Unit = {
    val actions = reactions.values.map { x => x.reset }
  }

  def traceCopy: Behavior = {
    Behavior.fromJson(toJson)
  }

}

object Behavior {
  def fromJson(js: JValue): Behavior = {
    implicit val formats = DefaultFormats
    //    js.extract[Behavior]
    val name = js \ "Behavior" \ "name" match {
      case JString(x) => x
      case _ => throw new Error("Behavior.fromJson - can't extract 'name'")
    }

    val thisDropping = js \ "Behavior" \ "dropping" match {
      case JBool(x) => x
      case _ => throw new Error("Behavior.fromJson - can't extract 'dropping'")
    }

    val b = new Behavior(name, thisDropping)

    b.a = null

    val jsReactions = js \ "Behavior" \ "reactions"
    // DEBUG
    //      println("\n\n"+ pretty(render((jsReactions \ "message")(0))) +"\n\n")
    //    println("\n\n" + jsReactions + "\n\n")

    var listOfReactionPairs = Seq[JValue]()
    jsReactions match {
      case JArray(x) => x map {
        y => listOfReactionPairs = listOfReactionPairs :+ y
        // DEBUG
        //        x => println(x + "\n\n")
      }
      case _ => throw new Error("Behavior.fromJson - can't extract reaction pairs")
    }

    //    if (listOfReactionPairs.isEmpty)
    //      throw new Error("HOW?!")

    listOfReactionPairs map { x =>
      x match {
        case JObject(List(JField("message", y), JField("action", z))) =>
          //        println("MSG = "+x)
          //        println("ACT = "+y)
          b += (Message.fromJson(y), Action.fromJson(z))
        case _ => throw new Error("Behavior.fromJson - What a disaster! couldn't match reaction pair!")
      }
    }

    b
  }
}
