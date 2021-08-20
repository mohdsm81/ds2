package edu.utah.cs.gauss.ds2.core.ir.datastructures

import net.liftweb.json._
import net.liftweb.json.JsonAST.JInt
import net.liftweb.json.JsonDSL.{boolean2jvalue, jobject2assoc, list2jvalue, pair2Assoc, pair2jvalue, string2jvalue}

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 */

@SerialVersionUID(700)
class Message(
    var name: String,
    var sender: Agent,
    var sendMethod: Boolean,
    private var payyload: Seq[Any] = Seq()) extends Serializable {

  def payload_=(pl: Seq[Any]): Unit = payyload = pl
  def payload[T](index: Int): T = payyload(index).asInstanceOf[T]
  def payload: Seq[Any] = payyload
  //==============================
  // Instrumentation fields
  //==============================
  val forward = "FORWARD"
  val backward = "BACKWARD"
  var isResponse = false
  var direction: String = forward
  var id: BigInt = 0
  //==============================
  // utility
  //==============================
  // we may not need it, but leave it for now (I thought we needed it)
  def assignAttributesTo(dstMsg: Message): Unit = {
    dstMsg.name = name
    dstMsg.sender = sender
    dstMsg.sendMethod = sendMethod
    dstMsg.payload = payload
    dstMsg.isResponse = isResponse
    dstMsg.direction = direction
    dstMsg.id = id
  }

  //==============================
  // Equality handling
  //==============================
//   override def hashCode: Int = {
//     //    refreshSenderName
//     // then check
// //    val sndr: String =
// //      if (null != sender) sender.name
// //      else if (senderName != null) senderName // do nothing
// //      else null
// //
// //    val senderNameHash = if (null == sndr) 0 else sndr.hashCode

//     name.hashCode

//     // causes problems when trying to retrieve a reaction
//     //      sendMethod.hashCode +
//     //      payload.hashCode
//     //      senderNameHash
//   }
  
  override def hashCode( ): Int = {
    val senderHash: Int = sender match{
      case null => 0
      case x: Agent => x.hashCode
    }
    name.hashCode +
      senderHash +
      payyload.hashCode()
  }

  override def equals(that: Any): Boolean = {
    hashCode == that.hashCode
  }
  //==============================

  def this() = {
    this("Message", null, false, Seq[Any]())
  }

  def this(name: String) {
    this(name, null, false, Seq[Any]())
  }
  def this(payload: Seq[Any]) {
    this("Message", null, false, payload)
  }

  def this(name: String, payload: Seq[Any]) {
    this(name, null, false, payload)
  }

  def this(src: Agent, sendMethod: Boolean, dst: Agent) {
    this("Message", src, sendMethod, Seq[Any]())
  }

  def this(src: Agent) {
    this("Message", src, false, Seq[Any]())
  }

  def toJson: JValue = {
    val sndr: String = sender match{
      case null => null
      case x:Agent => x.name
    }

    ("Message" ->
       ("name" -> name) ~
       ("sender" -> JNothing) ~ // as long as we have sender name, we can retrieve it after deserializing all the DS
       ("senderName" -> sndr) ~ // determined at run time after it is sent
       ("sendMethod" -> sendMethod) ~
       ("payload" -> List()) ~
       ("isResponse" -> isResponse) ~
       ("direction" -> direction) ~
       ("id" ->  JInt(id)))
  }

  def traceCopy: Message = {
    Message.fromJson(toJson)
  }

  def in(behavior: Behavior): Boolean = {
    require(null != behavior, "Behavior.in method doesn't accept null arguments")
    val setOfMessages = behavior.reactions.keySet
    setOfMessages.find { x => x.name == name } != None
  }
  def in(messages: Set[Message]): Boolean = {
    require(null != messages, "Message.in(Set[messages]) method - doesn't accept null arguments")
    messages.find { x => x.name == name } != None
  }
  def in(messages: Seq[Message]): Boolean = {
    require(null != messages, "Message.in(Seq[messages]) method - doesn't accept null arguments")
    messages.find { x => x.name == name } != None
  }

//  def in(ds: DistributedSystem): Boolean = {
//    require(null != ds, "Message.in(DS) method - doesn't accept null arguments")
//
//    ds.messages.find { x => x.name == name } != None
//  }

  def is(that: Message): Boolean = {

    val sameSenderCond = sender == that.sender

    sameSenderCond &&
      sendMethod == that.sendMethod &&
      payload == that.payload
  }

  def setSender(a: Agent): Unit = {
    require(null != a, "Message.setSender - argument can't be null")
    sender = a
  }

  override def toString: String = {

    val sndr: String = sender match{
      case null => null
      case x:Agent => x.name
    }
    
    name + "( sender = " + sndr + ", method = " + sendMethod + s", payload: ${payyload}" + ")"

  }

  def copy: Message = {
    new Message(name,sender,sendMethod,payload)
  }
}
object Message {
  def fromJson(js: JValue): Message = {
    implicit val formats = DefaultFormats
    //    js.extract[Message]
    var msg = new Message

    js \\ "name" match {
      case JString(x) =>
        x match {
          case "Start"      => msg = new Start
          case "Stop"       => msg = new Stop
          case "Join"       => msg = new Join
          case "ReJoin"     => msg = new ReJoin
          case "Leave"      => msg = new Leave
          case "Demise"     => msg = new Demise
          case "LinkFailed" => msg = new LinkFailed
          case "PoisonPill" => msg = new PoisonPill
          case "ResolveDummyFuture" =>
            val f = js \\ "payload"
            msg = new ResolveDummyFuture(DummyFuture.fromJson(f(0)))
          case _ => msg = new Message; msg.name = x // Message
        }
      case _ => throw new Error(msg.getClass.getSimpleName + " - there can't be a message without a name!")
    }

    val mname = msg.name

    //    println(msg.getClass.getSimpleName)

    val msender = null

    //    val msendMethod = js \ msg.getClass.getSimpleName \ "sendMethod" match {

    val msendMethod = js \\ "sendMethod" match {
      case JBool(x) => x
      case _        => throw new Error(msg.getClass.getSimpleName + " - couldn't extract 'sendMethod'")
    }

    val isResponse = js \\ "isResponse" match{
      case JBool(x) => x
      case _ => throw new Error(msg.getClass.getSimpleName + " - couldn't extract 'isResponse'")
    }

    val direction = js \\ "direction" match {
      case JString(x) => x
      case _ => throw new Error(msg.getClass.getSimpleName + " - couldn't extract 'direction'")
    }

    val id = js \\ "id" match {
      case JInt(x) => x
      case _ => throw new Error(msg.getClass.getSimpleName + " - couldn't extract 'id'")
    }

    // payload is empty Seq[Any]()

    msg.name = mname
    msg.payload = Seq[Any]()
    msg.sender = msender
    msg.sendMethod = msendMethod
    msg.isResponse = isResponse
    msg.direction = direction
    msg.id = id

    msg
  }
}

/**
 * Means do the necessary (e.g. initialization, ...etc) to be a functional agent in the party.
 */
case class Start() extends Message() { name = getClass.getSimpleName }

/**
 * Means complete the work (in the queue) you have been assigned after locking your queue
 */
case class Stop() extends Message() { name = getClass.getSimpleName }

/**
 * Join the party
 */
case class Join() extends Message() { name = getClass.getSimpleName }

/**
 * Pick from where you disappeared after joining the party then resume work.
 */
case class ReJoin() extends Message() { name = getClass.getSimpleName }

/**
 * Means do the necessary "communications" to leave the party (kind of inverse of Join)
 */
case class Leave() extends Message() { name = getClass.getSimpleName }

/**
 * Quit abruptly without even doing the necessary work and/or communication.
 */
case class Demise() extends Message() { name = getClass.getSimpleName }

/**
 * Simulated link failure (i.e. two communicating agents can't talk to each other, both queues are in "locked" state.
 */
case class LinkFailed() extends Message() { name = getClass.getSimpleName }

/**
 * The special killer (i.e. sent by killer to to-be-killed agent) message.
 */
case class PoisonPill() extends Message() { name = getClass.getSimpleName }

/**
 * The special message for resolving a future, that is the "reply" for an ask call.
 * Note that the DummyFuture is itself the lone element in the payload<code>payload</code>
 * in the message.
 */
case class ResolveDummyFuture(df: DummyFuture) extends Message(Seq(df)) { name = "ResolveDummyFuture" }

// the commented class isn't needed, the asker agent creates a future (assuming the remote one will resolve it). Later,
// the remote asked agent may/not resolve the future (this way is much simpler than 3-way hand shake, this is only two 
// way handshake from the start of the ask till the end). This agrees more with our OpSem.

///**
// * The special message for promising a future, this is the initial promise before a future is resolved.
// * That is, this is the future returned by the asked agent.
// *
// */
//case class PromiseDummyFuture(df: DummyFuture) extends Message(Seq(df))
