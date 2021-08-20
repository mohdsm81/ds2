package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
import edu.utah.cs.gauss.ds2.core.ir.features.Printable
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */
@SerialVersionUID(1112)
class UnStash extends Statement with
  UnStashing with JsonSerializable with
  TraceCopying[UnStash] with
  Printable {

  override def generateDynamic: UnStash = {
    generateStatic
  }

  override def generateStatic: UnStash = {
    code = (m: Message, a: Agent) => { ds.unstash(a) }
    this
  }

  override def toJson: JValue = {
    ("UnStash" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: UnStash = {
    UnStash.fromJson(toJson)
  }

  override def toString: String = {
    val agentName = a match{
      case null => null
      case x:Agent => x.name
    }

    val toUnstashMsgName = if (!a.stash.isEmpty) a.stash.head.name else "nothing"

    s"UNSTASH ---> ${agentName} to unstash ${toUnstashMsgName}"
  }
}

object UnStash extends JsonDeSerializable[UnStash] {

  def apply: UnStash = new UnStash

  def fromJson(js: JValue): UnStash = {
    val newOne = new UnStash
    Statement.fromJson(js \ "UnStash" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
