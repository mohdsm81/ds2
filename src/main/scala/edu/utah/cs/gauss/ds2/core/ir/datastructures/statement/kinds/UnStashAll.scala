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
@SerialVersionUID(1111)
class UnStashAll extends Statement with
  UnStashingAll with JsonSerializable with
  TraceCopying[UnStashAll] with
  Printable {

  override def generateDynamic: UnStashAll = {
    generateStatic
  }

  override def generateStatic: UnStashAll = {
    code = (m: Message, a: Agent) => { ds.unstashAll(a) }
    this
  }

  override def toJson: JValue = {
    ("UnStashAll" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: UnStashAll = {
    UnStashAll.fromJson(toJson)
  }

  override def toString: String = {
    val agentName = a match{
      case null => null
      case x:Agent => x.name
    }

//    val toUnstashMsgName = if (!a.stash.isEmpty) a.stash.head.name else "nothing"

    s"UNSTASH_ALL ---> ${agentName} to unstashall messages"
  }

}

object UnStashAll extends JsonDeSerializable[UnStashAll] {
  def apply: UnStashAll = new UnStashAll

  def fromJson(js: JValue): UnStashAll = {
    val newOne = new UnStashAll
    Statement.fromJson(js \ "UnStashAll" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
