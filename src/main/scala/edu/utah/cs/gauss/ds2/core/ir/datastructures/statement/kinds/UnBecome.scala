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
@SerialVersionUID(1114)
class UnBecome extends Statement with
  UnBecoming with JsonSerializable with
  TraceCopying[UnBecome] with
  Printable {

  override def generateDynamic: UnBecome = {
    generateStatic
  }

  override def generateStatic: UnBecome = {
    code = (m: Message, a: Agent) => { ds.unbecome(a) }
    this
  }
  
  override def toJson: JValue = {
    ("UnBecome" -> 
       ("Statement" -> super.toJson))
  }

  override def traceCopy: UnBecome = {
    UnBecome.fromJson(toJson)
  }

  override def toString: String = {
    val agentNme = a.name
    val behavior = if(null != a && !a.oldBehaviors.isEmpty) a.oldBehaviors.head else "default"
    
    s"UNBECOME ---> ${agentNme} to ${behavior}"
  }
}

object UnBecome extends JsonDeSerializable[UnBecome] {
  def apply: UnBecome = new UnBecome

  def fromJson(js: JValue): UnBecome = {
    val newOne = new UnBecome
    Statement.fromJson(js \ "UnBecome" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
