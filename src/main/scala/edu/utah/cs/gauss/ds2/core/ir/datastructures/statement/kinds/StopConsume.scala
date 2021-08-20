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
@SerialVersionUID(1116)
class StopConsume extends Statement with
  StoppingConsuming with JsonSerializable with
  TraceCopying[StopConsume] with
  Printable {

  override def generateDynamic: StopConsume = {
    generateStatic
  }

  override def generateStatic: StopConsume = {
    code = (m: Message, a: Agent) => { ds.stopConsuming(a) }
    this
  }

  override def toJson: JValue = {
    ("StopConsume" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: StopConsume = {
    StopConsume.fromJson(toJson)
  }

  override def toString: String = {
    val name = a match{
      case null => null
      case x:Agent => x.name
    }

    s"RESUME_CONSUME ---> ${name}"
  }
}

object StopConsume extends JsonDeSerializable[StopConsume] {

  def apply: StopConsume = new StopConsume

  def fromJson(js: JValue): StopConsume = {
    val newOne = new StopConsume
    Statement.fromJson(js \ "StopConsume" \ "Statement").assignAttributesTo(newOne)

    newOne
  }
}
