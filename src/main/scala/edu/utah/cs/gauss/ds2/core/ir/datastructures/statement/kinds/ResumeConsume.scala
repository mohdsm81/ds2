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
@SerialVersionUID(1121)
class ResumeConsume extends Statement with
  ResumingConsuming with JsonSerializable with
  TraceCopying[ResumeConsume] with
  Printable {

  override def generateDynamic: ResumeConsume = {
    generateStatic
  }

  override def generateStatic: ResumeConsume = {
    code = (m: Message, a: Agent) => { ds.resumeConsuming(a) }
    this
  }

  override def toJson: JValue = {
    ("ResumeConsume" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: ResumeConsume = {
    ResumeConsume.fromJson(toJson)
  }

  override def toString: String = {
    val subjectAgentName = a match{
      case null => null
      case x:Agent => x.name
    }

    s"RESUME_CONSUME ---> ${subjectAgentName}"
  }
}

object ResumeConsume extends JsonDeSerializable[ResumeConsume] {
  def apply: ResumeConsume = new ResumeConsume

  def fromJson(js: JValue): ResumeConsume = {
    val newOne = new ResumeConsume
    Statement.fromJson(js \ "ResumeConsume" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
