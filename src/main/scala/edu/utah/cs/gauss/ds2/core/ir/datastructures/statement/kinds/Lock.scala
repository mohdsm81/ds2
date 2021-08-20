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
@SerialVersionUID(1126)
class Lock extends Statement with Locking with JsonSerializable with TraceCopying[Lock] with Printable {

  override def generateDynamic: Lock = {
    generateStatic
  }

  override def generateStatic: Lock = {
    // just care for one thing at a time
    //    subjectAgentName = subjectAgent.name
    code = (m: Message, a: Agent) => {
      ds.lock(a)
    }
    this
  }

  override def toJson: JValue = {
    ("Lock" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: Lock = {
    import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{Lock => LockStmt}
    LockStmt.fromJson(toJson)
  }

  override def toString: String = {
    val agentNme = a match{
      case null => null
      case x:Agent => x.name
    }

    s"LOCK ---> ${agentNme}"
  }
}

object Lock extends JsonDeSerializable[Lock] {

  def apply: Lock = new Lock

  def fromJson(js: JValue): Lock = {
    val newOne = new Lock
    Statement.fromJson(js \ "Lock" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
