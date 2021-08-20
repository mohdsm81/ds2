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
@SerialVersionUID(1113)
class UnLock extends Statement with
  UnLocking with JsonSerializable with
  TraceCopying[UnLock] with
  Printable {

  override def generateDynamic: UnLock = {
    generateStatic
  }

  override def generateStatic: UnLock = {
    code = (m: Message, a: Agent) => {
      ds.unlock(a)
    }
    this
  }
  
  override def toJson: JValue = {
    ("UnLock" ->
      ("Statement" -> super.toJson))
  }

  override def traceCopy: UnLock = {
    import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{UnLock => UnLockStmt}
    UnLockStmt.fromJson(toJson)
  }

  override def toString: String = {
    val subjectAgentName = a match{
      case null => null
      case x: Agent => x.name
    }
    
    s"UNLOCK ---> ${subjectAgentName}"
  }
}

object UnLock extends JsonDeSerializable[UnLock] {

  def apply: UnLock = new UnLock

  def fromJson(js: JValue): UnLock = {
    val newOne = new UnLock
    Statement.fromJson(js \ "UnLock" \ "Statement").assignAttributesTo(newOne)
    newOne
  }
}
