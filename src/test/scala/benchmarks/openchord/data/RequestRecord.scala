package benchmarks.openchord.data


import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent

object OperationType extends Enumeration{
  type OperationType = Value
  val READ, WRITE, NONE = Value
}
case class RequestRecord(client: Agent, key: String, value: Any = null)
