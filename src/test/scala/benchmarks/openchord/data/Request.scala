package benchmarks.openchord.data

/**
 * this class represents a request command that a user can send from a node with a key to look up the corresponding 
 * value from the DHT system.
 * val reqNode stands for the node that is requesting the value providing the requested key  and requested ID (reqID)
 * @author zepeng zhao
 *         with modification by Mohammed S. Al-Mahfoudh
 */
//  case class Request(node:Node, idString:String, key:ID, originalKey: String = "")