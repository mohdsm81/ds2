//package benchmarks.openchord.data
//
///**
// * This is a utility to generate the appropriate payloads and both serialize
// * and de-serialize to be able to use them in the harness.
// *
// * @author <br>
// *         Mohammed S. Al-Mahfoudh <br/>
// *         mahfoudh@cs.utah.edu <br/>
// *         SoC - Gauss Group <br/>
// */
//object HarnessPayloadSerialization {
//  var counter = 0
//
//  def createSerializedEntry(k: String, v: Serializable): List[Byte] = {
//    ???
//  }
//
//  def createSerializedRequest(k: String, reqID: String): List[Byte] = {
//    ???
//  }
//
//  def deSerializeEntryToPayload(entryAsBytes: List[Byte]): List[Byte] = {
//    ???
//  }
//
//  def deSerializeRequestToPayload(requestAsBytes: List[Byte]) = {
//    ???
//  }
//
//  /**
//   * This is Zepeng's implementation i am using it as is but with easier/auto counter.
//   * @return a string representing the read request id.
//   */
//  private def makeRequestID():String = {
//    val reqID = System.currentTimeMillis().toString() +"_"+counter
//    counter += 1
//    reqID
//  }
//
//  def main(args: Array[String]): Unit = {
//    /*
//     in total we have 2 writes and two reads: 2 reads and 1 write to operate on k1, k2
//     for the second write. This is to show the least (but significant)  advantage of
//     LiViola over others.
//
//     NOTES:
//     - for write Entry.id use HashFunction.createID(key:String)
//     - for reads
//     */
//
//    val k1 = "k1"
//    val entryID1 = HashFunction.createID(k1.getBytes()) // one write to use this
//    val value1 = 10
//
//    val k2 = "k2"
//    val entryID2 = HashFunction.createID(k2.getBytes()) // the other write to use this
//    val value2 = 20
//
//    val lookupID1 = HashFunction.createID(k1.getBytes()) // two reads will use this one
//
//    val read1RequestID = makeRequestID()
//
//    println("write entry1 bytes: ") // can only be added as array of bytes
//    println("write entry2 bytes: ")
//    println("read request1 bytes: ")
//    println("read request2 bytes: ")
//
//    /*
//     then we copy each and add it to the harness as a list of bytes in the payload in
//     UploadEntry(e:Entry) and ThisRequest(req:Request).
//     */
//  }
//}
