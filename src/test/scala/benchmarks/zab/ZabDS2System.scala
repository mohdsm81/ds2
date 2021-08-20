package benchmarks.zab

import java.io.File
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem}
import edu.utah.cs.gauss.serialization.IO._

object ZabDS2System {
  def getInstance(numOfAgents: Int = 3, harnessFilePath: String, log: Boolean = false): (DistributedSystem, File) = {
    val systemName = "Zab"
    val ds = new DistributedSystem(systemName)

    (1 to numOfAgents) foreach { id =>
      ds + createAgent(id.toString)
    }

    ds.refresh
    (ds, makeHarnessFile(harnessFilePath, ds))
  }
  private def createAgent(agentName: String = "zab"): Agent = new ZabDS2Agent(agentName)
  private def makeHarnessFile(filePath: String, ds: DistributedSystem): File = {

    // produce harness file

    if (new File(filePath).exists()) deleteFile(filePath)

    //    val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"
    //
    //    val writeMsgName = "UploadEntry"
    //    val readMsgName = "Request"
    //    val writeAckMsgName = "ClientWriteResponse"
    //    val readAckMsgName = "ClientReadResponse"
    //    val ired = new Agent("IRed")
    //
    //    appendToFile(filePath,
    //      SPEC, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    //    appendToFile(filePath, "") // newline
    //    appendToFile(filePath, """\d+""") // regex for ADT agents (i.e. the cluster)
    //    appendToFile(filePath, "") // newline
    //    appendSeqToFile(filePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x =>
    //      if(x.name == "1") x + ", "
    //      else x + s", ${x.name.toInt - 1}"
    //    }.toSeq)
    //    appendToFile(filePath, "") // newline
    //    appendToFile(filePath,
    //      s"write, $writeMsgName, k1, 10", // add operations here (harness)
    //      s"read, $readMsgName, k1",
    //      s"read, $readMsgName, k1",
    //      s"write, $writeMsgName, k2, 20",
    //      s"read, $readMsgName, k2",
    //      s"read, $readMsgName, k2")
    //    appendToFile(filePath, "") // newline
    //    appendToFile(filePath,
    //      readMsgName + ", " + readAckMsgName, // add response messages regexes
    //      writeMsgName + ", " + writeAckMsgName)
    //    appendToFile(filePath, "") // newline
    //    /*
    //    This part is special to LiViola, it focuses on shuffling what is mentioned here and leaves others to happen in
    //    whatever order they happen in, so be VERY CAREFUL what you put/leave-out of this. Other schedules simply ignore
    //    this part.
    //     */
    //    appendToFile(filePath,
    //      // write-transaction-related messaging
    //      "UploadEntry, 0, w",
    //      "UploadEntryResponse, 0, w",
    //      // read-transaction-related messaging
    //      "Request, 0, r",
    //      "Response, 0, r"
    //    ) // newline

    new File(filePath)
  }
}
