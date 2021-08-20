package benchmarks.paxos

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem, LocalState }
import edu.utah.cs.gauss.serialization.IO._

import java.io.File

/**
 * Gives an instance of MultiPaxos as a DistributedSystem with the same number of agents provided as an argument.
 *
 * @author Mohammed S. Al-Mahfoudh
 *         based on Zepeng Zhao's implementation
 *
 */
object PaxosDS2System {
  val PAXOS = "paxos"

  def getInstance(numOfAgents: Int = 3, harnessFileToWritePath: String, log: Boolean = false): (DistributedSystem, File) = {
    val ds = new DistributedSystem("Multi Paxos")
    (1 to numOfAgents) foreach { id => ds + makeAgent(id.toString, numOfAgents, log) }
    ds.refresh
    (ds, makeHarnessFile(harnessFileToWritePath, ds))
  } // DistributedSystem Instance Function

  private def makeAgent(id: String, numOfAgentsInSystem: Int, log: Boolean = false): Agent = new PaxosDS2Agent2(id,numOfAgentsInSystem, log)

  private def makeHarnessFile(filePath: String, ds: DistributedSystem): File = {

    // produce harness file

    if (new File(filePath).exists()) deleteFile(filePath)

    val SPEC = s"spec${LocalState.DELIM}mutable.Map[Any,Any]"

    val writeMsgName = "ProposeCommand"
    val writeAckMsgName = "ClientWriteResponse"
    val readMsgName = "Request"
    val readAckMsgName = "ClientReadResponse"
    val ired = new Agent("IRed")

    appendToFile(filePath,
      SPEC, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(filePath, "") // newline
    appendToFile(filePath, """\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(filePath, "") // newline
    appendSeqToFile(filePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x =>
      x + ", "
    }.toSeq)
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      s"write, $writeMsgName, k1, 10", // add operations here (harness)
      s"read, $readMsgName, k1",
      s"read, $readMsgName, k1",
      s"write, $writeMsgName, k2, 20",
      s"read, $readMsgName, k2",
      s"read, $readMsgName, k2")
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      "read" + ", " + readAckMsgName, // add response messages regexes
      "write" + ", " + writeAckMsgName)
    appendToFile(filePath, "") // newline
    /*
    This part is special to LiViola, it focuses on shuffling what is mentioned here and leaves others to happen in
    whatever order they happen in, so be VERY CAREFUL what you put/leave-out of this. Other schedules simply ignore
    this part.
     */
    appendToFile(filePath,
      // write-transaction-related messaging
//    "ProposeCommand, 0, w", // write only // the propose it sends will get shuffled (it maps one-to-one with Propose)
//    "Request, 0, r", // read only // same as ProposeCommand, maps one-to-one with "Prepare" message
    "Propose, 0, b", // read and write
    "Prepare, 1, b", // read and write
    "Promise, 1, b", // read and write
    "Nack, 2, b",  // read and write
    "Accept, 1, w", // write only
    "Accepted, 1, w" // write only
    ) // newline

    new File(filePath)
  }

} // PaxosSystem