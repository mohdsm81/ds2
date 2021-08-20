package benchmarks.erroneosregister

import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem, LocalState}
import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile}

import java.io.File


object ErrDistRegisterDS2System {
  def getInstance(numOfAgents: Int = 2, harnessFilePath: String = "./dist.new.err.reg.harness.txt", log: Boolean = false): (DistributedSystem, File) = {
    val systemName = "Erroneous Distributed Register"
    val ds = new DistributedSystem(systemName)

    val agentName = "register"
    (1 to numOfAgents) foreach { id =>
      ds + createAgent(agentName + id, numOfAgents - 1, if (id == 1) true else false, log)
    }

    ds.refresh
    (ds, createHarnessFile(harnessFilePath, numOfAgents))
  }

  private def createAgent(name: String, peersCount: Int, leader: Boolean = false, log: Boolean = false): Agent = new ErrDistRegDS2Agent( name, peersCount, leader, log )

  private def createHarnessFile(filePath: String, numOfAgents: Int): File = {
    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.deleteFile
    val nl = "\n"

    if (new File(filePath).exists()) deleteFile(filePath)

    appendToFile(filePath,
      s"register${LocalState.DELIM}mutable.Map[Any,Any]", "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(filePath, "") // newline
    appendToFile(filePath, """register\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(filePath, "") // newline
    appendSeqToFile(filePath, (1 to numOfAgents).map { x => s"register$x" }) //ds.agents.filterNot(_.name.startsWith("IRed")).map { x => x + ", " }.toSeq)
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      s"write, Write, k, 1", // add operations here (harness)
      s"read, Read, k",
      s"read, Read, k",
      s"write, Write, k, 2",
      s"read, Read, k",
      s"read, Read, k")
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      "read" + ", " + "ReadAck", // add response messages regexes
      "write" + ", " + "WriteAck")
    appendToFile(filePath, "") // newline
    appendToFile(filePath,
      "Write , 0, w",
      "WriteReplica , 0, w",
      "WriteReplicaAck , 0, w", // for this implementation, by the time replica receives the writeReplica, written the state, only then the ack flies (won't affect/interfere-with the state in the leader either)
//      "WriteAck , 0, w",
      "Read , 0, r",
      "ReadReplica , 0, r",
      "ReadReplicaAck , 0, r"
//      "ReadAck , 0, r"
    ) // newline

    new File(filePath)
  }
}
