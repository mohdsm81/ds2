package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{ Agent, DistributedSystem, Message }
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic.DebuggerScheduler

import java.io.File

class DebuggerchedulerSpec extends MyTestSpecs {

  test("First test") {

    val sample: (DistributedSystem, File) =
      SampleDistributedSystems.sampleLinearizableNAgentsRegisterSTRICT_R_Retries_AND_NO_cache_TAINTED("./harness.debugger.1.txt", 2)
    val ds  = sample._1
    ds + Agent("IRed0") + Agent("IRed1") // one per request/invocation

    // the schedule to be manually constructed
//      READ:	 IRed1 --> dude1	 (read--24EE -- 0)
//      SEND:	 dude1 --> dude0	 (msgReadReplica--24EE -- 0)

//      WRITE:	 IRed0 --> dude0	 (write--EE12 -- 0)
//      SEND:	 dude0 --> dude1	 (replication--EE12 -- 1)

//      REC:	 dude1 <-- dude0	 (replication--EE12)
//      REPLY:	 dude0 <-- dude1	 (replicationAck--EE12 -- 1)

//      REC:	 dude0 <-- dude1	 (replicationAck--EE12)
//      W_RES:	 IRed0 <-- dude0	 (writeAck--EE12 -- 1)

//      REC:	 dude0 <-- dude1	 (msgReadReplica--24EE)
//      REPLY:	 dude1 <-- dude0	 (msgReadReplicaAck--24EE -- 1)

//      REC:	 dude1 <-- dude0	 (msgReadReplicaAck--24EE)
//      SEND:	 dude1 --> dude0	 (msgReadReplica--24EE -- 0)

    // execution stops here where dude1 sends W_RES to client!

//      REC:	 dude0 <-- dude1	 (msgReadReplica--24EE)
//      REPLY:	 dude1 <-- dude0	 (msgReadReplicaAck--24EE -- 1)

//      REC:	 dude1 <-- dude0	 (msgReadReplicaAck--24EE)
//      SEND:	 dude1 --> dude0	 (msgReadReplica--24EE -- 0)

    val m1 = new Message("read") // 1
    val m2 = new Message("write") // 0
    val m3 = new Message("replication") // 1
    val m4 = new Message("replicationAck") // 0
    val m5 = new Message("msgReadReplica") // 0
    val m6 = new Message("msgReadReplicaAck") // 1

    val r1 = (m1 ,sample._1.get("dude1"))
    val r2 = (m2 ,sample._1.get("dude0"))
    val r3 = (m3 ,sample._1.get("dude1"))
    val r4 = (m4 ,sample._1.get("dude0"))
    val r5 = (m5 ,sample._1.get("dude0"))
    val r6 = (m6 ,sample._1.get("dude1"))

    val schedule = Seq(r1, r2, r3, r4, r5, r6)

    val sch = new DebuggerScheduler(sample._2.getPath, sample._1, schedule)

    sch.explore

    println("Hello there, breakpoint!")
  }
}
