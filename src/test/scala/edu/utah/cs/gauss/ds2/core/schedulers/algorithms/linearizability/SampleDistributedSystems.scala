package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability

import java.io.File
import java.util.UUID

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._

object SampleDistributedSystems {

  // NOTE: I wrote lengthy descriptive identifier names in order to make reading the model easier w/o documentation

  def sampleLinearizableNAgentsRegisterSTRICT(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {
    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Any)]"
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksValue(m: Message, a: Agent): Any = {
      a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old))
    }

    def setReadAcksValue(m: Message, a: Agent, value: Any): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (old, value))
    }

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents - 1
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = getReplicaWriteAcksCount(m, a)
        reachedCount >= a.localState[Int](peersCountVar)
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, (Int, Any)]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        a.localState(readsAcksMapVar) = a.localState[Map[String, (Int, Any)]](readsAcksMapVar) + (m.sender.name -> (0, getRegisterVal(a)))
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement { (m: Message, a: Agent) => containsServerValueOrCreateEntry(m, a) }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, (Int, Any)]](readsAcksMapVar)
        theAcksMap + (m.sender.name -> (1, getRegisterVal(a))) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement {
        (m: Message, a: Agent) => {
          a.localState(iterAgentsVar) = ds.agents.filter { x =>
            !x.name.startsWith(ired.name.trim) &&
              x.name != a.name
          }.iterator
        } // code(m,a)
      } // statement
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ m.sender.name
        msg
      })
      // send to all agents, only need to accumulate majority vote
      val whileHasMoreAgentsSendReadReplicaMessages = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar)
      ) // end while

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        whileHasMoreAgentsSendReadReplicaMessages

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent
      val sameValue = (m: Message, a: Agent) => {
        m.payload[Any](0) ==
          getReplicaReadAcksValue(m, a) //a.localState[Map[Any, Any]](registerVar)(registerKey)
      }

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      val reachedMajorityRead = (m: Message, a: Agent) => {
        val localValue: Any = getReplicaReadAcksValue(m, a) //a.localState[Map[Any, Any]](registerVar)(registerKey)

        var reachedCount: Int = getReplicaReadAcksCount(m, a) //a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)

        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val

        reachedCount >= a.localState[Int](peersCountVar)
      }

      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, (Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
      }

      val ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount = If(sameValue) { // increment counter by one
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          val theAcksMap = a.localState[Map[String, (Int, Any)]](readsAcksMapVar)
          val client = m.payload.last.toString
          theAcksMap + (client -> (getReplicaReadAcksCount(m, a) + 1, getReplicaReadAcksValue(m, a)))
        })
      }

      val ifMajorityReachedSendReadAckToClientAndRemoveCount = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, getReplicaReadAcksValue(m, a)) //a.localState[Map[Any, Any]](registerVar)(registerKey))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
          a.localState[Map[String, Int]](readsAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientIsInReadsAcksMapThenCheckIfMajorityReached = If(readsAcksMapContainsCount)(
        ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount,
        ifMajorityReachedSendReadAckToClientAndRemoveCount
      )

      readReplicaAckAction +
        ifClientIsInReadsAcksMapThenCheckIfMajorityReached

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_W_Retries(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {
    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Any,Int,Int)]" // (count, value, round, received-same-value-count)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val roundReadAcksReceived = s"roundReadAcksReceived${LocalState.DELIM}Int"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksValue(m: Message, a: Agent): Any = {
      a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def isSameRound(m: Message, a: Agent): Boolean = {
      m.payload[Int](1) == a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._3
    }

    def incrementSameValueCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3, oldTuple._4 + 1))
    }

    def incrementReceivedAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, 0))
    }

    def getSameValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(clientID)._4
    }

    def getAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(clientID)._1
    }

    def resetAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    /**
     * Increments the round number associated with the client ID and stored in the agent's local state
     *
     * @param a        the agent with the local state containing the round associated with the client ID
     * @param clientID the specific client ID in the local state
     * @return the new round number, after incrementing it
     */
    def incrementRound(a: Agent, clientID: String): Int = {
      val theMap = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._3 + 1
      val newTuple = (tuple._1, tuple._2, newRound, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksValue(m: Message, a: Agent, value: Any): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.payload.last.toString -> (old._1, value, old._3, old._4))
    }

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Any, Int, Int)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = getReplicaWriteAcksCount(m, a)
        reachedCount >= a.localState[Int](peersCountVar)
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        a.localState(readsAcksMapVar) = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.sender.name -> (0, getRegisterVal(a), 0, 0))
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement {
        containsServerValueOrCreateEntry
      }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
        val oldTuple = theAcksMap(m.sender.name)
        theAcksMap + (m.sender.name -> (1, getRegisterVal(a), oldTuple._3, 1)) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement {
        (m: Message, a: Agent) => {
          a.localState(iterAgentsVar) = ds.agents.filter { x =>
            !x.name.startsWith(ired.name.trim) &&
              x.name != a.name
          }.iterator
        } // code(m,a)
      } // statement
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.sender.name)._3 :+ m.sender.name
        msg
      })

      val broadcastReadReplicaRequests = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar))

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        broadcastReadReplicaRequests

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload[Int](1), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      val sameValueAndSameRound = (m: Message, a: Agent) => {
        m.payload[Any](0) == getReplicaReadAcksValue(m, a) && isSameRound(m, a)
      }
      val reachedMajorityRead = (m: Message, a: Agent) => {
        val localValue: Any = getReplicaReadAcksValue(m, a)
        var reachedCount: Int = getSameValueCount(m, a) //a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)
        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val
        reachedCount == a.localState[Int](peersCountVar)
      }
      val noMoreAcksToWaitFor = (m: Message, a: Agent) => {
        getAcksCount(m, a) == a.localState[Int](peersCountVar)
      }
      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar).contains(m.payload.last.toString)
      }


      // statements start here

      val incrementReceivedAcksCnt = Statement {
        incrementReceivedAcksCount
      }

      val ifReceivedReplicaValueIsSameAsCachedValueThenIncrementSameValCount =
        If(sameValueAndSameRound)( // increment counter AND same-value-count by one
          ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
            val clientID = m.payload.last.toString
            val theAcksMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
            val oldTuple = theAcksMap(clientID)
            theAcksMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, oldTuple._4 + 1))
          })
        )

      val removeClientIDFromReadsAcksMap = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
        a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) - m.payload.last.toString
      })

      val ifMajorityReachedSendReadAckToClient = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, getReplicaReadAcksValue(m, a)) //a.localState[Map[Any, Any]](registerVar)(registerKey))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        removeClientIDFromReadsAcksMap
      )

      val ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest =
        If((m: Message, a: Agent) => {
          readsAcksMapContainsCount(m, a) && noMoreAcksToWaitFor(m, a)
        })(
          Statement((m: Message, a: Agent) => incrementRound(a, m.payload.last.toString)),
          Statement(resetAcksCount),
          Statement(incrementReceivedAcksCount), // to indicate I just read it
          Statement(resetSameValueCount),
          Statement(incrementSameValueCount), // to indicate it is the same as local value
          Statement((m: Message, a: Agent) => setReadAcksValue(m, a, getRegisterVal(a))),
          createAgentsIterator,
          While((m: Message, a: Agent) => {
            a.localState[Iterator[Agent]](iterAgentsVar).hasNext
          })(
            ModifyState(dstAgentVar, (_: Message, a: Agent) => {
              a.localState[Iterator[Agent]](iterAgentsVar).next
            }),
            ModifyState(msgToSendVar, (m: Message, a: Agent) => {
              val msg = msgReadReplica.copy
              msg.payload = Seq(registerKey, getRound(m, a), m.payload.last)
              msg
            }),
            Send(msgToSendVar, dstAgentVar)
          )
        )


      readReplicaAckAction + If(readsAcksMapContainsCount)(
        incrementReceivedAcksCnt,
        ifReceivedReplicaValueIsSameAsCachedValueThenIncrementSameValCount,
        ifMajorityReachedSendReadAckToClient,
        ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest)

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_W_R_Retries_no_cache(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int)]" // (count, value, round, received-same-value-count)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val roundReadAcksReceived = s"roundReadAcksReceived${LocalState.DELIM}Int"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._1

    def isSameRound(m: Message, a: Agent): Boolean = {
      m.payload[Int](1) == a.localState.apply[Map[String, (Int, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementSameValueCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1))
    }

    def incrementReceivedAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3))
    }

    def resetSameValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0))
    }

    def getSameValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)(clientID)._3
    }

    def getAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)(clientID)._1
    }

    def resetAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    /**
     * Increments the round number associated with the client ID and stored in the agent's local state
     *
     * @param a        the agent with the local state containing the round associated with the client ID
     * @param clientID the specific client ID in the local state
     * @return the new round number, after incrementing it
     */
    def incrementRound(a: Agent, clientID: String): Int = {
      val theMap = a.localState.apply[Map[String, (Int, Int, Int)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3))
    }

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = getReplicaWriteAcksCount(m, a)
        reachedCount >= a.localState[Int](peersCountVar)
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        a.localState(readsAcksMapVar) = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar) + (m.sender.name -> (0, 0, 0))
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement {
        containsServerValueOrCreateEntry
      }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)
        val oldTuple = theAcksMap(m.sender.name)
        theAcksMap + (m.sender.name -> (1, oldTuple._2, 1)) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement {
        (m: Message, a: Agent) => {
          a.localState(iterAgentsVar) = ds.agents.filter { x =>
            !x.name.startsWith(ired.name.trim) &&
              x.name != a.name
          }.iterator
        } // code(m,a)
      } // statement
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)(m.sender.name)._2 :+ m.sender.name
        msg
      })

      val broadcastReadReplicaRequests = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar))

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        broadcastReadReplicaRequests

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload[Int](1), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      val sameValueAndSameRound = (m: Message, a: Agent) => {
        m.payload[Any](0) == getRegisterVal(a) && isSameRound(m, a)
      }
      val reachedMajorityRead = (m: Message, a: Agent) => {
        val localValue: Any = getRegisterVal(a)
        var reachedCount: Int = getSameValueCount(m, a) //a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)
        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val
        reachedCount == a.localState[Int](peersCountVar)
      }
      val noMoreAcksToWaitFor = (m: Message, a: Agent) => {
        getAcksCount(m, a) == a.localState[Int](peersCountVar)
      }
      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar).contains(m.payload.last.toString)
      }


      // statements start here

      val incrementReceivedAcksCnt = Statement {
        incrementReceivedAcksCount
      }

      val ifReceivedReplicaValueIsSameAsCachedValueThenIncrementSameValCount =
        If(sameValueAndSameRound)( // increment counter AND same-value-count by one
          ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
            val clientID = m.payload.last.toString
            val theAcksMap = a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar)
            val oldTuple = theAcksMap(clientID)
            theAcksMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1))
          })
        )

      val removeClientIDFromReadsAcksMap = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
        a.localState[Map[String, (Int, Int, Int)]](readsAcksMapVar) - m.payload.last.toString
      })

      val ifMajorityReachedSendReadAckToClient = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, getRegisterVal(a))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        removeClientIDFromReadsAcksMap
      )

      val ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest =
        If((m: Message, a: Agent) => {
          readsAcksMapContainsCount(m, a) && noMoreAcksToWaitFor(m, a)
        })(
          Statement((m: Message, a: Agent) => incrementRound(a, m.payload.last.toString)),
          Statement(resetAcksCount),
          Statement(incrementReceivedAcksCount), // to indicate I just read it
          Statement(resetSameValueCount),
          Statement(incrementSameValueCount), // to indicate it is the same as local value
          createAgentsIterator,
          While((m: Message, a: Agent) => {
            a.localState[Iterator[Agent]](iterAgentsVar).hasNext
          })(
            ModifyState(dstAgentVar, (_: Message, a: Agent) => {
              a.localState[Iterator[Agent]](iterAgentsVar).next
            }),
            ModifyState(msgToSendVar, (m: Message, a: Agent) => {
              val msg = msgReadReplica.copy
              msg.payload = Seq(registerKey, getRound(m, a), m.payload.last)
              msg
            }),
            Send(msgToSendVar, dstAgentVar)
          )
        )


      readReplicaAckAction + If(readsAcksMapContainsCount)(
        incrementReceivedAcksCnt,
        ifReceivedReplicaValueIsSameAsCachedValueThenIncrementSameValCount,
        ifMajorityReachedSendReadAckToClient,
        ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest)

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_W_RW_Retries_AND_cache(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val roundReadAcksReceived = s"roundReadAcksReceived${LocalState.DELIM}Int"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      m.payload[Int](2) == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      m.payload[Int](2) == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent, value: Any): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent, value: Any): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head, getReadRoundForReplicaRead(m, a), m.sender.name) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head, m.payload.last, getWriteRoundForReplication(m, a), m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: m.payload.tail
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a))
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      msg.payload = Seq(registerKey, getReadValue(m, a))
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        })

      val initializeAgentsIterator = ModifyState(iterAgentsVar, newAgentsIterator _)

      val broadcastReplicationRequests = While(cond = iterHasMoreAgents)(
        ModifyState(msgToSendVar, createWriteReplicaMessage _),
        ModifyState(dstAgentVar, nextAgent _),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        initializeAgentsIterator +
        broadcastReplicationRequests

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar, createWriteReplicaAckMessage _)

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString)

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = m.payload[Any](1) == getSameWriteValueCount(m, a).toString

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = getWriteAcksCount(m, a) == a.localState[Int](peersCountVar)


      val ifSameRound = If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
        Statement(incrementReceivedWriteAcksCount),
        If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount)))

      val ifReachedMajority = If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getWriteValue(m, a))),
        ModifyState(msgToSendVar, createWriteAckMessage _),
        ModifyState(dstAgentVar, getClient _),
        Send(msgToSendVar, dstAgentVar),
        Statement(removeClientFromWritesMap))

      val ifNotReachedMajorityButReachedAcksCount =
        If((m: Message, a: Agent) => {
          writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
        } && reachedWritePeersCount(m, a))(
          Statement(resetWriteAcksCount),
          Statement(incrementReceivedWriteAcksCount),
          Statement(resetSameWriteValueCount),
          Statement(incrementWriteSameValueAcksCount),
          Statement(incrementWriteRound),
          initializeAgentsIterator,
          broadcastReplicationRequests)

      replicateAckAction +
        ifSameRound +
        ifReachedMajority +
        ifNotReachedMajorityButReachedAcksCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      val initializeReadEntryForThisClient = ModifyState(readsAcksMapVar,
        (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        })

      val broadcastReadReplicaRequests = While(cond = iterHasMoreAgents)(
        ModifyState(msgToSendVar, createReadReplicaMessage _),
        ModifyState(dstAgentVar, nextAgent _),
        Send(msgToSendVar, dstAgentVar)
      )

      readAction +
        initializeReadEntryForThisClient +
        initializeAgentsIterator +
        broadcastReadReplicaRequests

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createMsgReplicaAck = ModifyState(msgToSendVar, createReadReplicaAckMessage _) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)

      def reachedMajorityRead(m: Message, a: Agent) = getReplicaReadAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameReadValue(m: Message, a: Agent): Boolean = m.payload[Int](1) == getSameReadValueCount(m, a)

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)


      val ifSameReadRound = If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
        Statement(incrementReceivedReadAcksCount),
        If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount)))

      val ifReachedReadMajority = If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
        ModifyState(msgToSendVar, createReadAckMessage _),
        ModifyState(dstAgentVar, getClient _),
        Send(msgToSendVar, dstAgentVar),
        Statement(removeClientFromReadsMap))

      val ifNotReachedMajorityButReachedAcksCountForReads =
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a)
        } && reachedReadPeersCount(m, a))(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          initializeAgentsIterator,
          broadcastReadReplicaRequests)

      replicateAckAction +
        ifSameReadRound +
        ifReachedReadMajority +
        ifNotReachedMajorityButReachedAcksCountForReads

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_W_RW_Retries_AND_cache_TAINTED(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val wReqIDVar = s"wReqID${LocalState.DELIM}String"
    val rReqIDVar = s"rReqID${LocalState.DELIM}String"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](1).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](rReqIDVar), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(registerKey,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](wReqIDVar), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(registerKey,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      msg.payload = Seq(registerKey, getReadValue(m, a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](wReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](rReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](dstAgentVar).name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(wReqIDVar) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        Statement(logInvokeWrite) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> m.payload.last)) +
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createWriteReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          Statement(logSend), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        Statement(logReceived) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(registerKey -> toReplicateValue)
        }) +
        ModifyState(msgToSendVar, createWriteReplicaAckMessage _) +
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        Statement(logReply) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        Statement(logReceived) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(msgToSendVar, createWriteAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          Statement(logWriteResponse), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(writesAcksMapVar, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(rReqIDVar, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        Statement(logInvokeRead) + // log
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createReadReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          Statement(logSend), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        Statement(logReceived) + // log
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        ModifyState(msgToSendVar, createReadReplicaAckMessage _) +
        Statement(logReply) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getReadValue(m, a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)


      // logged, i am just re-using it

      readReplicaAckAction +
        Statement(logReceived) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(msgToSendVar, createReadAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          Statement(logReadResponse), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(readsAcksMapVar, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // yup, otherwise it won't converge! (because the old value could be forever)
          ModifyState(iterAgentsVar, newAgentsIterator _), // initialize agents iterator
          While(cond = iterHasMoreAgents)(
            ModifyState(msgToSendVar, createReadReplicaRetryMessage _),
            ModifyState(dstAgentVar, nextAgent _),
            Statement(logSend), // log
            Send(msgToSendVar, dstAgentVar)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_R_Retries_AND_NO_cache_TAINTED(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val wReqIDVar = s"wReqID${LocalState.DELIM}String"
    val rReqIDVar = s"rReqID${LocalState.DELIM}String"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](1).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](rReqIDVar), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(registerKey,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](wReqIDVar), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(registerKey,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(registerKey, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](wReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](rReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](dstAgentVar).name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> "0") // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(wReqIDVar) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        Statement(logInvokeWrite) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> m.payload.last)) +
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createWriteReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          Statement(logSend), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        Statement(logReceived) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(registerKey -> toReplicateValue)
        }) +
        ModifyState(msgToSendVar, createWriteReplicaAckMessage _) +
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        Statement(logReply) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        Statement(logReceived) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(msgToSendVar, createWriteAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          Statement(logWriteResponse), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(writesAcksMapVar, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(rReqIDVar, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        Statement(logInvokeRead) + // log
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createReadReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          Statement(logSend), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        Statement(logReceived) + // log
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        ModifyState(msgToSendVar, createReadReplicaAckMessage _) +
        Statement(logReply) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)


      // logged, i am just re-using it

      readReplicaAckAction +
        Statement(logReceived) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          //          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(msgToSendVar, createReadAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          Statement(logReadResponse), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(readsAcksMapVar, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // not needed anymore in this non-cached read, but i will leave it
          ModifyState(iterAgentsVar, newAgentsIterator _), // initialize agents iterator
          While(cond = iterHasMoreAgents)(
            ModifyState(msgToSendVar, createReadReplicaRetryMessage _),
            ModifyState(dstAgentVar, nextAgent _),
            Statement(logSend), // log
            Send(msgToSendVar, dstAgentVar)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED(harnessFileToWritePath: String, numOfAgents: Int = 3, numOfRetries: Int = 3, log: Boolean = true): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val wReqIDVar = s"wReqID${LocalState.DELIM}String"
    val rReqIDVar = s"rReqID${LocalState.DELIM}String"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](1).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](rReqIDVar), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(registerKey,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](wReqIDVar), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(registerKey,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(registerKey, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](wReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](rReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](dstAgentVar).name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> "0") // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(wReqIDVar) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        If(cond = (_, _) => log)(Statement(logInvokeWrite)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> m.payload.last)) +
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createWriteReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(registerKey -> toReplicateValue)
        }) +
        ModifyState(msgToSendVar, createWriteReplicaAckMessage _) +
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(msgToSendVar, createWriteAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logWriteResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(writesAcksMapVar, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(rReqIDVar, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        If(cond = (_, _) => log)(Statement(logInvokeRead)) + // log
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createReadReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        ModifyState(msgToSendVar, createReadReplicaAckMessage _) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)

      def canRetryMore(m: Message, a: Agent): Boolean = numOfRetries <= getReadRound(m, a)

      // logged, i am just re-using it

      readReplicaAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          //          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(msgToSendVar, createReadAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logReadResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(readsAcksMapVar, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // not needed anymore in this non-cached read, but i will leave it
          ModifyState(iterAgentsVar, newAgentsIterator _), // initialize agents iterator
          While(cond = (m: Message, a: Agent) => iterHasMoreAgents(m, a) && canRetryMore(m, a))(
            ModifyState(msgToSendVar, createReadReplicaRetryMessage _),
            ModifyState(dstAgentVar, nextAgent _),
            If(cond = (_, _) => log)(Statement(logSend)), // log
            Send(msgToSendVar, dstAgentVar)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 4",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 5",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 6",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "write , 0, w",
      "replication , 0, w",
      "replicationAck , 0, w",
      "writeAck , 0, w",
      "read , 0, r",
      "msgReadReplica , 0, r",
      "msgReadReplicaAck , 0, r",
      "readAck , 0, r") // newline

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterSTRICT_limited_R_Retries_AND_NO_cache_TAINTED_ERRONEOUS(harnessFileToWritePath: String, numOfAgents: Int = 3, numOfRetries: Int = 3, log: Boolean = true): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val wReqIDVar = s"wReqID${LocalState.DELIM}String"
    val rReqIDVar = s"rReqID${LocalState.DELIM}String"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](1).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](rReqIDVar), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(registerKey,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](wReqIDVar), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(registerKey,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(registerKey, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](wReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](rReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](dstAgentVar).name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> "0") // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(wReqIDVar) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        If(cond = (_, _) => log)(Statement(logInvokeWrite)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> m.payload.last)) +
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createWriteReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(registerKey -> toReplicateValue)
        }) +
        ModifyState(msgToSendVar, createWriteReplicaAckMessage _) +
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(msgToSendVar, createWriteAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logWriteResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(writesAcksMapVar, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(rReqIDVar, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        If(cond = (_, _) => log)(Statement(logInvokeRead)) + // log
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createReadReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        ModifyState(msgToSendVar, createReadReplicaAckMessage _) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim >= 1.toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)

      def canRetryMore(m: Message, a: Agent): Boolean = numOfRetries <= getReadRound(m, a)

      // logged, i am just re-using it

      readReplicaAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          //          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(msgToSendVar, createReadAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logReadResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(readsAcksMapVar, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // not needed anymore in this non-cached read, but i will leave it
          ModifyState(iterAgentsVar, newAgentsIterator _), // initialize agents iterator
          While(cond = (m: Message, a: Agent) => iterHasMoreAgents(m, a) && canRetryMore(m, a))(
            ModifyState(msgToSendVar, createReadReplicaRetryMessage _),
            ModifyState(dstAgentVar, nextAgent _),
            If(cond = (_, _) => log)(Statement(logSend)), // log
            Send(msgToSendVar, dstAgentVar)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 4",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 5",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 6",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "write , 0, w",
      "replication , 0, w",
      "replicationAck , 0, w",
      "writeAck , 0, w",
      "read , 0, r",
      "msgReadReplica , 0, r",
      "msgReadReplicaAck , 0, r",
      "readAck , 0, r") // newline

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegisterMajority_W_Retries(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {
    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Any,Int,Int)]" // (count, value, round, received-same-value-count)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val roundReadAcksReceived = s"roundReadAcksReceived${LocalState.DELIM}Int"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksValue(m: Message, a: Agent): Any = {
      a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def isSameRound(m: Message, a: Agent): Boolean = {
      m.payload[Int](1) == a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._3
    }

    def incrementSameValueCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3, oldTuple._4 + 1))
    }

    def incrementReceivedAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, 0))
    }

    def getSameValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(clientID)._4
    }

    def getAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(clientID)._1
    }

    def resetAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def getRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)._3
    }

    /**
     * Increments the round number associated with the client ID and stored in the agent's local state
     *
     * @param a        the agent with the local state containing the round associated with the client ID
     * @param clientID the specific client ID in the local state
     * @return the new round number, after incrementing it
     */
    def incrementRound(a: Agent, clientID: String): Int = {
      val theMap = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._3 + 1
      val newTuple = (tuple._1, tuple._2, newRound, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksValue(m: Message, a: Agent, value: Any): Unit = {
      val old = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.payload.last.toString -> (old._1, value, old._3, old._4))
    }

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents - 1
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Any, Int, Int)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = getReplicaWriteAcksCount(m, a)
        reachedCount >= a.localState[Int](peersCountVar)
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        a.localState(readsAcksMapVar) = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) + (m.sender.name -> (0, getRegisterVal(a), 0, 0))
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement { (m: Message, a: Agent) => containsServerValueOrCreateEntry(m, a) }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
        val oldTuple = theAcksMap(m.sender.name)
        theAcksMap + (m.sender.name -> (1, getRegisterVal(a), oldTuple._3, 1)) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement {
        (m: Message, a: Agent) => {
          a.localState(iterAgentsVar) = ds.agents.filter { x =>
            !x.name.startsWith(ired.name.trim) &&
              x.name != a.name
          }.iterator
        } // code(m,a)
      } // statement
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ getRound(m, a) :+ m.sender.name
        msg
      })
      // send to all agents, only need to accumulate majority vote
      val whileHasMoreAgentsSendReadReplicaMessages = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar)
      ) // end while

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        whileHasMoreAgentsSendReadReplicaMessages

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload[Int](1), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      val sameValueAndSameRound = (m: Message, a: Agent) => {
        m.payload[Any](0) == getReplicaReadAcksValue(m, a) && isSameRound(m, a)
      }

      val reachedMajorityRead = (m: Message, a: Agent) => {
        val localValue: Any = getReplicaReadAcksValue(m, a)
        var reachedCount: Int = getSameValueCount(m, a) //a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)
        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val
        reachedCount > a.localState[Int](peersCountVar) / 2
      }

      val noMoreAcksToWaitForAndNotReachedMajority = (m: Message, a: Agent) => {
        getAcksCount(m, a) == a.localState[Int](peersCountVar) && getReplicaReadAcksCount(m, a) <= a.localState[Int](peersCountVar)
      }

      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar).contains(m.payload.last.toString)
      }

      val incrementReceivedAcksCnt = Statement {
        incrementReceivedAcksCount
      }

      val ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementSameValCount = If(sameValueAndSameRound)( // increment counter AND same-value-count by one
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          val clientID = m.payload.last.toString
          val theAcksMap = a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar)
          val oldTuple = theAcksMap(clientID)
          theAcksMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, oldTuple._4 + 1))
        }))

      val ifMajorityReachedSendReadAckToClientAndRemoveCount = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, getReplicaReadAcksValue(m, a)) //a.localState[Map[Any, Any]](registerVar)(registerKey))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
          a.localState[Map[String, (Int, Any, Int, Int)]](readsAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientIsInReadsAcksMapThenCheckIfMajorityReached = If(readsAcksMapContainsCount)(
        ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementSameValCount,
        ifMajorityReachedSendReadAckToClientAndRemoveCount
      )

      val ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest = If(noMoreAcksToWaitForAndNotReachedMajority)(
        Statement((m: Message, a: Agent) => incrementRound(a, m.payload.last.toString)),
        Statement(resetAcksCount),
        Statement(resetSameValueCount),
        Statement((m: Message, a: Agent) => setReadAcksValue(m, a, getRegisterVal(a))),
        createAgentsIterator,
        whileHasMoreAgentsSendReadReplicaMessages)

      readReplicaAckAction +
        incrementReceivedAcksCnt +
        ifClientIsInReadsAcksMapThenCheckIfMajorityReached +
        ifNoMoreAcksToWaitForThenNewRoundAndSendAllAReadReplicaRequest

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleLinearizableNAgentsRegister(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {
    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = s"k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val replicatedCountVar = s"replicatedCount${LocalState.DELIM}Int"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,Int]"
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents - 1
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(replicatedCountVar) = 0
      agent.localState(readsAcksMapVar) = Map[String, Int]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)
        reachedCount > (a.localState[Int](peersCountVar) / 2)
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.sender.name
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, Any]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        // why 1 => this current agent is one
        a.localState(readsAcksMapVar) = a.localState[Map[String, Any]](readsAcksMapVar) + (m.payload.last.toString -> 0)
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement { (m: Message, a: Agent) => containsServerValueOrCreateEntry(m, a) }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, Int]](readsAcksMapVar)
        theAcksMap + (m.sender.name -> 1) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement { (m: Message, a: Agent) => a.localState(iterAgentsVar) = ds.agents.filterNot(_.name.startsWith(ired.name.trim)).iterator }
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ m.sender.name
        msg
      })
      // send to all agents, only need to accumulate majority vote
      val whileHasMoreAgentsSendReadReplicaMessages = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar)
      ) // end while

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        whileHasMoreAgentsSendReadReplicaMessages

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------

      val sameValue = (m: Message, a: Agent) => {
        m.payload[Any](0) == a.localState[Map[Any, Any]](registerVar)(registerKey)
      }

      val reachedMajorityRead = (m: Message, a: Agent) => {
        var localValue: Any = null

        if (a.localState[Map[Any, Any]](registerVar).contains(registerKey)) {
          localValue = a.localState[Map[Any, Any]](registerVar)(registerKey)
        }

        var reachedCount: Int = a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)

        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val

        reachedCount > (a.localState[Int](peersCountVar) / 2)
      }

      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](readsAcksMapVar).contains(m.payload.last.toString)
      }

      // ==================================
      //  Read Replica Ack Action
      // ==================================
      val readReplicaAckAction = new Action

      val ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount = If(sameValue) { // increment counter by one
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          val theAcksMap = a.localState[Map[String, Int]](readsAcksMapVar)
          val client = m.payload.last.toString
          val oldValue = theAcksMap(client)
          theAcksMap + (client -> (oldValue + 1))
        })
      }

      val ifMajorityReachedSendReadAckToClientAndRemoveCount = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, a.localState[Map[Any, Any]](registerVar)(registerKey))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
          a.localState[Map[String, Int]](readsAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientIsInReadsAcksMapThenCheckIfMajorityReached = If(readsAcksMapContainsCount)(
        ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount,
        ifMajorityReachedSendReadAckToClientAndRemoveCount
      )

      readReplicaAckAction +
        ifClientIsInReadsAcksMapThenCheckIfMajorityReached

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def sampleNoneLinearizableNAgentsRegister(harnessFileToWritePath: String, numOfAgents: Int = 3): (DistributedSystem, File) = {
    val ds = new DistributedSystem("distributed-register-majority-rw")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = s"k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,Int]"
    val replicatedCountVar = s"replicatedCount${LocalState.DELIM}Int"
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,Int]"
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> 0) // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents - 1
      agent.localState(writesAcksMapVar) = Map[String, Int]()
      agent.localState(replicatedCountVar) = 0
      agent.localState(readsAcksMapVar) = Map[String, Int]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      val initializeReplicationAcksCount = ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> 0)
      })

      val assignReceivedValueToRegister = ModifyState(registerVar,
        (m: Message, a: Agent) => {
          Map(registerKey -> m.payload.tail.head)
        })

      val incrementClientWritesAcksCounter = ModifyState(writesAcksMapVar,
        (m: Message, a: Agent) => {
          val oldCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.sender.name)
          a.localState[Map[String, Int]](writesAcksMapVar) + (m.sender.name -> (oldCount + 1))
        })

      val getAgentsIterator = ModifyState(iterAgentsVar,
        (m: Message, a: Agent) => {
          // again, normally ired wouldn't be in ds.agents since using msgs exchanged during init stage of agents won't
          // yield an Ack from ired, so it won't be added to this iterated set of neighbors.
          ds.agents.filterNot(ax => ax.name == a.name || ax.name.startsWith(ired.name)).iterator
        })

      val thereAreMoreAgents = (m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      }

      val broadcastReplicateRequestsToAgents = While(thereAreMoreAgents)(
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val r = msgReplication.copy
          r.payload = m.payload :+ m.sender.name
          r
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => a.localState[Iterator[Agent]](iterAgentsVar).next),
        Send(msgToSendVar, dstAgentVar)
      )

      writeAction +
        initializeReplicationAcksCount +
        assignReceivedValueToRegister +
        incrementClientWritesAcksCounter +
        getAgentsIterator +
        broadcastReplicateRequestsToAgents

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      val writeReceivedValueToRegister = ModifyState(registerVar, (m: Message, a: Agent) => {
        val toReplicateValue = m.payload[String](1)
        Map(registerKey -> toReplicateValue)
      })

      val prepareReplicationAckMessage = ModifyState(msgToSendVar,
        (m: Message, a: Agent) => {
          val msg = msgReplicationAck.copy
          msg.payload = m.payload
          msg
        })

      val prepareDstAgentToReplyTo = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)

      val sendReplicationAck = Send(msgToSendVar, dstAgentVar)

      replicateAction +
        writeReceivedValueToRegister +
        prepareReplicationAckMessage +
        prepareDstAgentToReplyTo +
        sendReplicationAck

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      val writeAcksMapContainsClient = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](writesAcksMapVar).contains(m.payload.last.toString)
      }

      val reachedMajorityWrite = (m: Message, a: Agent) => {
        val reachedCount = a.localState[Map[String, Int]](writesAcksMapVar)(m.payload.last.toString)
        reachedCount > (a.localState[Int](peersCountVar) / 2 - 1) // flawed majority in purpose, assuming the size won't be less than 3
      }

      val ifWritesAckContainsClientANDnotReachedMajority = (m: Message, a: Agent) => {
        writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a)
      }


      // used inside the next statement
      val ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount = If(reachedMajorityWrite)(
        // remove client-specific replicate-count
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val wAck = msgWriteAck.copy
          wAck.payload = m.payload.dropRight(1)
          wAck
        }),
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        Send(msgToSendVar, dstAgentVar), // reply to client
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, Int]](writesAcksMapVar) - m.sender.name
        })
      )

      val ifClientInWriteAcksMapThenIncrementCount = If(ifWritesAckContainsClientANDnotReachedMajority)(
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => { // increment count
          val theAcksMap = a.localState[Map[String, Int]](writesAcksMapVar)
          val client = m.payload.last.toString
          val oldCount = theAcksMap(client)
          theAcksMap + (client -> (oldCount + 1))
        }),
        ifMajorityWritesReachedThenSendWriteAckToClientAndRemoveItsCount
      ) // end outer-if: ifClientInWritesAcksMapThenIncrementCount

      replicateAckAction +
        ifClientInWriteAcksMapThenIncrementCount

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      def containsServerValueOrCreateEntry(m: Message, a: Agent): Unit = {
        if (!a.localState[Map[String, Any]](readsAcksMapVar).contains(m.sender.name)) // reading client (IRed)
        // why 1 => this current agent is one
        a.localState(readsAcksMapVar) = a.localState[Map[String, Any]](readsAcksMapVar) + (m.payload.last.toString -> 0)
      }

      val ifReadsAcksMapNotContainClientThenCreateIt = Statement { (m: Message, a: Agent) => containsServerValueOrCreateEntry(m, a) }

      val setReadAcksCountToOne = ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
        val theAcksMap = a.localState[Map[String, Int]](readsAcksMapVar)
        theAcksMap + (m.sender.name -> 1) // reset the count of acks for that specific client
      })

      val createAgentsIterator = Statement { (m: Message, a: Agent) => a.localState(iterAgentsVar) = ds.agents.filterNot(_.name.startsWith(ired.name.trim)).iterator }
      val createAndSetMessageToSendVar = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplica.copy
        msg.payload = m.payload :+ m.sender.name
        msg
      })
      // send to all agents, only need to accumulate majority vote
      val whileHasMoreAgentsSendReadReplicaMessages = While((m: Message, a: Agent) => {
        a.localState[Iterator[Agent]](iterAgentsVar).hasNext
      })(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => {
          a.localState[Iterator[Agent]](iterAgentsVar).next
        }),
        Send(msgToSendVar, dstAgentVar)
      ) // end while

      readAction +
        ifReadsAcksMapNotContainClientThenCreateIt +
        setReadAcksCountToOne +
        createAgentsIterator +
        createAndSetMessageToSendVar +
        whileHasMoreAgentsSendReadReplicaMessages

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      val setDstAgentToMessageSender = ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender)
      val createAndSetMsgReplicaAck = ModifyState(msgToSendVar, (m: Message, a: Agent) => {
        val msg = msgReadReplicaAck.copy
        msg.payload = Seq(a.localState[Map[Any, Any]](registerVar)(registerKey), m.payload.last)
        msg
      }) // m.payload.last is the client-name (ID)
      val sendMsgReplicaAckToDstAgent = Send(msgToSendVar, dstAgentVar)

      readReplicaAction +
        setDstAgentToMessageSender +
        createAndSetMsgReplicaAck +
        sendMsgReplicaAckToDstAgent

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------

      val sameValue = (m: Message, a: Agent) => {
        m.payload[Any](0) == a.localState[Map[Any, Any]](registerVar)(registerKey)
      }

      val reachedMajorityRead = (m: Message, a: Agent) => {
        var localValue: Any = null

        if (a.localState[Map[Any, Any]](registerVar).contains(registerKey)) {
          localValue = a.localState[Map[Any, Any]](registerVar)(registerKey)
        }

        var reachedCount: Int = a.localState[Map[String, Int]](readsAcksMapVar)(m.payload.last.toString)

        if (localValue == m.payload[Any](0)) reachedCount += 1 // for received val

        reachedCount > (a.localState[Int](peersCountVar) / 2 - 1) // assuming the size is 3 or more
      }

      val readsAcksMapContainsCount = (m: Message, a: Agent) => {
        a.localState[Map[String, Int]](readsAcksMapVar).contains(m.payload.last.toString)
      }

      val readReplicaAckAction = new Action

      val ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount = If(sameValue) { // increment counter by one
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          val theAcksMap = a.localState[Map[String, Int]](readsAcksMapVar)
          val client = m.payload.last.toString
          val oldValue = theAcksMap(client)
          theAcksMap + (client -> (oldValue + 1))
        })
      }

      val ifMajorityReachedSendReadAckToClientAndRemoveCount = If(reachedMajorityRead)(
        ModifyState(dstAgentVar, (m: Message, a: Agent) => ds.get(m.payload.last.toString)),
        ModifyState(msgToSendVar, (m: Message, a: Agent) => {
          val msg = msgReadAck.copy
          msg.payload = Seq(registerKey, a.localState[Map[Any, Any]](registerVar)(registerKey))
          msg
        }),
        Send(msgToSendVar, dstAgentVar),
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => { // remove key so no more acks sending to client
          a.localState[Map[String, Int]](readsAcksMapVar) - m.payload.last.toString
        })
      )

      val ifClientIsInReadsAcksMapThenCheckIfMajorityReached = If(readsAcksMapContainsCount)(
        ifReceivedReplicaValueIsSameAsCurrentValueThenIncrementCount,
        ifMajorityReachedSendReadAckToClientAndRemoveCount
      )

      readReplicaAckAction +
        ifClientIsInReadsAcksMapThenCheckIfMajorityReached

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write

  def transDporExample(harnessFileToWritePath: String, log: Boolean = false): (DistributedSystem, File) = {

    val ds = new DistributedSystem("transDPOR-registry")

    //------------------------------------
    // strings
    //------------------------------------
    val register = "register"
    val regAddress = "registry-address"

    //------------------------------------
    // Messages
    //------------------------------------

    // the master is IRed0
    val registry = new Agent("registry")
    registry.locked = false
    val worker1 = new Agent("worker1")
    worker1.locked = false
    val worker2 = new Agent("worker2")
    worker2.locked = false

    // just a formality for the scheduler not to complain
    registry.localState("registerVar$$Map[Any,Any]") = Map("k" -> "0")
    worker1.localState("registerVar$$Map[Any,Any]") = Map("k" -> "0")
    worker2.localState("registerVar$$Map[Any,Any]") = Map("k" -> "0")


    // messages
    val r0 = new Message(register)
    val r1 = new Message(register)
    val r2 = new Message(register)

    val w1 = new Message(regAddress)
    val w2 = new Message(regAddress)

    //------------------------------------
    // adding reactions
    //------------------------------------
    val registryAction = new Action + If((_, _) => log) {
      Statement((m: Message, a: Agent) => println("REGISTRY received: " + m.name + " FROM " + m.sender.name))
    }
    registry.reactions += (r0.copy -> registryAction)

    val action1 = new Action +
      If((_, _) => log) {
        Statement((m: Message, a: Agent) => println("WORKER1 received: " + m.name + " FROM " + m.sender.name))
      } +
      Send(r1.copy, registry) //+
    val action2 = new Action +
      If((_, _) => log) {
        Statement((m: Message, a: Agent) => println("WORKER2 received: " + m.name + " FROM " + m.sender.name))
      } +
      Send(r2.copy, registry) //+

    worker1.reactions += (w1 -> action1)
    worker2.reactions += (w2 -> action2)

    registry.defaultBehavior = registry.reactions
    worker1.defaultBehavior = worker1.reactions
    worker2.defaultBehavior = worker2.reactions

    ds + registry + worker1 + worker2


    // produce harness file

    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}

    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      """registerVar$$Map[Any,Any]""", "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith("IRed")).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, write, k, 1", // add operations here (harness)
      s"read, read, k",
      s"read, read, k",
      s"write, write, k, 2",
      s"read, read, k",
      s"read, read, k",
      s"read, read, k",
      s"write, write, k, 3",
      s"read, read, k",
      s"read, read, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "read" + ", whatever1", // add response messages regexes
      "write" + ", whatever2")

    ds.refresh
    (ds, new File(harnessFileToWritePath)) // return it
  }


  def sampleDistributedRegister_SEMI_FINAL_VERSION(harnessFileToWritePath: String, numOfAgents: Int = 3, numOfRetries: Int = 3, log: Boolean = true): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw-SEMI-FINAL-VERSION")

    val registerVar = s"register${LocalState.DELIM}Map[Any,Any]"
    val registerKey = "k"
    val peersCountVar = s"peersCount${LocalState.DELIM}Int"
    val writesAcksMapVar = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val readsAcksMapVar = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    val harnessFile = new File(harnessFileToWritePath)
    val iterAgentsVar = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    val dstAgentVar = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val msgToSendVar = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val wReqIDVar = s"wReqID${LocalState.DELIM}String"
    val rReqIDVar = s"rReqID${LocalState.DELIM}String"

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[Map[Any, Any]](registerVar)(registerKey)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(registerVar) = a.localState.apply[Map[Any, Any]](registerVar) + (registerKey -> value)
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](1).toString.trim == a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(writesAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(readsAcksMapVar) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(writesAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(readsAcksMapVar) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(m.payload.last.toString)
      a.localState(writesAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(m.payload.last.toString)
      a.localState(readsAcksMapVar) = a.localState.apply[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(readsAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)
      val oldTuple = oldMap(clientID)
      a.localState(writesAcksMapVar) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[Iterator[Agent]](iterAgentsVar)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) - m.payload.last.toString
    }

    def removeClientFromReadsMap(m: Message, a: Agent): Map[String, (Int, Int, Int, Any)] = {
      a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) - m.payload.last.toString
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](rReqIDVar), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(registerKey,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](wReqIDVar), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(registerKey,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[Map[Any, Any]](registerVar)(registerKey) +: registerKey +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(registerKey, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(registerKey, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](wReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](rReqIDVar)} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](dstAgentVar).name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${a.localState[Message](msgToSendVar).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](dstAgentVar).name} <-- ${a.name}\t (${a.localState[Message](msgToSendVar).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](registerVar)(registerKey)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(registerVar) = Map[Any, Any](registerKey -> "0") // use only one k-v pair!
      agent.localState(peersCountVar) = numOfAgents
      agent.localState(writesAcksMapVar) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(readsAcksMapVar) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(dstAgentVar) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(wReqIDVar) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        If(cond = (_, _) => log)(Statement(logInvokeWrite)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> m.payload.last)) +
        ModifyState(writesAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createWriteReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(registerVar, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(registerKey -> toReplicateValue)
        }) +
        ModifyState(msgToSendVar, createWriteReplicaAckMessage _) +
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](writesAcksMapVar).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](peersCountVar)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(msgToSendVar, createWriteAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logWriteResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(writesAcksMapVar, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(rReqIDVar, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        If(cond = (_, _) => log)(Statement(logInvokeRead)) + // log
        ModifyState(readsAcksMapVar, (m: Message, a: Agent) => {
          a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(iterAgentsVar, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(msgToSendVar, createReadReplicaMessage _),
          ModifyState(dstAgentVar, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(msgToSendVar, dstAgentVar)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(dstAgentVar, (m: Message, a: Agent) => m.sender) +
        ModifyState(msgToSendVar, createReadReplicaAckMessage _) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(msgToSendVar, dstAgentVar)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](readsAcksMapVar).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](peersCountVar).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](peersCountVar)

      def canRetryMore(m: Message, a: Agent): Boolean = numOfRetries <= getReadRound(m, a)

      // logged, i am just re-using it

      readReplicaAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          //          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(msgToSendVar, createReadAckMessage _),
          ModifyState(dstAgentVar, getClient _),
          If(cond = (_, _) => log)(Statement(logReadResponse)), // log
          Send(msgToSendVar, dstAgentVar),
          ModifyState(readsAcksMapVar, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // not needed anymore in this non-cached read, but i will leave it
          ModifyState(iterAgentsVar, newAgentsIterator _), // initialize agents iterator
          While(cond = (m: Message, a: Agent) => iterHasMoreAgents(m, a) && canRetryMore(m, a))(
            ModifyState(msgToSendVar, createReadReplicaRetryMessage _),
            ModifyState(dstAgentVar, nextAgent _),
            If(cond = (_, _) => log)(Statement(logSend)), // log
            Send(msgToSendVar, dstAgentVar)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(peersCountVar) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      registerVar, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 4",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 5",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 6",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "write , 0, w",
      "replication , 0, w",
      "replicationAck , 0, w",
      "writeAck , 0, w",
      "read , 0, r",
      "msgReadReplica , 0, r",
      "msgReadReplicaAck , 1, r",
      "readAck , 0, r") // newline

    ds.refresh
    (ds, harnessFile) // return it
  } // dist-register with majority read/write
  def sampleDistributedRegister_FINAL_VERSION(harnessFileToWritePath: String, numOfAgents: Int = 3, numOfRetries: Int = 3, log: Boolean = true): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw-FINAL-VERSION")

    val REGISTER = s"register${LocalState.DELIM}Map[Any,Any]"
    type REGISTER_TYPE = Map[Any,Any]
    val KEY = "k"
    type KEY_TYPE = Any
    val PEERS_COUNT = s"peersCount${LocalState.DELIM}Int"
    type PEERS_COUNT_TYPE = Int
    val WRITES_ACKS = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    type WRITES_ACKS_TYPE = Map[String,(Int,Int,Int,Any)]
    val READS_ACKS = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    type READS_ACKS_TYPE = Map[String,(Int,Int,Int,Any)]
    val HARNESS_FILE = new File(harnessFileToWritePath)
    val ITER_AGENTS = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    type ITER_AGENTS_TYPE = Iterator[Agent]
    val DST_AGENT = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val MSG_TO_SEND = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val W_REQ_ID = s"wReqID${LocalState.DELIM}String"
    val R_REQ_ID = s"rReqID${LocalState.DELIM}String"
    type REQ_ID_TYPE = String

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[REGISTER_TYPE](REGISTER)(KEY)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(REGISTER) = a.localState[REGISTER_TYPE](REGISTER) + KEY -> value
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(WRITES_ACKS) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(READS_ACKS) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(WRITES_ACKS) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(READS_ACKS) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)
      a.localState(WRITES_ACKS) = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)
      a.localState(READS_ACKS) = a.localState.apply[READS_ACKS_TYPE](READS_ACKS) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[ITER_AGENTS_TYPE](ITER_AGENTS)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): WRITES_ACKS_TYPE = {
      val result = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS) - m.payload.last.toString
      a.localState(WRITES_ACKS) = result
      result
    }

    def removeClientFromReadsMap(m: Message, a: Agent): READS_ACKS_TYPE = {
      val result = a.localState[READS_ACKS_TYPE](READS_ACKS) - m.payload.last.toString
      a.localState(READS_ACKS) = result
      result
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](R_REQ_ID), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(KEY,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](W_REQ_ID), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(KEY,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[REGISTER_TYPE](REGISTER)(KEY) +: KEY +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(KEY, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(KEY, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](W_REQ_ID)} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](R_REQ_ID)} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](DST_AGENT).name}\t (${a.localState[Message](MSG_TO_SEND).name}--${a.localState[Message](MSG_TO_SEND).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${a.localState[Message](MSG_TO_SEND).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(REGISTER) = Map[Any, Any](KEY -> "0") // use only one k-v pair!
      agent.localState(PEERS_COUNT) = numOfAgents
      agent.localState(WRITES_ACKS) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(READS_ACKS) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(DST_AGENT) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction +
        Statement((_, a: Agent) => a.localState(W_REQ_ID) = UUID.randomUUID().toString.takeRight(4).toUpperCase) +
        If(cond = (_, _) => log)(Statement(logInvokeWrite)) + // log
        ModifyState(REGISTER, (m: Message, a: Agent) => Map[Any, Any](KEY -> m.payload.last)) +
        ModifyState(WRITES_ACKS, (m: Message, a: Agent) => {
          a.localState[WRITES_ACKS_TYPE](WRITES_ACKS) + (m.sender.name -> (1, 0, 1, m.payload.last))
        }) +
        ModifyState(ITER_AGENTS, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(MSG_TO_SEND, createWriteReplicaMessage _),
          ModifyState(DST_AGENT, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(MSG_TO_SEND, DST_AGENT)
        )

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(REGISTER, (m: Message, a: Agent) => {
          val toReplicateValue = m.payload[String](1)
          Map(KEY -> toReplicateValue)
        }) +
        ModifyState(MSG_TO_SEND, createWriteReplicaAckMessage _) +
        ModifyState(DST_AGENT, (m: Message, a: Agent) => m.sender) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(MSG_TO_SEND, DST_AGENT)

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](PEERS_COUNT)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](PEERS_COUNT).toString.trim
        ans
      }


      // logged already! I am just re-using it

      replicateAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a))(
          Statement(incrementReceivedWriteAcksCount),
          If(cond = isSameWriteValue)(Statement(incrementWriteSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a))(
          ModifyState(MSG_TO_SEND, createWriteAckMessage _),
          ModifyState(DST_AGENT, getClient _),
          If(cond = (_, _) => log)(Statement(logWriteResponse)), // log
          Send(MSG_TO_SEND, DST_AGENT),
          ModifyState(WRITES_ACKS, removeClientFromWritesMap _))
      //        If((m: Message, a: Agent) => {writeAcksMapContainsClient(m, a) && !reachedMajorityWrite(m, a) && reachedWritePeersCount(m, a)})(
      //          Statement(resetWriteAcksCount),
      //          Statement(incrementReceivedWriteAcksCount),
      //          Statement(resetSameWriteValueCount),
      //          Statement(incrementWriteSameValueAcksCount),
      //          Statement(incrementWriteRound),
      //          ModifyState(iterAgentsVar, newAgentsIterator _),
      //          While(cond = iterHasMoreAgents)(
      //            ModifyState(msgToSendVar, createWriteReplicaRetryMessage _),
      //            ModifyState(dstAgentVar, nextAgent _),
      //            Statement(logSend), // log
      //            Send(msgToSendVar, dstAgentVar)
      //          )
      //        )

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction +
        ModifyState(R_REQ_ID, (m: Message, a: Agent) => UUID.randomUUID().toString.takeRight(4).toUpperCase()) +
        If(cond = (_, _) => log)(Statement(logInvokeRead)) + // log
        ModifyState(READS_ACKS, (m: Message, a: Agent) => {
          a.localState[READS_ACKS_TYPE](READS_ACKS) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        }) +
        ModifyState(ITER_AGENTS, newAgentsIterator _) +
        While(cond = iterHasMoreAgents)(
          ModifyState(MSG_TO_SEND, createReadReplicaMessage _),
          ModifyState(DST_AGENT, nextAgent _),
          If(cond = (_, _) => log)(Statement(logSend)), // log
          Send(MSG_TO_SEND, DST_AGENT)
        )

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg
      readReplicaAction +
        If(cond = (_, _) => log)(Statement(logReceived)) + // log
        ModifyState(DST_AGENT, (m: Message, a: Agent) => m.sender) +
        ModifyState(MSG_TO_SEND, createReadReplicaAckMessage _) +
        If(cond = (_, _) => log)(Statement(logReply)) + // log
        Send(MSG_TO_SEND, DST_AGENT)

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](READS_ACKS).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](PEERS_COUNT).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](PEERS_COUNT)

      def canRetryMore(m: Message, a: Agent): Boolean = numOfRetries <= getReadRound(m, a)

      // logged, i am just re-using it

      readReplicaAckAction +
        If(cond = (_, _) => log)(Statement(logReceived)) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && isSameReadRound(m, a))(
          Statement(incrementReceivedReadAcksCount),
          If(cond = isSameReadValue)(Statement(incrementReadSameValueAcksCount))) +
        If(cond = (m: Message, a: Agent) => readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a))(
          //          ModifyState(registerVar, (m: Message, a: Agent) => Map[Any, Any](registerKey -> getReadValue(m, a))),
          ModifyState(MSG_TO_SEND, createReadAckMessage _),
          ModifyState(DST_AGENT, getClient _),
          If(cond = (_, _) => log)(Statement(logReadResponse)), // log
          Send(MSG_TO_SEND, DST_AGENT),
          ModifyState(READS_ACKS, removeClientFromReadsMap _)) +
        If((m: Message, a: Agent) => {
          readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)
        })(
          Statement(resetReadAcksCount),
          Statement(incrementReceivedReadAcksCount),
          Statement(resetSameReadValueCount),
          Statement(incrementReadSameValueAcksCount),
          Statement(incrementReadRound),
          Statement(setReadValue), // not needed anymore in this non-cached read, but i will leave it
          ModifyState(ITER_AGENTS, newAgentsIterator _), // initialize agents iterator
          While(cond = (m: Message, a: Agent) => iterHasMoreAgents(m, a) && canRetryMore(m, a))(
            ModifyState(MSG_TO_SEND, createReadReplicaRetryMessage _),
            ModifyState(DST_AGENT, nextAgent _),
            If(cond = (_, _) => log)(Statement(logSend)), // log
            Send(MSG_TO_SEND, DST_AGENT)
          )
        )

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(PEERS_COUNT) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      REGISTER, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 4",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 5",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 6",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "write , 0, w",
      "replication , 0, w",
      "replicationAck , 0, w", // in this implementation, this one definitely WILL affect the state in question at the originator
//      "writeAck , 0, w",
      "read , 0, r",
      "msgReadReplica , 0, r",
      "msgReadReplicaAck , 1, r"
//      "readAck , 0, r"
    ) // newline

    ds.refresh
    (ds, HARNESS_FILE) // return it
  } // dist-register with majority read/write

  def sampleDistributedRegister_FINAL_VERSION_SIMPLIFIED(harnessFileToWritePath: String, numOfAgents: Int = 3, numOfRetries: Int = 3, log: Boolean = true): (DistributedSystem, File) = {

    val ds = new DistributedSystem("distributed-register-majority-rw-FINAL-VERSION-SIMPLIFIED")

    val REGISTER = s"register${LocalState.DELIM}Map[Any,Any]"
    type REGISTER_TYPE = Map[Any,Any]
    val KEY = "k"
    type KEY_TYPE = Any
    val PEERS_COUNT = s"peersCount${LocalState.DELIM}Int"
    type PEERS_COUNT_TYPE = Int
    val WRITES_ACKS = s"writeAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    type WRITES_ACKS_TYPE = Map[String,(Int,Int,Int,Any)]
    val READS_ACKS = s"readAcksMap${LocalState.DELIM}Map[String,(Int,Int,Int,Any)]" // (count, round, received-same-value-count, value)
    type READS_ACKS_TYPE = Map[String,(Int,Int,Int,Any)]
    val HARNESS_FILE = new File(harnessFileToWritePath)
    val ITER_AGENTS = s"agentsIterator${LocalState.DELIM}Iterator[Agent]"
    type ITER_AGENTS_TYPE = Iterator[Agent]
    val DST_AGENT = s"dstAgentVar${LocalState.DELIM}Agent" // use it for fixing the model implementation
    val MSG_TO_SEND = s"msgToSendVar${LocalState.DELIM}Message" // use it for fixing the model implementation
    val W_REQ_ID = s"wReqID${LocalState.DELIM}String"
    val R_REQ_ID = s"rReqID${LocalState.DELIM}String"
    type REQ_ID_TYPE = String

    // different messages
    val msgWrite = new Message("write")
    val msgWriteAck = new Message("writeAck")
    val msgReplication = new Message("replication")
    val msgReplicationAck = new Message("replicationAck")
    val msgRead = new Message("read")
    val msgReadAck = new Message("readAck")
    val msgReadReplica = new Message("msgReadReplica")
    val msgReadReplicaAck = new Message("msgReadReplicaAck")

    // normally we don't need this one. the way this example is setup, however, needs this.
    val ired = new Agent("IRed")

    def getRegisterVal(a: Agent): Any = a.localState.apply[REGISTER_TYPE](REGISTER)(KEY)

    def setRegisterVal(a: Agent, value: Any): Unit = {
      a.localState(REGISTER) = a.localState[REGISTER_TYPE](REGISTER) + KEY -> value
    }

    def getReplicaWriteAcksCount(m: Message, a: Agent): Int = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._1

    def getReplicaReadAcksCount(m: Message, a: Agent): Int = {
      val ans = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._1
      ans
    }

    def isSameWriteRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2.toString.trim
      ans
    }

    def isSameReadRound(m: Message, a: Agent): Boolean = {
      val ans = m.payload[Any](2).toString.trim == a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2.toString.trim
      ans // for debugging
    }

    def getWriteRound(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2
    }

    def getReadRound(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2
    }

    def getWriteRoundForReplication(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.sender.name)._2
    }

    def getWriteRoundForReplicationRetry(m: Message, a: Agent): Int = {
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)._2
    }

    def getReadRoundForReplicaRead(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.sender.name)._2
    }

    def getReadRoundForReplicaReadRetry(m: Message, a: Agent): Int = {
      a.localState[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)._2
    }

    def incrementWriteSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(WRITES_ACKS) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReadSameValueAcksCount(m: Message, a: Agent): Unit = {
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(m.payload.last.toString)
      a.localState(READS_ACKS) = oldMap + (m.payload.last.toString -> (oldTuple._1, oldTuple._2, oldTuple._3 + 1, oldTuple._4))
    }

    def incrementReceivedWriteAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def incrementReceivedReadAcksCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1 + 1, oldTuple._2, oldTuple._3, oldTuple._4))
    }

    def resetSameWriteValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def resetSameReadValueCount(m: Message, a: Agent): Unit = {
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, 0, oldTuple._4))
    }

    def getSameWriteValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val ans = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._3
      ans
    }

    def getSameReadValueCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._3
    }

    def getWriteAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._1
    }

    def getReadAcksCount(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._1
    }

    def resetWriteAcksCount(m: Message, a: Agent): Unit = {
      setWriteAcksCount(m, a, 0)
    }

    def resetReadAcksCount(m: Message, a: Agent): Unit = {
      setReadAcksCount(m, a, 0)
    }

    def incrementWriteRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(WRITES_ACKS) = theMap + (clientID -> newTuple)
      newRound
    }

    def incrementReadRound(m: Message, a: Agent): Int = {
      val clientID = m.payload.last.toString
      val theMap = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)
      val tuple = theMap(clientID)
      val newRound = tuple._2 + 1
      val newTuple = (tuple._1, newRound, tuple._3, tuple._4)
      a.localState(READS_ACKS) = theMap + (clientID -> newTuple)
      newRound
    }

    def setWriteAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS)(m.payload.last.toString)
      a.localState(WRITES_ACKS) = a.localState.apply[WRITES_ACKS_TYPE](WRITES_ACKS) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadAcksCount(m: Message, a: Agent, count: Int): Unit = {
      val old = a.localState.apply[READS_ACKS_TYPE](READS_ACKS)(m.payload.last.toString)
      a.localState(READS_ACKS) = a.localState.apply[READS_ACKS_TYPE](READS_ACKS) + (m.payload.last.toString -> (count, old._2, old._3, old._4))
    }

    def setReadValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[READS_ACKS_TYPE](READS_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(READS_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def setWriteValue(m: Message, a: Agent): Unit = {
      val value = getRegisterVal(a)
      val clientID = m.payload.last.toString
      val oldMap = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)
      val oldTuple = oldMap(clientID)
      a.localState(WRITES_ACKS) = oldMap + (clientID -> (oldTuple._1, oldTuple._2, oldTuple._3, value))
    }

    def getReadValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      val value = a.localState[READS_ACKS_TYPE](READS_ACKS)(clientID)._4
      value
    }

    def getWriteValue(m: Message, a: Agent): Any = {
      val clientID = m.payload.last.toString
      a.localState[WRITES_ACKS_TYPE](WRITES_ACKS)(clientID)._4
    }

    def getAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      a.localState[ITER_AGENTS_TYPE](ITER_AGENTS)
    }

    def newAgentsIterator(m: Message, a: Agent): Iterator[Agent] = {
      ds.agents.filter { x =>
        !x.name.startsWith(ired.name.trim) &&
          x.name != a.name
      }.iterator
    }

    def iterHasMoreAgents(m: Message, a: Agent): Boolean = {
      getAgentsIterator(m, a).hasNext
    }

    def nextAgent(m: Message, a: Agent): Agent = {
      getAgentsIterator(m, a).next()
    }

    def removeClientFromWritesMap(m: Message, a: Agent): WRITES_ACKS_TYPE = {
      val result = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS) - m.payload.last.toString
//      a.localState(WRITES_ACKS) = result
      result
    }

    def removeClientFromReadsMap(m: Message, a: Agent): READS_ACKS_TYPE = {
      val result = a.localState[READS_ACKS_TYPE](READS_ACKS) - m.payload.last.toString
      a.localState(READS_ACKS) = result
      result
    }

    def createReadReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(m.payload.head,
        getReadRoundForReplicaRead(m, a),
        a.localState[String](R_REQ_ID), // log
        m.sender.name) // key, round, clientID
      msg
    }

    def createReadReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplica.copy
      msg.payload = Seq(KEY,
        getReadRoundForReplicaReadRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, round, clientID
      msg
    }

    def createWriteReplicaMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(m.payload.head,
        m.payload.last,
        getWriteRoundForReplication(m, a),
        a.localState[String](W_REQ_ID), // log
        m.sender.name) // key, value, round, clientID
      msg
    }

    def createWriteReplicaRetryMessage(m: Message, a: Agent): Message = {
      val msg = msgReplication.copy
      msg.payload = Seq(KEY,
        getWriteValue(m, a),
        getWriteRoundForReplicationRetry(m, a),
        m.payload.takeRight(2).head, // log
        m.payload.last) // key, value, round, clientID
      msg
    }

    def createWriteReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReplicationAck.copy
      msg.payload = m.payload // contains UUID
      msg
    }

    def createReadReplicaAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadReplicaAck.copy
      msg.payload = a.localState[REGISTER_TYPE](REGISTER)(KEY) +: KEY +: m.payload.tail // log
      msg
    }

    def createWriteAckMessage(m: Message, a: Agent): Message = {
      val msg = msgWriteAck.copy
      msg.payload = Seq(KEY, getWriteValue(m, a)) // special log
      msg
    }

    def createReadAckMessage(m: Message, a: Agent): Message = {
      val msg = msgReadAck.copy
      //      msg.payload = Seq(registerKey, getReadValue(m,a)) // special log
      msg.payload = Seq(KEY, getRegisterVal(a)) // special log
      msg
    }

    def getClient(m: Message, a: Agent): Agent = {
      ds.get(m.payload.last.toString)
    }


    // LOGGING for debugging
    /**
     * for logging invocations
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logInvokeWrite(m: Message, a: Agent): Unit = println(s"WRITE:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](W_REQ_ID)} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logInvokeRead(m: Message, a: Agent): Unit = println(s"READ:\t ${m.sender.name} --> ${a.name}\t (${m.name}--${a.localState[String](R_REQ_ID)} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logReceived(m: Message, a: Agent): Unit = println(s"REC:\t ${a.name} <-- ${m.sender.name}\t (${m.name}--${m.payload.takeRight(2).head})")

    /**
     * For logging a send (that is NOT a reply to a message received, i.e. for initiating a repl)
     *
     * @param m message received
     * @param a agent receiving it
     */
    def logSend(m: Message, a: Agent): Unit = println(s"SEND:\t ${a.name} --> ${a.localState[Agent](DST_AGENT).name}\t (${a.localState[Message](MSG_TO_SEND).name}--${a.localState[Message](MSG_TO_SEND).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    /**
     * For logging a "send" that is a reply to a received message
     *
     * @param m message received
     * @param a agent receving it
     */
    def logReply(m: Message, a: Agent): Unit = println(s"REPLY:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${a.localState[Message](MSG_TO_SEND).payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    /**
     * For logging a send of a response message (to client)
     *
     * @param m message received
     * @param a agent receving it
     */
    def logWriteResponse(m: Message, a: Agent): Unit =
      println(s"W_RES:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")

    def logReadResponse(m: Message, a: Agent): Unit =
      println(s"R_RES:\t ${a.localState[Agent](DST_AGENT).name} <-- ${a.name}\t (${a.localState[Message](MSG_TO_SEND).name}--${m.payload.takeRight(2).head} -- ${a.localState[Map[Any, Any]](REGISTER)(KEY)})")


    (0 until numOfAgents) map { x: Int =>
      val agent = new Agent(s"dude$x")
      agent.localState(REGISTER) = Map[Any, Any](KEY -> "0") // use only one k-v pair!
      agent.localState(PEERS_COUNT) = numOfAgents
      agent.localState(WRITES_ACKS) = Map[String, (Int, Int, Int, Any)]()
      agent.localState(READS_ACKS) = Map[String, (Int, Int, Int, Any)]() // clientName -> numOfAcks to ack the Invocation
      agent.localState(DST_AGENT) = "" // just declare it

      //------------------------------------
      // implement majority-write (m,action)
      //------------------------------------
      val writeAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast replicate request to all
       */

      writeAction + Statement{(m: Message, a:Agent) =>
        a.localState(W_REQ_ID) = UUID.randomUUID().toString.takeRight(4).toUpperCase
        if(log) logInvokeWrite(m,a)
        a.localState(REGISTER) = Map[Any, Any](KEY -> m.payload.last)
        a.localState(WRITES_ACKS) = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS) + (m.sender.name -> (1, 0, 1, m.payload.last))
        for(dst <- a.ds.agents - a) {
          val msg = createWriteReplicaMessage(m, a)
          a.ds.send(a,msg,dst)
          if(log) logSend(m,a)
        }
      }

      //------------------------------------
      // replicate (m, action)
      //------------------------------------
      val replicateAction = new Action

      replicateAction + Statement{(m:Message,a:Agent) =>
        if(log) logReceived(m,a)
        a.localState(REGISTER) = Map(KEY -> m.payload[String](1))
        val msg = createWriteReplicaAckMessage(m,a)
        a.ds.send(a, msg, m.sender)
        if(log) logReply(m,a)
      }

      //------------------------------------
      // ack-replicate (m, action)
      //------------------------------------
      val replicateAckAction = new Action

      /*
      1- if sameRound,
        - increment acks received count
        - if sameValue, increment sameValueAcks-count
      2- if reachedMajority, write value to register and send writeAck to client AND remove clientID from writeMap
      3- if acks-count == peer-count && !reachedMajority && writesMap.contains(clientID), then:
          a. reset everything but round && increment sameValueCount and acksCount
          b. increment round
          c. initialize agents iterator
          c. broadcast replication request
       */
      def writeAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[WRITES_ACKS_TYPE](WRITES_ACKS).contains(m.payload.last.toString.trim)
        ans
      }

      def reachedMajorityWrite(m: Message, a: Agent) = getReplicaWriteAcksCount(m, a) == a.localState[Int](PEERS_COUNT)

      def isSameWriteValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload[Any](1).toString.trim == getWriteValue(m, a).toString.trim
        ans
      }

      def reachedWritePeersCount(m: Message, a: Agent): Boolean = {
        val ans = getWriteAcksCount(m, a).toString.trim == a.localState[Int](PEERS_COUNT).toString.trim
        ans
      }


      replicateAckAction + Statement{(m:Message,a:Agent) =>
        if(log) logReceived(m,a)
        if(writeAcksMapContainsClient(m, a) && isSameWriteRound(m, a)){
          incrementReceivedWriteAcksCount(m,a)
          if(isSameWriteValue(m,a)) incrementWriteSameValueAcksCount(m,a)
        }
        if(writeAcksMapContainsClient(m, a) && reachedMajorityWrite(m, a)){
          val msg = createWriteAckMessage(m,a)
          val dst = getClient(m,a)
          a.ds.send(a,msg,dst)
          if(log) logWriteResponse(m,a)
          a.localState(WRITES_ACKS) = removeClientFromWritesMap(m,a)
        }
      }

      //------------------------------------
      // implement majority-read (m,action)
      //------------------------------------
      // reset the count of that specific client (each client issues a single request at a time)
      val readAction = new Action

      /*
      STEPS:
      1- init count for this client
      2- broadcast read requests to all
      */

      readAction + Statement{(m:Message,a:Agent)=>
        a.localState(R_REQ_ID) = UUID.randomUUID().toString.takeRight(4).toUpperCase()
        if(log) logInvokeRead(m,a)
        a.localState(READS_ACKS) = a.localState[READS_ACKS_TYPE](READS_ACKS) + (m.sender.name -> (1, 0, 1, getRegisterVal(a)))
        for(dst <- a.ds.agents - a){
          val msg = createReadReplicaMessage(m,a)
          a.ds.send(a,msg,dst)
          if(log) logSend(m,a)
        }
      }

      //------------------------------------
      // read replicas (m,action)
      //------------------------------------
      val readReplicaAction = new Action // performed by replica upon receiving ReadReplicaMsg

      readReplicaAction + Statement{(m:Message,a:Agent)=>
        if(log) logReceived(m,a)
        val msg = createReadReplicaAckMessage(m,a)
        a.ds.send(a,msg,m.sender)
        if(log) logReply(m,a)
      }

      //------------------------------------
      // read replica ack (m,action)
      //------------------------------------
      val readReplicaAckAction = new Action

      /*
     1- if sameRound,
       - increment acks received count
       - if sameValue, increment sameValueAcks-count
     2- if reachedMajority, send readAck to client AND remove clientID from readMap
     3- if acks-count == peer-count && !reachedMajority && readsMap.contains(clientID), then:
         a. reset everything but round && increment sameValueCount and acksCount
         b. increment round
         c. initialize agents iterator
         c. broadcast read-replica requests
      */

      def readAcksMapContainsClient(m: Message, a: Agent) = {
        val ans = a.localState[Map[String, (Int, Int, Int, Any)]](READS_ACKS).contains(m.payload.last.toString)
        ans // for debugging
      }

      def reachedMajorityRead(m: Message, a: Agent) = {
        val ans = getSameReadValueCount(m, a).toString.trim == a.localState[Int](PEERS_COUNT).toString.trim
        ans
      }

      def isSameReadValue(m: Message, a: Agent): Boolean = {
        val ans = m.payload.head.toString.trim == getRegisterVal(a).toString.trim
        //getReadValue(m,a).toString.trim
        ans
      }

      def reachedReadPeersCount(m: Message, a: Agent): Boolean = getReadAcksCount(m, a) == a.localState[Int](PEERS_COUNT)

      def canRetryMore(m: Message, a: Agent): Boolean = numOfRetries <= getReadRound(m, a)

      // logged, i am just re-using it

      readReplicaAckAction + Statement{(m:Message,a:Agent) =>
        if(log) logReceived(m,a)
        if(readAcksMapContainsClient(m, a) && isSameReadRound(m, a)){
          incrementReceivedReadAcksCount(m,a)
          if(isSameReadValue(m,a)) incrementReadSameValueAcksCount(m,a)
        }
        if(readAcksMapContainsClient(m, a) && reachedMajorityRead(m, a)){
          val msg = createReadAckMessage(m,a)
          val dst = getClient(m,a)
          a.ds.send(a,msg,dst)
          if(log) logReadResponse(m,a)
          removeClientFromReadsMap(m,a)
        }
        if(readAcksMapContainsClient(m, a) && !reachedMajorityRead(m, a) && reachedReadPeersCount(m, a)){
          resetReadAcksCount(m,a)
          incrementReceivedReadAcksCount(m,a)
          resetSameReadValueCount(m,a)
          incrementReadSameValueAcksCount(m,a)
          incrementReadRound(m,a)
          setReadValue(m,a)
          for(dst <- a.ds.agents - a){
            if(canRetryMore(m, a)){
              val msg = createReadReplicaRetryMessage(m, a)
              a.ds.send(a, msg, dst)
              if (log) logSend(m, a)
            }
          }
        }
      }

      //------------------------------------
      // adding reactions
      //------------------------------------
      agent.reactions += (msgWrite -> writeAction) // invoked by client
      agent.reactions += (msgReplication -> replicateAction) // invoked by write-init-server
      agent.reactions += (msgReplicationAck -> replicateAckAction) // invoked by replicas, write ack is sent by this action to client
      agent.reactions += (msgRead -> readAction) // invoked by client
      agent.reactions += (msgReadReplica -> readReplicaAction) // invoked by read-init server, contacts peer-servers to read from
      agent.reactions += (msgReadReplicaAck -> readReplicaAckAction) // replies (read-ack) to client upon majority-read complete

      agent.defaultBehavior = agent.reactions

      ds + agent
    }

    // update the peersCount in all agents (this is basically done in the initialization code of each agent, or periodically)
    ds.agents foreach { x => x.localState(PEERS_COUNT) = ds.agents.filterNot(_.name.startsWith(ired.name)).size }

    // produce harness file
    import edu.utah.cs.gauss.serialization.IO.{appendSeqToFile, appendToFile, deleteFile}
    val nl = "\n"

    if (new File(harnessFileToWritePath).exists()) deleteFile(harnessFileToWritePath)

    appendToFile(harnessFileToWritePath,
      REGISTER, "MAP") // a map is also a multi register, so a single register can be represented as a map with a single key
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath, """dude\d+""") // regex for ADT agents (i.e. the cluster)
    appendToFile(harnessFileToWritePath, "") // newline
    appendSeqToFile(harnessFileToWritePath, ds.agents.filterNot(_.name.startsWith(ired.name)).map { x => x + ", " }.toSeq)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      s"write, ${msgWrite.name}, k, 1", // add operations here (harness)
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 2",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 3",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 4",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 5",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k",
      s"write, ${msgWrite.name}, k, 6",
      s"read, ${msgRead.name}, k",
      s"read, ${msgRead.name}, k")
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      msgRead.name + ", " + msgReadAck.name, // add response messages regexes
      msgWrite.name + ", " + msgWriteAck.name)
    appendToFile(harnessFileToWritePath, "") // newline
    appendToFile(harnessFileToWritePath,
      "write , 0, w",
      "replication , 0, w",
      "replicationAck , 0, w", // in this implementation, this one definitely WILL affect the state in question at the originator
//      "writeAck , 0, w",
      "read , 0, r",
      "msgReadReplica , 0, r",
      "msgReadReplicaAck , 1, r"
//      "readAck , 0, r"
    ) // newline

    ds.refresh
    (ds, HARNESS_FILE) // return it
  } // dist-register with majority read/write

}
