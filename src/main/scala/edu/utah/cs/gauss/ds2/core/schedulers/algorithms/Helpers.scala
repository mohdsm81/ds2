package edu.utah.cs.gauss.ds2.core.schedulers.algorithms

import edu.utah.cs.gauss.Helpers.DecoratorMethods.SetContains
import edu.utah.cs.gauss.ds2.core.ir.datastructures.{Agent, DistributedSystem, LocalState, Message}
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat.HistoryTypes._
import edu.utah.cs.gauss.serialization.IO.readLinesFromFile

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.matching.Regex

/**
 * @author
 * Mohammed S. Al-Mahfoudh <p>
 * mahfoudh@cs.utah.edu <p>
 * Gauss Group - SoC <p>
 * The University of Utah <p>
 *
 */

object Constants {
  val aSet = 'SET
  val aMap = 'MAP
}

object AbstractTypes {
  /**
   * Message sent to the agent is a receive event
   */
  type Receive = (Message, Agent)
  type Schedule = Seq[Receive]
}

trait Helpers {

  import AbstractTypes._
  import Constants._

  def s: Scheduler

  val ired = "IRed"
  val maxHistoryLength: Int

  //  var backtrack: MStack[(SchedulerState, DistributedSystemState)] = MStack()
  var targetAgents: Set[Agent] = Set() // initially we think all agents are targets
  var targetAgentsRegexes: Seq[Regex] = Seq()
  var responseMessagesRgexes: Map[String, Set[Regex]] = Map() // parsed
  var keyIndicesMap: Map[String, Int] = Map[String, Int]()
  var internalAPIMap: Map[String, Boolean] = Map[String, Boolean]()
  var overlapAPISet: Set[String] = Set[String]()

  var harnessFilePath: String = ""
  var harnessMap: MMap[String, String] = MMap() // format msg.name -> Seq(opname, args*)
  var harness: Seq[Message] = Seq()

  var userChoice: Symbol = aSet
  var variable: String = ""
  var startSeq: Seq[String] = Seq()


  var spec: MMap[Any, Any] = MMap()


  var clientID: Int = 0
  // initially just IRed agent
  var currentClient: Agent = new Agent(ired)
  currentClient.locked = false
  var clients: Set[Agent] = Set()


  val modificationAPI: Set[String] =
    Set("append", "write", "put", "cas") union
      Set(":append", ":write", ":put", ":cas") union
      Set("add", "remove") union
      Set(":add", ":remove")

  // necessary for makeInvocation and makeResponse
  val invocationsSet: Set[String] =
    Set("add", "remove", "contains", "read", "get", "write", "put", "append", "cas") union
      Set(":add", ":remove", ":contains", ":read", ":get", ":write", ":put", ":append", ":cas")

  def isModificationCall(m: Message): Boolean = {
    if (m.name in internalAPIMap.keySet)
      internalAPIMap(m.name) || (m.name in overlapAPISet)
    else if (m.name in harnessMap.keySet.toSet) // weird scala package arrangement
      harnessMap(m.name).toLowerCase in modificationAPI
    else false
  }

  def isReadCall(m: Message): Boolean = {
    if (m.name in internalAPIMap.keySet)
      !internalAPIMap(m.name) || (m.name in overlapAPISet)
    else if (m.name in harnessMap.keySet.toSet)
      !(harnessMap(m.name).toLowerCase in modificationAPI)
    else false
  }

  var modificationCalls: Seq[Message] = harness.filter { x => isModificationCall(x) }
  var readingCalls: Seq[Message] = harness.filterNot { x => isModificationCall(x) }

  def createNewClient: Agent = {
    val agent = new Agent(ired + clientID)
    currentClient = agent
    s.ds + agent
    s.ds.unlock(agent)
    clients += agent
    clientID += 1
    agent
  }

  def resetClients: Boolean = {
    s.ds.agents = s.ds.agents -- clients
    val clientsWereRemovedFromDS = !(clients subsetOf s.ds.agents)
    clientID = 0
    clients = Set()
    clientsWereRemovedFromDS && clients.isEmpty && clientID == 0
  }

  def inputFileParser(file: File): Unit = {

    // file format (each comma is a new line of course), block indicates which section we are at now
    // datastructure, emptyline, regexes, emptyline, start-seq, emptyline, harness-entries

    val lines = readLinesFromFile(file)
    var block = 0

    for {line <- lines} yield {
      line match {
        case x if x == "" => block += 1
        case x if LocalState.isValidVarName(x) => variable = x
        case x if x.toUpperCase == "SET" => userChoice = aSet
        case x if x.toUpperCase == "MAP" => userChoice = aMap
        case x if block == 1 => // target agent(s) regex
          targetAgentsRegexes = targetAgentsRegexes :+ x.r.anchored
        case x if block == 2 => // start sequence
          startSeq = startSeq :+ x
        case x if block == 3 => // test harness(es)
          val splits: Seq[String] = x.split("""(\s*),(\s*)""").map(_.trim).toSeq
          harnessMap(splits.tail.head) = splits.head
          harness = harness :+ new Message(name = splits.tail.head, payload = splits.tail.tail)
        case x if block == 4 => // message mapping
          responseMessagesRgexes = responseMessagesRgexes + (x.trim.split(",")(0) -> x.trim.split(",").tail.map { x => x.trim.r.anchored }.toSet)
        case x if block == 5 =>
          val values = x.split("""(\s*),(\s*)""").map(_.trim)
          keyIndicesMap = keyIndicesMap + (values(0) -> values(1).toInt)
          val isModification = values(2) match {
            case "r" | "R" => false
            case "w" | "W" => true
            case "b" | "B" =>
              overlapAPISet += values(0)
              true
            case _ => false
          }
          internalAPIMap = internalAPIMap + (values(0) -> isModification)
      }
    }
  }

  def filePrompt(): Unit = {

    // TODO uncomment usage documentation after DEBUG

    /*    println("""
    The formate of a file input for the tool is as the following:
    <variable-containing-datastructure> // e.g. mymap$$Map[Any,Any] (dollar signs are the type delimiters)
    <Type-of-data-structure> // SET, MAP
    <empty line/linefeed> // each line is a java regex capturing one or more actor-IDs during runtime
    <agent-id-regex>
    :
    <agent-id-regex>
    <empty line/linefeed> // following is the start sequence with arguments
    <actor-type><COMMA><comma-separated-arguments>
    :
    <actor-type><COMMA><comma-separated-arguments>
    <empty line/linefeed> // following is the test harness (a set of operations to be explored to find linearizability violation)
    <operation-name><COMMA><message-name-representing-operation><COMMA><comma-separated-args> // see below to know what operations are available
    :
    <operation-name><COMMA><message-name-representing-operation><COMMA><comma-separated-args>
    <empty line/linefeed> // the following is the regexes of message names that are considered a response to requests
    <invocation-message>,<response-message-regexes>+ // an invocation message e.g. one mapped to a PUT for example, maps to one or more response regexes that are considered a response to that message
    :
    <invocation-message>,<response-message-regexes>+
    <empty line/linefeed> // following is a bench of where the key is to be found in the message payload (zero-based index), and whether it is a read/write op
    <message-name>, <payload-index>, <r/w/b> // b for both and hence it is added to internalAPIMap AND overlapAPISet as a write-related-call
    :
    <message-name>, <payload-index>, <r/w/b>
    -------------------------------------------------------------------------------------
    NOTE: The following are the operations signatures available for each data structure:

    SET:
         add, msg-label, x
         remove, msg-label, x
         contains, msg-label, x
    MAP:
         read, msg-label, k
         get, msg-label, k
         write, msg-label k,v
         put, msg-label, k,v
         cas, msg-label, k,v-expected,v-intended
    """)*/

    //    println("Please enter the path of the test harness file: ")
    //    val filePath = readLine
    val file = new File(harnessFilePath)
    if (!file.exists()) throw new Error("File doesn't exist.")
    inputFileParser(file)
  }


  /**
   * Takes a filtered set of agents, namely the target ADT agants WITHOUT the dynamically generated clients
   *
   * @param agents the target agents that represent the distributed ADT
   * @return the initialized shard of the ADT
   */
  def getInitializedSpecFromAgentDataStructure(agents: Set[Agent]): MMap[Any, Any] = {
    // SET or MAP, always a Map.
    var agentState = MMap.empty[Any, Any]

    if (userChoice == aMap) {
      val agentMap: Any = if (agents.nonEmpty) agents.head.localState[Any](variable) else MMap()

      agentMap match {
        case theMap: Map[_, _] =>
          theMap.toList.foreach { x => agentState(Some(x._1)) = x._2 }
        case _: MMap[_, _] =>
          agentState = agentMap.asInstanceOf[MMap[Any, Any]].map { x => Some(x._1) -> x._2 }
        case tuple: (_, _) =>
          agentState = MMap[Any, Any](Some(tuple._1) -> tuple._2)
        case _ =>
          agentState = MMap[Any, Any](Some("k") -> agentMap)
      }
    } // end if a map
    else { // must be a set
      val agentSet: Any = if (agents.nonEmpty) agents.head.localState[Any](variable) else MMap()
      agentSet match {
        case _: Set[_] =>
          agentSet.asInstanceOf[Iterable[_]].foreach { x => agentState(Some(x)) = x }
        case _: MSet[_] =>
          agentSet.asInstanceOf[mutable.Iterable[_]] foreach { x => agentState(Some(x)) = x }
        case theMap: Map[_, _] =>
          theMap.foreach { x => agentState(Some(x)) = x }
        case theMap: MMap[_, _] =>
          theMap foreach { x => agentState(Some(x)) = x }
        case iterable: Iterable[_] => // a list of items
          iterable.toSet foreach { x: Any => agentState(Some(x)) = x }
        case _ => // mutable list of items
          agentSet.asInstanceOf[mutable.Iterable[Any]].toSet.foreach { x: Any => agentState(Some(x)) = x }
      }
    } // end else (set)
    agentState
  }

  /**
   * Starts all target agents whose types have a start sequence
   * provided. That is, sends a start message with the appropriate
   * payload, schedules the 'start' task for each, and execute these
   * tasks.
   */
  def startAllAgentsUsing(sch: Scheduler): Unit = {
    targetAgents = sch.ds.agents.filter { x =>
      targetAgentsRegexes.exists { r =>
        //        val ans = r.findFirstMatchIn(x.name).isDefined
        val ans = r.pattern.matcher(x.name).matches
        ans
      }
    }

    // <actor-type><COMMA><comma-separated-arguments>
    val startToAgentsMap: Map[String, Seq[String]] = startSeq.map { x => (x.split("""(\s*),(\s*)""").head, x.split("""(\s*),(\s*)""").tail.toSeq) }.toMap
    val toStartAgents = targetAgents
    toStartAgents.foreach { x =>
      val msg = edu.utah.cs.gauss.ds2.core.ir.datastructures.Start()
      msg.payload = startToAgentsMap(x.name).asInstanceOf[mutable.WrappedArray[String]]
      sch.ds.bootStrap(x, msg.payload)

      //      while(sch.ds.hasWork.nonEmpty) { // new addition to enable complete startup
      sch.schedule(x)
      sch.consumeATask(x)
      sch.executeAll(0)
      //      }
    }

    /*
     note how we start each agent, we don't explore the interleaving
     WHILE starting the agent. We just start each (i.e. each is ready
     to receive requests) then we start the normal work of the IRed
     scheduler.
     */
  }

  def makeReceiveFor(a: Agent, harness: Seq[String]): Receive = {
    val msg = new Message(harness(1))
    msg.sender = currentClient
    msg.payload = harness.tail.tail
    (msg, a)
  }

  def makeReceiveFor(a: Agent, msg: Message): Receive = {
    msg.sender = currentClient
    (msg, a)
  }

  def isInvocation(m: Message): Boolean = m.name in harnessMap.keySet.toSet // compiler stupidity

  def isResponse(m: Message): Boolean = {
    val notInvocation = !isInvocation(m)
    val regexMatchExists = if(notInvocation) responseMessagesRgexes.exists {
      case (_, respRegexSet) => respRegexSet.exists { x =>
//        x.findFirstIn(m.name).isDefined
        x.pattern.matcher(m.name).matches
      }
    } else false

    notInvocation && regexMatchExists
  } // end isResponse

  def makeInvocation(message: Message, agent: Agent): Event = {
    /*
    EXAMPLES:

    SET:
     add, msg-label, x
     remove, msg-label, x
     contains, msg-label, x
    MAP:
     read, msg-label, k
     get, msg-label, k
     write, msg-label k,v
     put, msg-label, k,v
     cas, msg-label, k,v-expected,v-intended
    */

    val opname = harnessMap(message.name) // opname
    val key = message.payload.head.toString
    val args = message.payload.tail.map(_.toString).toList

    if (opname in invocationsSet)
      Invocation(Some(key), message.sender.name, opname, args)
    else
      throw new Error("makeInvocation - Can not create operation: " + opname)
  }

  def findOperationNameForResponse(m: Message): String = {
    val found = responseMessagesRgexes.filter {
      case (_, v) => v.exists { r =>
//        r.findFirstMatchIn(m.name).isDefined
        r.pattern.matcher(m.name).matches
      }
    }
    found.head._1
  }

  def makeResponse(message: Message, agent: Agent): Event = {
    val opname = findOperationNameForResponse(message)
    val key = message.payload.head.toString
    val args = message.payload.tail.map{
      case null => ""
      case x => x.toString
    }.toList

//    if (opname in invocationsSet)
    if (opname in invocationsSet)
      Response(Some(key), agent.name, opname, args)
    else
      throw new Error("makeResponse - Can not create operation: " + opname)
  }


  def insertAfter(target: Event,
                  toInsert: Event,
                  r: Receive = null): Unit = Event.insertAfter(target, toInsert)

  def step(r: Receive,
           hist: Event,
           insertAfterMethod: (Event,
             Event,
             Receive) => Unit): Unit = {

    val m = r._1
    val a = r._2
    var counter = 0

    if (isInvocation(m)) {
      insertAfterMethod(
        hist.last,
        makeInvocation(m, a),
        r)
      counter += 1
    } // end if-inv

    s.ds.send(s.ds.get(m.sender.name), m, s.ds.get(a.name)) // IRed is the one that sends requests/invocations (the scheduler)
    s.schedule(a)
    s.consumeATask(a)
    s.executeWhile(0) {
      true
    } // stops when s.consumeQ.isEmpty, doesn't deal with blocking (future improvement)

    // execute generated work (to stabilize the cluster while each task executed you check for a response in ired.q)
    while (
      s.ds.hasWork.exists { x => !x.name.startsWith(ired) } &&
        counter < maxHistoryLength) {
      s.ds.hasWork.filterNot(_.name.startsWith(ired)).foreach { a =>
        s.schedule(a)
        s.consumeATask(a)
        s.executeWhile(0)(condition = true)
      } // end-map


      val clientsIter = clients.filterNot(_.q.isEmpty).iterator
      while (clientsIter.hasNext) {
        // discarding the non-response messages from IRed clients
        currentClient = clientsIter.next
        currentClient.q = currentClient.q filter isResponse

        // create responses and append them to history
        currentClient.q.foreach { x =>
          counter += 1
          insertAfterMethod(hist.last,
            makeResponse(x, currentClient),
            r)
        }
        currentClient.q = Seq() // empty client queue (we are done with all those messages already)
      }
    } // end WHILE
  } // end of STEP

  def filterTargetAgents(ds: DistributedSystem): Set[Agent] = {
    ds.agents.filterNot(_.name.startsWith(ired))
    ds.agents.filter { x =>
      targetAgentsRegexes.exists { r =>
        //        val ans = r.findFirstMatchIn(x.name).isDefined
        val ans = r.pattern.matcher(x.name).matches
        ans
      }
    }
  }

  /**
   * The method takes a schedue and re assigns agents to both the receiver agent and the sender 
   * agent of the message after snapshotting and restoring to a new DistributedSystem object. This 
   * is in order for the receivers to be of the new distributed system not the old one.
   *
   * @param sch the schedule with the agent references from the old distributed system object
   * @param ds  the NEW distributed system that will be manipulated by the scheduler.
   * @return the new copy of the schedule but with references from the current distributed system 
   *         referenced by the <code>ds</code> field of the scheduler.
   */
  def reAssignNewAgents(sch: Schedule, ds: DistributedSystem): Schedule = {
    sch map { r =>
      val m: Message = r._1 // still has the old reference of the old ds.clientAgent
      m.sender = null
      val receiver: Agent = ds.get(r._2.name)
      (m, receiver)
    }
  }

  def liftInternalReceives: Map[Agent, Schedule] = {
    var schedules: Map[Agent, Schedule] = Map()
    s.ds.agents.filter(_.hasWork).foreach { agent =>
      var internalEvents = agent.q.filter(!isInvocation(_))
      agent.q = agent.q.filterNot(!isInvocation(_))
      schedules += agent -> (schedules(agent) ++ internalEvents.map { m => (m, agent) })
    }
    schedules
  }

  //  def push(): Unit = backtrack.push((s.snapshot, s.ds.snapshot))
  //
  //  def pop(): Unit = {
  //    backtrack.top._1.restore
  //    backtrack.pop._2.restore
  //  } // reset state

  def isClient(agent: Agent): Boolean = {
    clients.contains(agent)
  }

  def lessThanHistorySizeLimit(hist: Event): Boolean = {
    hist.size < maxHistoryLength
  }
}
