package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.linearizability.history.flat

import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVector
import edu.utah.cs.gauss.ds2.core.time.versionvectors.DottedVersionVectorTypes._

import scala.collection.mutable.ArrayBuffer

/**
 * @author <br>
 *         Mohammed S. Al-Mahfoudh <br/>
 *         mahfoudh@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 */

object HistoryTypes {

  type Entry = Event
  type History = Event
  type Inv = Invocation
  type Res = Response

  //========================================
  //  The Event hierarchy follows
  //========================================
  object DataStructure extends Enumeration {
    type DataStructure = Value
    val SET, MAP, REG = Value
  }

  object Event {


    val DUMMY_HEAD_NAME = "DUMMY HEAD - padding"

    // old check
    //    def isComplete(hist: Event): Boolean = {
    //      val (invocations, responses) = Event.historyToSeq(hist.copyAll.head).partition(_.isInvocation)
    //
    //      val cond1 = invocations.size == responses.size
    //      val cond2 = invocations forall { x => responses.exists(x.matches(_)) }
    //
    //      //      Event.makeHistory(Event.historyToSeq(hist)) // in order to reconnect the doubly list
    //      cond1 && cond2
    //    }

    // new check
    def isComplete(hist: Event): Boolean = {
      val (invocations, responses) = Event.historyToSeq(hist.copyAll.head).partition(_.isInvocation)

      val cond1 = invocations.size == responses.size
      val cond2 = invocations forall { x =>
        responses.exists { y =>
          val result = x.matches(y)
          result
        }
      }

      //      Event.makeHistory(Event.historyToSeq(hist)) // in order to reconnect the doubly list
      cond1 && cond2
    }


    def seqMatchOf(historyAsSequence: Seq[Event], toMatchEntry: Event): Option[Event] = {
      val current = toMatchEntry
      var theMatch = if (current.isInvocation) {
        val range = historyAsSequence.splitAt(historyAsSequence.indexOf(toMatchEntry))._2
        if (range.nonEmpty) range.tail.find { x => toMatchEntry.matches(x) }
        else None
      }
      else None
      theMatch
    }

    def doComplete(hist: Event): Event = {
      import scala.collection.mutable.{Seq => MSeq}

      var dummyHandle = hist.head
      val history = MSeq(Event.historyToSeq(hist.copyAll): _*)
      var indexToInsert = history.size
      val resultHistory = MSeq.fill[Option[Event]](history.size * 2)(None)

      history.map { x =>
        seqMatchOf(history, x) match {
          case Some(r) =>
            resultHistory(history.indexOf(x)) = Some(x)
            resultHistory(history.indexOf(r)) = Some(r)
            history(history.indexOf(x)) = Info(Some("Whatever"), "whatever", "whatever", Seq("whatever")) // removing the invocation
            history(history.indexOf(r)) = Info(Some("Whatever"), "whatever", "whatever", Seq("whatever")) // removing the matching response
          case None if x.isInvocation =>
            resultHistory(history.indexOf(x)) = Some(x)
            history(history.indexOf(x)) = Info(Some("Whatever"), "whatever", "whatever", Seq("whatever")) // removing the invocation
            resultHistory(indexToInsert) = Some(WildCardResponse())
            indexToInsert = indexToInsert + 1
          case None if x.isResponse =>
            resultHistory(history.indexOf(x)) = Some(x)
            history(history.indexOf(x)) = Info(Some("Whatever"), "whatever", "whatever", Seq("whatever")) // removing the response
          case _ => // do nothing
        }
      }

      //      assert(resultHistory.flatten.size % 2 == 0)

      if (!dummyHandle.isInvocation) resultHistory(0) = Some(dummyHandle)

      Event.makeHistory(resultHistory.flatten)
    }


    def getPadding: Event = {
      val padding = new Info(None, DUMMY_HEAD_NAME, DUMMY_HEAD_NAME, Seq())
      padding.setID(-1)
      padding
    }

    /**
     * Inserts the node <code>toInsert</code> before the <code>target</code> node
     * in a doubly linked list of entries/events.
     */
    def insertBefore(target: Entry, toInsert: Entry): Unit = {
      toInsert.setNext(target)
      toInsert.setPrev(target.getPrev)

      if (target.getPrev != null)
        target.getPrev.setNext(toInsert)
      target.setPrev(toInsert)
    }

    /**
     * Inserts the node <code>toInsert</code> after the <code>target</code>
     * node in a doubly linked list entries/events.
     */
    def insertAfter(target: Entry, toInsert: Entry): Unit = {
      if (target.getNext != null)
        target.getNext.setPrev(toInsert)
      toInsert.setNext(target.getNext)
      toInsert.setPrev(target)
      target.setNext(toInsert)
    }

    def remove(entry: Entry): Unit = {
      if (entry.getPrev != null)
        entry.getPrev.setNext(entry.getNext)
      if (entry.getNext != null)
        entry.getNext.setPrev(entry.getPrev)

      entry.setNext(null)
      entry.setPrev(null)
    }

    def stripOffPadding(history: Event): Event = {
      var start = history.head
      while (start == getPadding) {
        start = start.getNext
        remove(start.getPrev)
      }
      start
    }


    // /**
    //  * Removes the entry <code>entryToRemove</code> from the doubly linked list
    //  * of entries/events and returns the new head of the list if not empty,
    //  * <code>null</code> otherwise.
    //  */
    // def removeEntry(entryToRemove: Entry): Entry = {
    //   var pointerToHead: Entry = null

    //   if (entryToRemove.getNext != null) {
    //     pointerToHead = entryToRemove.getNext // take note of it for now

    //     entryToRemove.getNext.setPrev(entryToRemove.getPrev)
    //     entryToRemove.setNext(null)
    //   }
    //   if (entryToRemove.getPrev != null) {
    //     entryToRemove.getPrev.setNext(pointerToHead)
    //     pointerToHead = entryToRemove.getPrev
    //     entryToRemove.setPrev(null)
    //   }

    //   if (pointerToHead != null)
    //     pointerToHead.head
    //   else
    //     pointerToHead // null
    // }

    /**
     * Takes a sequence of atom-events and links their next+previous pointers
     * according to their appearance in the <code>events</code> sequence.
     */
    def makeHistory(events: Seq[Event]): Event = {

      val filteredEvents = events // .filterNot { x => x.isInfo || x.isFail }

      var event = filteredEvents.head

      val size = filteredEvents.size
      var counter = 0

      if (size < 2) return event

      while (counter <= size - 1) {
        if (counter == 0) {
          event.setNext(filteredEvents(1)) // event.prev == null
          event.setPrev(null)
        }
        else if (counter == size - 1) {
          event.setNext(null)
          event.setPrev(filteredEvents(counter - 1))
        }
        else {
          event.setPrev(filteredEvents(counter - 1))
          event.setNext(filteredEvents(counter + 1))
        }
        counter += 1
        if (counter <= size - 1)
          event = event.getNext
      } // event.last.next == null

      event.head
    }

    //    def makeHistory(events: Seq[Event]): Event = {
    //      val filtered = events.filterNot(x => x.isInfo || x.isFail)
    //      if(filtered.size < 2) filtered.head
    //
    //      var event = filtered.head
    //      for(e <- events.tail){
    //        insertAfter(event,e)
    //        event = e
    //      }
    //      event.first
    //    }

    /**
     * Partitions the sequence of events to subhistories based on key, i.e. one
     * key per sub history, and keeps the relative order of events as is.
     */
    def partitionedHistories(events: Seq[Event]): Map[Option[Any], Event] = {
      val filteredEvents = events.filterNot { x => x.isInfo || x.isFail }
      filteredEvents.groupBy(_.getKey).mapValues { ev =>
        Event.makeHistory(ev)
      }
    }

    def historyToSeq(history: Event): Seq[Event] = {

      var event = history.head
      var buffer = Seq[Event]()
      var nxt: Event = null
      while (event != null) {
        nxt = event.getNext
        buffer = buffer :+ event
        event.setNext(null)
        event.setPrev(null)
        event = nxt
      }
      buffer
    }

    //========================================
    //  Extra event attributes
    //========================================

    def lift(entry: Entry): Unit = { // shamelessly copied verbatim from the paper
      entry.getPrev.setNext(entry.getNext)
      entry.getNext.setPrev(entry.getPrev)
      entry.setMatch(matchOf(entry))
      entry.getMatch.getPrev.setNext(entry.getMatch.getNext)
      if (entry.getMatch.getNext != null)
        entry.getMatch.getNext.setPrev(entry.getMatch.getPrev)
    }

    def unlift(entry: Entry): Unit = { // shamelessly copied verbatim from the paper
      val theMatch = entry.getMatch
      theMatch.getPrev.setNext(theMatch)
      if (theMatch.getNext != null)
        theMatch.getNext.setPrev(theMatch)
      entry.getPrev.setNext(entry)
      entry.getNext.setPrev(entry)
      entry.setMatch(null)
    }

    def matchOf(entry: Entry): Event = {
      require(entry != null, "the argument of matchOf can't be null")

      if (entry.isResponse) return null

      var event: Event = entry.getNext // a match is one of next entries that matches the criteria
      while (event != null && !entry.matches(event))
        event = event.getNext
      event
    }

    def partitionByKey(entry: Entry): Map[Option[Any], Event] = partitionedHistories(Event.historyToSeq(entry))

    //    /**
    //     * Wraps those events that are causally related in CausalChains, modifying
    //     * the history to a coarser-grained one, returns <code>true</code> if it
    //     * found a single causal chain of length more than ONE event, <code>false</code>
    //     * otherwise (if all causal chains are of length ONE event).
    //     */
    //
    //    def makeCausal(history: Event): Event = {
    //      /*
    //       STEPS:
    //       - end of history event
    //       - backwards, scan each DVV of previous events
    //         + if prev.DVV
    //         happensBefore current.DVV, prepend as causal chain element
    //         + else prev = prev.getPrev
    //       - After prev == null (i.e. first event in history):
    //         + re-arrange causaly equivalent events in the causal chain so that:
    //           1- the least of the DVV is in the beginning and the max is
    //           at the end of the causal chain
    //       
    //           2- If there are two causal equivalen events such that (e1
    //           concurrentWith e2) then one ordering matters only if one of
    //           them is closer to next event DVV values (i.e. causally closer to it)
    //
    //           3- Think and figure: is it possible to implement causal
    //           chains so that it moves internal events as required to
    //           match a call/response? Does this even have significance of
    //           any sorts?
    //       */
    //
    //      var current = history.last
    //      var prev = current.getPrev
    //
    //      var chainBuffer = ArrayBuffer[Event]()
    //      var newHistoryBuffer = ArrayBuffer[Event]()
    //
    //      var restart: Boolean = true
    //
    //      while (current != null) {
    //        restart = true
    //        while (prev != null) {
    //          println("0000!")
    //          if ((prev.getDVV happensBefore current.getDVV) &&
    //            prev.getKey == current.getKey && // because causality respects partitions, it is meant to bind multi-process causality order
    //            !prev.wasSelectedBefore) {
    //            if (restart) { // enters this one only once per outer-while iteration
    //              prev.setSelectedBefore(true)
    //              chainBuffer.prepend(current)
    //              chainBuffer.prepend(prev)
    //              restart = false
    //            }
    //            else {
    //              prev.setSelectedBefore(true)
    //              chainBuffer.prepend(prev)
    //            }
    //          } // end if
    //          prev = prev.getPrev
    //
    //        } // inner while
    //        println("1111!")
    //
    //        if (chainBuffer.size >= 1) // if not empty
    //          newHistoryBuffer.prepend(CausalChain(makeHistory(chainBuffer.toSeq)))
    //
    //        println("size of chain: " + chainBuffer.size)
    //        chainBuffer.clear // clear the chain buffer to construct the next chain
    //
    //        current = current.getPrev // only causal-ize those that weren't selected by previously constructed causal chains          
    //      } // outer while
    //
    //      println("2222!")
    //      // the last remaining events must also be prepended if there is any that was not selected before
    //      current = history.last
    //      while (current != null) {
    //        println("3333!")
    //        if (!current.wasSelectedBefore)
    //          // yes no need to flag them since they are not part of a causal chain
    //          newHistoryBuffer.prepend(current)
    //        current = current.getPrev
    //      }
    //      println("4444!")
    //      makeHistory(newHistoryBuffer.toSeq)
    //    }

    // code makeUnCausal only if needed
    //    /**
    //     * Reverts the effect of <code>makeCausal</code>, i.e. makes the history
    //     * flat sequence of events again (aka great again).
    //     */
    //    def makeUnCausal(history: Event): Event = {
    //      var current = history
    //      var flatHistoryBuffer = ArrayBuffer[Event]()
    //      while (current != null) {
    //        current match {
    //          case CausalChain(chain) =>
    //            historyToSeq(chain).map { x =>
    //              x.setSelectedBefore(false) // reset the flag not to confuse next makeCausal call
    //              flatHistoryBuffer.append(x)
    //            }
    //          case x: Event =>
    //            x.setSelectedBefore(false) // reset the flag not to confuse next makeCausal call
    //            flatHistoryBuffer.append(x)
    //        }
    //        current = current.getNext
    //      }
    //      makeHistory(historyToSeq(makeHistory(flatHistoryBuffer.toSeq)))
    //    }

    def makeCausal(history: Seq[Event]): Event = {
      val filtered = history.filterNot { x => x.isInfo || x.isFail }
      var hist = filtered.reverse
      var idx = -1
      val subHist = ArrayBuffer[Event]()
      val causalHist = ArrayBuffer[Event]()


      def nextIdx: Int = hist.indexWhere(!_.wasSelectedBefore)

      hist.map {
        event =>
          idx += 1
          val current = hist(idx)
          subHist.appendAll(hist.takeRight(hist.size - nextIdx).filter { prev =>
            (prev.getDVV happensBefore current.getDVV) && // current descends itself too
              prev.getDVV.nodeName == current.getDVV.nodeName &&
              prev.getKey == current.getKey &&
              !prev.wasSelectedBefore
          })
          subHist.map { x => x.setSelectedBefore(true) }

          // DEBUG
          // if (subHist.size > 0)
          //   println(subHist.toSeq.reverse)

          if (subHist.size > 0)
            causalHist.append(CausalChain(makeHistory(subHist.toSeq.reverse)))

          subHist.clear()
      }
      makeHistory(causalHist)
    }

    //==============================================
    // Cloning
    //==============================================
    def copy(entry: Entry): Event = entry.copy

  }

  /**
   * The top Event/Entry class.
   * The main reason why every variable is declared protected is that all should
   * be accessible through a getter, so that we can override these getters for
   * CausalChains appropriately.
   */
  trait Event {
    var key: Option[Any] = None
    var pid: String = ""
    var operation: String = ""
    var args: Seq[String] = Seq()
    //========================================
    //  Extra event attributes
    //========================================
    protected var dvv: DVV = DottedVersionVector("")

    import Event._

    protected var id: Int = -1
    protected var next: Event = null
    protected var prev: Event = null
    protected var theMatch: Event = null

    //========================================
    //  hashCode and equals
    //========================================
    //    override def hashCode: Int = {
    //      dvv.hashCode() +
    //        key.hashCode() +
    //        pid.hashCode() +
    //        args.hashCode() +
    //        operations.hashCode() +
    //        getClass.getSimpleName.hashCode()
    //    }

    //========================================
    //  util
    //========================================
    def take(n: Int): String = {
      var sb = ""
      var entry = this
      var count = 0
      while (count < n && entry != null) {
        sb ++= (entry.toPorcupineEntry + "\n")
        entry = entry.getNext
        count += 1
      }
      sb
    }

    //========================================
    //  output back to log format
    //========================================
    def toPorcupineEntry: String = {

      val theArgs = args.head match {
        case "nil" => "nil"
        case x => "\"" + x + "\""
      }

      if (isInvocation)
        s"""{:process $pid, :type :invoke, :f $operation, :key "${key.get}", :value ${theArgs}}"""
      else if (isInfo)
        s"""{:process $pid, :type :info, :f $operation, :key "${key.get}", :value ${theArgs}}"""
      else if (isFail)
        s"""{:process $pid, :type :fail, :f $operation, :key "${key.get}", :value ${theArgs}}"""
      else if (isResponse)
        s"""{:process $pid, :type :ok, :f $operation, :key "${key.get}", :value ${theArgs}}"""
      else
        throw new Error("Can't string-ify this event")
    }

    def toPorcupineLog: String = {
      var entry = head
      val sb = StringBuilder.newBuilder
      while (entry != null) {
        sb.append(entry.toPorcupineEntry + "\n")
        entry = entry.getNext
      }
      sb.toString
    }

    //    def toJepsenLog: String = {
    //
    //    }


    override def equals(that: Any): Boolean = hashCode() == that.hashCode()

    //========================================
    //  Cloning interface
    //========================================
    def copy: Event

    def copyAll: Event = {
      var event = this
      var result: Seq[Event] = Seq()
      while (event != null) {
        result = result :+ event.copy
        event = event.getNext
      }
      makeHistory(result)
    }

    //========================================
    //  CausalChain utilities
    //========================================

    protected var selectedBefore = false

    def setSelectedBefore(flag: Boolean) = selectedBefore = flag

    def wasSelectedBefore: Boolean = selectedBefore

    def simplify: Unit = {
      /* do nothing!*/
    }

    def causalReconstruct: Unit = {
      var event = this
      while (event != null) {
        event.simplify
        event = event.getNext
      }
    }

    def append(entry: Entry): Unit = Event.insertAfter(last, entry)

    //========================================
    //  Event methods
    //========================================
    // the new one
    def reNumber: Unit = {
      var counter: Int = 0
      var event = this
      while (event != null) {
        if (event.isInvocation) {
          event.setID(counter)
          val aMatch = matchOf(event)
          if (aMatch != null)
            aMatch.setID(counter)
          counter += 1
        }
        event = event.getNext
      }
    }

    // a cached matches numbering
    //    def reNumber: MMap[Event, Event] = {
    //      val map = MMap[Event, Event]()
    //      var counter = 0
    //      var event = this
    //      while (event != null) {
    //
    //        event match {
    //          case x: Invocation =>
    //            val aMatch = matchOf(event)
    //            aMatch.setID(counter)
    //            event.setID(counter)
    //            map(event) = aMatch
    //            counter += 1
    //          case x: Response =>
    //            map(event) = null
    //        }
    //        event = event.getNext
    //      }
    //      map
    //    }

    // The Old one
    // def reNumber: Unit = {
    //   var counter: Int = 0
    //   var event = this
    //   while (event != null) {
    //     event.setID(counter)
    //     counter += 1
    //     event = event.getNext
    //   }
    // }

    def isResponse: Boolean = false

    def isInvocation: Boolean = false

    def isFail: Boolean = false

    def isInfo: Boolean = false

    def getMatch: Event = if (theMatch != null) theMatch else {
      theMatch = matchOf(this)
      theMatch
    }

    def setMatch(aMatch: Event): Unit = theMatch = aMatch

    def getDVV: DVV = dvv

    def setDVV(dvv: DVV) = this.dvv = dvv

    def getKey: Option[Any] = key

    def getPID: String = pid

    def getOperation: String = operation

    def getArgs: Seq[String] = args

    def setID(id: Int): Unit = this.id = id

    def getID: Int = id

    def entryID: Int = eventID

    def eventID: Int = id

    def getNext: Event = next

    def setNext(next: Event): Unit = this.next = next

    def getPrev: Event = prev

    def setPrev(prev: Event) = this.prev = prev

    def sameKey(that: Event): Boolean = key == that.getKey

    def size: Int = {
      var count = 0
      var event = this // size of any history should start with the dummy-heads next (the padding first node of history)
      while (event != null) {
        if (!event.isDummyNode) // because if we call it on a padding event it may never be empty
          count += 1
        event = event.getNext
      }
      count
    }

    def matches(that: Event): Boolean = {
      {
        isInvocation && // by definition, only invocations can have matches (responses)
          that.isResponse && // the match has to be a return to a call
          getKey == that.getKey && // i.e. Obj(entry) == Obj(match) in paper definition 2
          getOperation == that.getOperation && // && // i.e. op(e) == op(r) in paper definition 2
          getPID == that.getPID // I added this because: e <_H e' where e is inv and e' is resp
      } || { // wild card response
        isInvocation &&
          that.isInstanceOf[WildCardResponse]
      }
    }

    def isEmpty: Boolean = size == 0

    def last: Event = {
      var event = this
      while (event.getNext != null) event = event.getNext
      event
    }

    def first: Event = {
      var event = this
      while (event.getPrev != null) event = event.getPrev
      event
    }

    def head: Event = first

    override def toString: String = s"${getClass.getSimpleName}(${dvv},${key},${pid},${operation},${args})"

    def isDummyNode: Boolean =
      getKey == None &&
        getOperation == DUMMY_HEAD_NAME &&
        getPID == DUMMY_HEAD_NAME &&
        getArgs == List()

    def mkString(delim: String = "\n"): String = {
      val sb = new StringBuffer()

      val trueHead =
        if (isDummyNode)
          getNext
        else
          this

      var event = trueHead
      while (event != null) {
        sb append event.toString + delim
        event = event.getNext
      }
      sb.toString
    }

    //========================================
    //  Chain-specific methods
    //========================================
    def unbox: Event = this

    def chainSize: Int = 1

    def chainFirst: Event = this

    def chainLast: Event = this

    def isResponseChain: Boolean = isResponse

    def isCallChain: Boolean = isInvocation

    def isCallResponseChain: Boolean = false

    def isResponseCallChain: Boolean = false

    def isCallInitiator: Boolean = isInvocation

    def isIncompleteHistory: Boolean = true

    def isSingularCausalChain: Boolean = true

    def isActualCausalChain: Boolean = false

    def keys: Seq[Any] = Seq(key)

    def pids: Seq[String] = Seq(pid)

    def operations: Seq[String] = Seq(operation)

    def arguments: Seq[Seq[String]] = Seq(args)

  }

  case class Invocation(k: Option[Any],
                        p: String,
                        op: String,
                        arg: Seq[String]) extends Event {

    key = k
    pid = p
    operation = op
    args = arg


    override def isInvocation: Boolean = true

    override def copy: Event = {
      val e = Invocation(key, pid, operation, args)
      e.setDVV(getDVV)
      e.setID(getID)
      e
    }

    override def toString: String = s"${getClass.getSimpleName}(${getDVV},${getKey},${getPID},${getOperation},${getArgs}) ${if (null != theMatch) "AND match = " + theMatch.toString()}"
  }

  case class Response(k: Option[Any],
                      p: String,
                      op: String,
                      arg: Seq[String]) extends Event {

    key = k
    pid = p
    operation = op
    args = arg

    override def isResponse: Boolean = true

    override def copy: Event = {
      val e = Response(key, pid, operation, args)
      e.setDVV(getDVV)
      e.setID(getID)
      e
    }

    override def equals(that: Any): Boolean = {
      that match {
        case x: Response =>
          getDVV == x.getDVV &&
            getKey == x.getKey &&
            getID == x.getID &&
            getPID == x.getPID &&
            getArgs == x.getArgs

        case _ => false
      }
    }
  }

  case class WildCardResponse(k: Option[Any] = Some("WILD"),
                              p: String = "WILD",
                              op: String = "WEST",
                              arg: Seq[String] = Seq("RESPONSE")) extends Event {

    key = k
    pid = p
    operation = op
    args = arg

    override def isResponse: Boolean = true

    override def copy: Event = {
      val e = WildCardResponse(key, pid, operation, args)
      e.setDVV(getDVV)
      e.setID(getID)
      e
    }

    override def equals(that: Any): Boolean = {
      that match {
        case x: Response =>
          getDVV == x.getDVV &&
            getKey == x.getKey &&
            getID == x.getID &&
            getPID == x.getPID &&
            getArgs == x.getArgs

        case _ => false
      }
    }

  }

  //  object Response {
  //    def unapply(resp: Response): Option[(DVV, Option[Any], String, String, Seq[String])] = {
  //      Some((resp.getDVV,
  //        resp.getKey,
  //        resp.getPID,
  //        resp.getOperation,
  //        resp.getArgs))
  //    }
  //  }

  case class Fail(k: Option[Any],
                  p: String,
                  op: String,
                  arg: Seq[String]) extends Event {

    key = k
    pid = p
    operation = op
    args = arg

    override def isFail: Boolean = true

    override def copy: Event = {
      val e = Fail(key, pid, operation, args)
      e.setDVV(getDVV)
      e.setID(getID)
      e
    }
  }

  case class Info(k: Option[Any],
                  p: String,
                  op: String,
                  arg: Seq[String]) extends Event {

    key = k
    pid = p
    operation = op
    args = arg

    override def isInfo = true

    override def copy: Event = {
      val e = Info(key, pid, operation, args)
      e.setDVV(getDVV)
      e.setID(getID)
      e
    }
  }

  @deprecated
  case class CausalChain(var chain: Event) extends Event {
    require(chain != null && !chain.isEmpty, "CausalChain -- can't create a chain from an empty ")

    key = chain.last.getKey
    pid = chain.last.getPID
    operation = chain.last.getOperation
    args = chain.last.getArgs

    override def copy: Event = {
      val e = CausalChain(chain.copyAll)
      e.setID(getID)
      e
    }

    override def getMatch: Event = {
      if (isInvocation) super.getMatch
      else null
    }


    override def append(entry: Entry): Unit = {
      Event.insertAfter(chainLast, entry)

      setDVV(entry.getDVV)
      setMatch(entry.getMatch)

      key = entry.getKey
      pid = entry.getPID
      operation = entry.getOperation
      args = entry.getArgs
    }

    //====================================
    // Hashcode (equals is inherited)
    //====================================
    override def hashCode: Int = super.hashCode() + chain.hashCode()

    //====================================
    override def simplify: Unit = {
      if (chainFirst.isResponse) {
        val frst = chainFirst
        val rest = chainFirst.getNext
        Event.insertBefore(this, frst.copy)
        Event.remove(frst)
        chain = rest
      }
    }

    override def causalReconstruct: Unit = super.causalReconstruct

    override def unbox: Event = chain

    override def chainSize: Int = chain.size

    override def chainFirst: Event = chain.first

    override def chainLast: Event = chain.last

    override def isResponseChain: Boolean = {
      // re-implementing since the overriden isResponse gets called from within this class not super-class
      //      chainFirst.isResponse && chainLast.isResponse
      val res1 = chainFirst match {
        case x: Invocation => true
        case _ => false
      }
      val res2 = chainLast match {
        case x: Response => true
        case _ => false
      }
      res1 && res2
    }

    override def isCallChain: Boolean = {
      // chainFirst.isInvocation && chainLast.isInvocation
      val inv1 = chainFirst match {
        case x: Invocation => true
        case _ => false
      }

      val inv2 = chainLast match {
        case x: Invocation => true
        case _ => false
      }
      inv1 && inv2
    }

    override def isCallResponseChain: Boolean = {
      // chainFirst.isInvocation && chainLast.isResponse
      val inv = chainFirst match {
        case x: Invocation => true
        case _ => false
      }
      val res = chainLast match {
        case x: Response => true
        case _ => false
      }

      inv && res
    }

    override def isResponseCallChain: Boolean = {
      // chainFirst.isResponse && chainLast.isInvocation
      val res = chainFirst match {
        case x: Response => true
        case _ => false
      }
      val inv = chainLast match {
        case x: Invocation => true
        case _ => false
      }
      res && inv
    }

    override def isCallInitiator: Boolean = isResponseCallChain || isCallChain

    override def isIncompleteHistory: Boolean = isResponseCallChain

    override def isSingularCausalChain: Boolean = chainSize == 1

    override def isActualCausalChain: Boolean = chainSize > 1

    override def isResponse: Boolean = {
      chainFirst match {
        case x: Response => true
        case _ => false
      }
    }

    override def isInvocation: Boolean = {
      chainLast match {
        case x: Invocation => true
        case _ => false
      }
    }

    override def matches(that: Event): Boolean = isInvocation && chainLast.matches(that.chainFirst)

    override def getDVV: DVV = if (isResponse) chainFirst.getDVV else chainLast.getDVV

    override def getKey: Option[Any] = if (isResponse) chainFirst.getKey else chainLast.getKey

    override def getPID: String = if (isResponse) chainFirst.getPID else chainLast.getPID

    override def getOperation: String = if (isResponse) chainFirst.getOperation else chainLast.getOperation

    override def getArgs: Seq[String] = if (isResponse) chainFirst.getArgs else chainLast.getArgs

    override def head: Event = {
      if (getPrev != null) getPrev.head
      else this
    }

    override def last: Event = {
      if (getNext != null) getNext.last
      else this
    }

    override def sameKey(that: Event): Boolean = if (isResponse) chainFirst.getKey == that.getKey else chainLast.getKey == that.getKey

    override def toString: String = {
      var event: Event = chain.first
      val sb = new StringBuilder()
      sb.append(s"CausalChain($dvv,$pid,$key,$operation,$args)[ ${event.toString}, ...")
      //      while (event != null) {
      //        sb.append(event.toString)
      //        event = event.getNext
      //      }
      sb.append(" ]").toString
    }

    override def keys: Seq[Any] = {
      var theKeys = Seq[Option[Any]]()
      var event: Event = chain
      while (event != null) {
        theKeys = theKeys :+ event.getKey
        event = event.getNext
      }
      theKeys
    }

    override def pids: Seq[String] = {
      var pids = Seq[String]()
      var event: Event = this
      while (event != null) {
        pids = pids :+ event.getPID
        event = event.getNext
      }
      pids
    }


    override def toPorcupineEntry: String = chain.toPorcupineLog

    override def operations: Seq[String] = {
      var ops = Seq[String]()
      var event: Event = this
      while (event != null) {
        ops = ops :+ event.getOperation
        event = event.getNext
      }
      ops
    }

    override def arguments: Seq[Seq[String]] = {
      var args = Seq[Seq[String]]()
      var event: Event = this
      while (event != null) {
        args = args :+ event.getArgs
        event = event.getNext
      }
      args
    }
  }

}





