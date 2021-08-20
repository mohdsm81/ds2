//package benchmarks.paxos
//
//import java.io.Serializable
//import java.net.InetSocketAddress
//import java.util.concurrent.TimeUnit
//import java.util.logging.Logger
//
//import akka.actor.{Actor, ActorRef}
//import akka.io.{IO, Udp}
//
//import scala.concurrent.duration.Duration
//
//case class ReceivePrepare(ins: BigInt, pid: ProposalID)
//
//case class ReceivePromise(ins: BigInt, nid: Int, vid: (Serializable, ProposalID), promisedPid: ProposalID)
//
//case class ReceiveNack(ins: BigInt, highestPid: ProposalID)
//
//case class Propose(value: Serializable, nid: Int)
//
//case class Accept(ins: BigInt, accValue: Serializable, accPid: ProposalID)
//
//case class Accepted(ins: BigInt, uid: Int, value: Serializable, pid: ProposalID)
//
//case class ProposeCMD(v: Serializable)
//
//case class ReceiveHeartbeat(ins: BigInt, nid: Int)
//
//case class LeaderLiveness()
//
//case class InstanceRequest(ins: BigInt)
//
//case class InstanceResponse(ins: BigInt, accValue: Serializable, accPid: ProposalID)
//
//case class PrintLogs()
//
//
///**
// * A paxos actor can act as three types of agents: proposer, acceptor, learner.
// * As proposer, users can prompt the paxos to propose a value from the console
// * As acceptor, the paxos can also receive prepare message, and send out promise or nack message as response
// * As learner, the paxos is always listening from other paxos accepted value.
// *
// * @author Zepeng Zhao
// *
// */
//class PaxosActor(val pm: Map[Int, InetSocketAddress], val id: Int) extends Actor {
//  private val logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME)
//
//  import context.system
//
//  private var proposalValues: List[Serializable] = List()
//  private var proposingValue: Serializable = null
//  private var proposingId: ProposalID = null
//  private var promises: Map[String, List[(Serializable, ProposalID)]] = Map() //((instance_number,proposal_id)=>List[(value,proposal_id)])
//  private var learnedProposals: Map[String, (Serializable, ProposalID, Int)] = Map()
//  private val quorumSize = pm.size / 2 + 1
//  private var promiseIds: Map[BigInt, ProposalID] = Map()
//  private var nextInstance = BigInt(0)
//  private var logs: Map[BigInt, (Serializable, ProposalID)] = Map() //(instance_number => (value, pid))
//  private var leader: Int = this.id
//  private var leadingInstance: BigInt = BigInt(0)
//  private var leaderLive = true
//  private var socket: ActorRef = null
//  private var lock1 = new Object()
//  private val filename = "/tmp/Node_" + this.id + ".txt"
//  try {
//    logger.info("Trying to read logs from disk.")
//    this.logs = Util.readFromDisk(filename)
//    //obtain the most recent instance number
//    this.logs.keys.foreach { x => if (this.nextInstance <= x) this.nextInstance = x + 1 }
//  }
//  catch {
//    case e: Exception => {
//      println("\nRead file failed:" + e.getMessage + "\n->")
//    }
//  }
//
//  this.leadingInstance = this.nextInstance
//
//  logger.info("Next instance:" + this.nextInstance)
//  implicit val executor = context.system.dispatcher
//  context.system.scheduler.schedule(Duration(1000, TimeUnit.MILLISECONDS),
//    Duration(500, TimeUnit.MILLISECONDS),
//    new Runnable {
//      def run() {
//        send_heartbeat()
//      }
//    })
//
//  context.system.scheduler.schedule(Duration(1, TimeUnit.SECONDS),
//    Duration(new scala.util.Random(System.currentTimeMillis).nextInt(1500) + 1500, TimeUnit.MILLISECONDS),
//    new Runnable {
//      def run() {
//        check_and_update_proposal_array()
//      }
//    })
//
//  context.system.scheduler.schedule(Duration(4000, TimeUnit.MILLISECONDS),
//    Duration(1500, TimeUnit.MILLISECONDS),
//    new Runnable {
//      def run() {
//        send_leader_liveness()
//      }
//    })
//
//  context.system.scheduler.schedule(Duration(5000, TimeUnit.MILLISECONDS),
//    Duration(3000, TimeUnit.MILLISECONDS),
//    new Runnable {
//      def run() {
//        check_leader_liveness
//      }
//    })
//
//  context.system.scheduler.schedule(Duration(3000, TimeUnit.MILLISECONDS),
//    Duration(500, TimeUnit.MILLISECONDS),
//    new Runnable {
//      def run() {
//        check_and_update_instance()
//      }
//    })
//
//  IO(Udp) ! Udp.Bind(self, pm(this.id))
//
//  def receive = {
//    case Udp.Bound(local) => {
//      socket = sender
//      context.become(ready())
//    }
//  }
//
//  def ready(): Receive = {
//    case Udp.Received(data, remote) => {
//
//      Util.toObject(data) match {
//
//        case ReceivePrepare(ins, promisedPid) => {
//          logger.info("receive prepare{instance_number:" + ins.toString() +
//            ", proposal_id:" + promisedPid.toString() + "} from remote:[" + remote.toString() + "]")
//          logger.info("Receive prepare from:" + remote.toString())
//          var pid: ProposalID = if (this.promiseIds.contains(ins)) this.promiseIds(ins) else null
//          if (pid == null || !pid.isGreater(promisedPid)) {
//            this.promiseIds += (ins -> promisedPid)
//            logger.info("Send back promise to remote:[" + remote.toString() + "]")
//            var acc_v_id: (Serializable, ProposalID) = if (this.logs.contains(ins)) this.logs(ins) else null
//            socket ! Udp.Send(Util.toByteString(ReceivePromise(ins, this.id, acc_v_id, promisedPid)), remote)
//          } else {
//            logger.info("Send back promise to remote:[" + remote.toString() + "]")
//            socket ! Udp.Send(Util.toByteString(ReceiveNack(ins, pid)), remote)
//          }
//        }
//
//        case ReceivePromise(ins, nid, accValueId, promised_pid) => {
//          if (this.nextInstance == ins) {
//            var key = promised_pid.toString()
//            var l = List(accValueId)
//            if (this.promises.contains(key))
//              l = l ++ this.promises(key)
//            this.promises += (key -> l)
//            if (l.size >= this.quorumSize) {
//              var mid: ProposalID = null
//              l.foreach(f => {
//                if (f != null && (mid == null || f._2.isGreater(mid))) {
//                  if (mid == null)
//                    this.proposalValues = this.proposingValue :: this.proposalValues
//                  mid = f._2
//                  this.proposingValue = f._1
//                }
//              })
//              this.promises = Map()
//              for ((k, v) <- pm) {
//                socket ! Udp.Send(Util.toByteString(Accept(this.nextInstance, this.proposingValue, this.proposingId)), v)
//              }
//            }
//          }
//        }
//
//        case ReceiveNack(ins, higherPid) => {
//
//          logger.info("Receive nack{instance:" + ins + ",higher_pid:" + higherPid.toString() + "} from:" + remote.toString())
//          if (this.leader == this.id) {
//            if (ins == this.nextInstance && this.proposingId != null && higherPid.isGreater(this.proposingId)) {
//              this.proposingId = new ProposalID(higherPid.getNumber + 1, this.id)
//              for ((k, v) <- pm) {
//                socket ! Udp.Send(Util.toByteString(ReceivePrepare(this.nextInstance, this.proposingId)), v)
//                logger.info("Send prepare{instance:[" + this.nextInstance + "], proposal_id:" + this.proposingId.toString() + "} to node:" + k)
//              }
//            }
//          } else {
//            self ! Propose(this.proposingValue, this.leader)
//          }
//
//        }
//
//        case Accept(ins, accValue, accPid) => {
//
//          var pid: ProposalID = if (this.promiseIds.contains(ins)) this.promiseIds(ins) else null
//          logger.info("Receive accept from:" + remote.toString() + " value:[" + accValue + "],proposal id:[" + accPid.toString() + "]")
//          if (pid == null || !pid.isGreater(accPid)) {
//            this.promiseIds += (ins -> accPid)
//            this.logs += (ins -> (accValue, accPid))
//            socket ! Udp.Send(Util.toByteString(Accepted(ins, this.id, accValue, accPid)), remote)
//            Util.writeToDisk(filename, logs)
//          } else {
//            logger.info("send back nack to remote:[" + remote.toString() + "]")
//            socket ! Udp.Send(Util.toByteString(ReceiveNack(ins, pid)), remote)
//          }
//
//        }
//
//        case Accepted(ins, nid, accValue, accPid) => {
//
//          logger.info("learning a value:[" + accValue + "] from node " + nid)
//          var key = accPid.toString() + "_" + ins
//          //println("accept key:"+key)
//          if (ins >= this.nextInstance) {
//            if (!this.learnedProposals.contains(key)) {
//              this.learnedProposals += (key -> (accValue, accPid, 1))
//            } else {
//              var temp1 = this.learnedProposals(key)
//              var temp2 = temp1._3 + 1
//              this.learnedProposals += (key -> (accValue, accPid, temp2))
//              if (temp2 >= this.quorumSize) {
//                this.proposingValue = null
//                this.proposingId = null
//                this.nextInstance += 1
//                this.learnedProposals = Map()
//                print("\nLearned value:" + accValue + ",instance:" + this.nextInstance + "\n->")
//              }
//            }
//          }
//
//        }
//
//
//        case ProposeCMD(va) => {
//          logger.info("receive propose command:propose{value:" + va + "}")
//          if (this.proposingValue == null) {
//            this.proposingValue = va
//            this.proposingId = new ProposalID(0, this.id)
//            this.lock1.synchronized {
//              for ((k, v) <- pm) {
//                socket ! Udp.Send(Util.toByteString(ReceivePrepare(this.nextInstance, this.proposingId)), v)
//                logger.info("Send prepare{instance:[" + this.nextInstance + "], proposal_id:" + this.proposingId.toString() + "} to node:" + k)
//              }
//            }
//          } else {
//            this.proposalValues = this.proposalValues :+ va
//          }
//        }
//
//        case ReceiveHeartbeat(ins, nid) => {
//          if (ins > this.leadingInstance || (ins == this.leadingInstance && this.leader < nid)) {
//            this.leader = nid
//            this.leadingInstance = ins
//            this.leaderLive = true
//            logger.info("Leader id:" + nid + ", instance:" + ins)
//          }
//        }
//
//        case LeaderLiveness() => {
//          this.leaderLive = true
//        }
//
//        case InstanceRequest(ins) => {
//          var temp = if (this.logs.contains(ins)) this.logs(ins) else null
//          if (temp != null)
//            socket ! Udp.Send(Util.toByteString(InstanceResponse(ins, temp._1, temp._2)), remote)
//        }
//
//        case InstanceResponse(ins, accValue, accPid) => {
//          if (this.proposingValue == null && ins == this.nextInstance) {
//            this.logs += (ins -> (accValue, accPid))
//            this.nextInstance += 1
//          }
//
//        }
//
//      }
//    }
//
//
//    case Propose(v: Serializable, nid: Int) => {
//      if (nid <= pm.size) {
//        socket ! Udp.Send(Util.toByteString(ProposeCMD(v)), pm(this.leader))
//        logger.info("ask node:[" + nid + "] to popose {value:" + v + "}")
//      }
//    }
//
//    case PrintLogs() => {
//      println()
//      for (i <- 1 until this.leadingInstance.toInt) {
//        if (logs.contains(i))
//          println("instance:" + i + ", value:" + this.logs(i))
//      }
//      println("->")
//    }
//
//    case Udp.Unbind => socket ! Udp.Unbind
//    case Udp.Unbound => context.stop(self)
//
//  }
//
//  def send_heartbeat() {
//    if (socket != null) {
//      for ((k, v) <- pm) {
//        socket ! Udp.Send(Util.toByteString(ReceiveHeartbeat(this.nextInstance, this.id)), v)
//      }
//    }
//  }
//
//  def send_leader_liveness() {
//    if (this.leadingInstance == this.nextInstance && this.id == this.leader) {
//      for ((k, v) <- pm) {
//        socket ! Udp.Send(Util.toByteString(LeaderLiveness()), v)
//      }
//    }
//  }
//
//  def check_leader_liveness() {
//    if (!this.leaderLive) {
//      logger.info("leader:" + this.leader + " was not alive, updating leadership")
//      this.leadingInstance = this.nextInstance
//      this.leader = this.id
//    }
//    this.leaderLive = false
//  }
//
//  def check_and_update_instance() {
//
//    if (this.nextInstance < this.leadingInstance) {
//      socket ! Udp.Send(Util.toByteString(InstanceRequest(this.nextInstance)), pm(this.leader))
//    }
//
//  }
//
//  def check_and_update_proposal_array() {
//
//    if (this.proposingValue == null && this.proposalValues.size != 0) {
//
//      var v = this.proposalValues(0)
//      println("sending " + v)
//      //socket ! Udp.Send(Util.toByteString(propose_cmd(v)),pm(this.id))
//      this.self ! Propose(v, this.leader)
//      this.proposalValues = this.proposalValues.drop(1)
//    }
//
//  }
//
//}