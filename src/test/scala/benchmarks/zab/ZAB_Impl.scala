//package benchmarks.zab
///***************************************************************************
// *                                                                         *
// *                           ZAB_Impl.scala                                *
// *                            -------------------                          *
// *   date                 : 9.30.2015                                      *
// *   email                : heath.french@utah.edu                          *
// *                                                                         *
// *                                                                         *
// ***************************************************************************/
//
///***************************************************************************
// *                                                                         *
// *   This program is free software; you can redistribute it and/or modify  *
// *   it under the terms of the GNU General Public License as published by  *
// *   the Free Software Foundation; either version 2 of the License, or     *
// *   (at your option) any later version.                                   *
// *                                                                         *
// *   A copy of the license can be found in the license.txt file supplied   *
// *   with this software or at: http://www.gnu.org/copyleft/gpl.html        *
// *                                                                         *
// ***************************************************************************/
//
//import java.util.concurrent.TimeUnit
//
//import akka.actor._
//import akka.pattern._
//import akka.util.Timeout
//import com.typesafe.config.ConfigFactory
//
//import scala.collection.mutable.ArrayBuffer
//import scala.concurrent._
//import scala.concurrent.duration.FiniteDuration
//
//class ZAB_Impl() extends ZAB{
//
//  private var system : ActorSystem = _
//  private var actor : ActorRef = _
//
//  // creates new node that is in its own ZAB system
//  override def createNode(local : URL) : Unit = {
//    if(this.system == null && this.actor == null){
//      val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + local.getPort + ", akka.remote.netty.tcp.hostname=\"" + local.getHost + "\"").withFallback(ConfigFactory.load("common"))
//
//      this.system = ActorSystem("ChordSystem" + local.getPort, config)
//
//      this.actor = this.system.actorOf(Props(classOf [ZAB_Actor], null), "nodeImpl")
//    }
//  }
//
//  // creates a new node that joins a pre-existing ZAB system
//  override def joinNode(local : URL, bootstrap : URL) : Unit = {
//    if(this.system == null && this.actor == null){
//      val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + local.getPort + ", akka.remote.netty.tcp.hostname=\"" + local.getHost + "\"").withFallback(ConfigFactory.load("common"))
//
//      this.system = ActorSystem("ChordSystem" + local.getPort, config)
//
//      this.actor = this.system.actorOf(Props(classOf [ZAB_Actor], bootstrap), "nodeImpl")
//    }
//  }
//
//  // attempts to submit a message to the entire
//  // ZAB system. Will only be sent once the system
//  // is ready to receive new messages.
//  override def submitState(message : Any) : Unit = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(SubmitState(message)))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case _ =>
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//  }
//
//  //gets the transaction history from the current
//  // node. Will be empty if no transactions have
//  // taken place.
//  override def getHistory : ArrayBuffer[Proposal] = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetHistory()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : ArrayBuffer[Proposal] =>
//            result
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  // returns history that has only been both proposed and committed
//  override def getCommittedHistory : ArrayBuffer[Proposal] = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetCommittedHistory()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : ArrayBuffer[Proposal] => result
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  // returns history that has only been proposed
//  override def getProposedHistory : ArrayBuffer[Proposal] = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetProposedHistory()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : ArrayBuffer[Proposal] => result
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  // returns a set of node currently connected in the system.
//  override def getSystemNodes : Set[String] = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetSystemNodes()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : Set[ActorRef] =>
//            var actualResult = Set[String]()
//            for(current : ActorRef <- result){
//              actualResult += current.path.toString
//            }
//            actualResult
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  override def getFollowerNodes : Set[String] = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetFollowerNodes()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : Set[ActorRef] =>
//            var actualResult = Set[String]()
//            for(current : ActorRef <- result){
//              actualResult += current.path.toString
//            }
//            actualResult
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  override def getLeaderString : String = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetLeaderRef()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : ActorRef =>
//            val actualResult = result.path.toString
//            actualResult
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  // returns the latest transaction submitted by the current node.
//  override def getCurrentState : Proposal = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(GetCurrentState()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case result : Proposal => result
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//    else null
//  }
//
//  // forces the current node to go back to the
//  // re-election phase. May cause entire system
//  // to recover if the current node was the leader.
//  override def forceReElection() : Unit = {
//    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
//
//    if(actor != null){
//      try{ // tries to retrieve result from other actor
//        val future : Future[Any] = actor.ask(MethodCalls(ForceReElection()))
//        val ref = Await.result(future, new FiniteDuration(10, TimeUnit.SECONDS))
//        ref match{
//          case exception : Throwable => throw exception
//          case _ =>
//        }
//      }
//      catch{ // if an error occurs, than throws error
//        case e : Exception => throw new Exception(e.toString)
//      }
//    }
//  }
//
//  // forces the current node to crash entirely.
//  // The user will need to re-enter the system again.
//  override def forceCrash() : Unit = {
//    this.system.shutdown()
////    this.system.terminate()
//    this.system = null
//    this.actor = null
//  }
//}