//package benchmarks.zab
//
///***************************************************************************
// *                                                                         *
// *                           ZAB_Application.scala                         *
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
//import scala.collection.mutable.ArrayBuffer
//import scala.io.Source
//
//object ZAB_Application extends App{
//  override def main(args: Array[String]){
//    val zab: ZAB = new ZAB_Impl()
//    println(
//"""Welcome to the Akka-ZAB application!
//To use this, simply type in any of the possible commands
//with any possible arguments then press enter.
//For a list of commands, type "help".
//"""")
//    for (ln : String <- Source.stdin.getLines) {
//      try{
//        val splitString : Array[String] = ln.split(" ")
//        splitString(0) match{
//          case "create" =>
//            if(zab != null){
//              val url : URL = new URL(splitString(1))
//              zab.createNode(url)
//            }
//          case "join" =>
//            if(zab != null){
//              val url : URL = new URL(splitString(1))
//              val bootstrap : URL = new URL(splitString(2))
//              zab.joinNode(url, bootstrap)
//            }
//          case "submit" =>
//            zab.submitState(splitString(1))
//          case "current_submission" =>
//            val submission : Proposal = zab.getCurrentState
//            println("\t " + submission.toString())
//          case "history" =>
//            val history : ArrayBuffer[Proposal] = zab.getHistory
//            println("{")
//            for(currentProposal : Proposal <- history){
//              println(currentProposal.toString())
//            }
//            println("}")
//          case "committed_history" =>
//            val history : ArrayBuffer[Proposal] = zab.getCommittedHistory
//            println("{")
//            for(currentProposal : Proposal <- history){
//              println(currentProposal.toString())
//            }
//            println("}")
//          case "proposed_history" =>
//            val history : ArrayBuffer[Proposal] = zab.getProposedHistory
//            println("{")
//            for(currentProposal : Proposal <- history){
//              println(currentProposal.toString())
//            }
//            println("}")
//          case "system" =>
//            val list : Set[String] = zab.getSystemNodes
//            print("{")
//            for(item : String <- list){
//              println("\t" + item)
//            }
//            print("}")
//          case "followers" =>
//            val list : Set[String] = zab.getFollowerNodes
//            print("{")
//            for(item : String <- list){
//              println("\t" + item)
//            }
//            print("}")
//          case "leader" =>
//            val leader = zab.getLeaderString
//            println("\t" + leader)
//          case "re-election" =>
//            zab.forceReElection()
//          case "crash" =>
//            zab.forceCrash()
//          case "help" =>
//            println("List of Commands:")
//            println("create, join, submit, current_submission")
//            println("history, committed_history, proposed_history")
//            println("system, followers, leader, re-election")
//            println("crash, help")
//        }
//      }
//      catch {
//        case e : Any => println("Error: " + e.getMessage + "\nTry again.")
//      }
//    }
//  }
//}
