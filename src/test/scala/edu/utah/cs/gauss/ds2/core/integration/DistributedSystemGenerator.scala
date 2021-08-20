package edu.utah.cs.gauss.ds2.core.integration

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

object DistributedSystemGenerator {
  
  def generateNumberManipulatorSystem() : DistributedSystem = {
    val result:DistributedSystem = new DistributedSystem("math_manipulator")
    result.attach(new BasicScheduler)
    
    val townCrier = new Agent("Town_Crier")
    val numberCruncher = new Agent("Number_Cruncher")
    
    // town crier setup
    val crierCode1 = (m: Message, a: Agent) => {
      println("--------------------------OUTPUT---------------------------")
      println(m.sender.name + " returned value: " + m.payload.mkString)
      println("--------------------------OUTPUT---------------------------")
    }
    val crierCode2 = (m: Message, a: Agent) => {
      m.payload[Integer](0) match {
        case x:Integer => {
          if(x == 42){
            result.send(a, new Message("modify_behavior"), m.sender, false)
          }
          println("recieved number: " + x)
        }
        case _ => {
          throw new Exception("unexpected value")
        }
      }
    }
    
    val crierCode3 = (m: Message, a: Agent) => {
      m.payload[Integer](0) match {
        case x:Integer => {
          if(x == 42){
            result.send(a, new Message("modify_number", Seq(42)), m.sender, false)
            println("modifying behavior")
          }
        }
        case _ => {
          throw new Exception("unexpected value")
        }
      }
    }
    
    val crierRegularAction: Action = new Action()
    crierRegularAction + Statement(crierCode1) + Statement(crierCode2) + Statement(crierCode3)
    
    val crierRegularReaction = (new Message(), crierRegularAction)
    
    val crierReactions = new Behavior("default")
    crierReactions += crierRegularReaction
    
    val crierStart = (m: Message, a: Agent) => {
      result.unlock(a);
      result.send(a, new Message("modify_number", Seq(42)), numberCruncher, false)
    }
    
    val crierStarterAction: Action = new Action()
    crierStarterAction + Statement(crierStart)
    
    val crierStartReaction = (new Start, crierStarterAction)
    
    townCrier.specialReactions += crierStartReaction
    townCrier.defaultBehavior = crierReactions
    townCrier.reactions = crierReactions
    townCrier.refresh
    
    // number cruncher creation
    val echoNumber = (m:Message, a:Agent) => {
      m.payload[Integer](0) match {
        case x : Integer => {
          result.send(a, new Message(Seq(x)), m.sender, false)
        }
        case _ => {
          throw new Exception("unexpected value")
        }
      }
    }
    
    val multiplyNumber = (m:Message, a:Agent) => {
      m.payload[Integer](0) match {
        case x : Integer => {
          result.send(a, new Message(Seq(x*2)), m.sender, false)
        }
        case _ => {
          throw new Exception("unexpected value")
        }
      }
    }
    
    val modifyBehavior = (m:Message, a:Agent) => {
      result.become(a, "multiply", false)
    }
    
    val defaultAction : Action = new Action
    defaultAction + Statement(echoNumber)
    
    val changeBehaviorAction : Action = new Action
    changeBehaviorAction + Statement(modifyBehavior)
    
    val modifiedAction : Action = new Action
    modifiedAction + Statement(multiplyNumber)
    
    val defaultReaction = (new Message("modify_number"), defaultAction)
    val changeBehaviorReaction = (new Message("modify_behavior"), changeBehaviorAction)
    val multiplyReaction = (new Message("modify_number"), modifiedAction)
    
    val defaultBehavior : Behavior = new Behavior("default")
    defaultBehavior += defaultReaction
    defaultBehavior += changeBehaviorReaction
    
    val differentBehavior : Behavior = new Behavior("multiply")
    differentBehavior += multiplyReaction
    
    numberCruncher.defaultBehavior = defaultBehavior
    numberCruncher.reactions = defaultBehavior
    numberCruncher.behaviors += (("multiply", differentBehavior), ("default", defaultBehavior))
    
    numberCruncher.refresh
    
    result + numberCruncher + townCrier
    result.refresh
    
    return result
  }

}