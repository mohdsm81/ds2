package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM 

/**
  * @author <br>
  * 	Anushree Singh <br/>
  * 	anushree@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */

class BecomeSpec extends MyTestSpecs {
  info("==================================")
  info("Become Statement tests")
  info("==================================")
       
  test("Become")
  {
    val ds = distributedSystemWithSchedulerInstance
    val agent = ds.get("server")
    ds.unlock(agent)
    ds + agent
    
    agent.behaviors = agent.behaviors + ("newBehavior"->new Behavior("newBehavior"))
    var serverOldReactionName = agent.reactions.name
    
    //Static Test
    var s = Become("newBehavior",true)
    s.setAgent(agent)
    s.apply
   
    assert(ds.get("server").oldBehaviors.contains(serverOldReactionName), "Old behavior is not remembered!")
    assert(ds.get("server").reactions == ds.get("server").behaviors("newBehavior"),"Become not working properly")
   
    //Dynamic Test
    
    val dagent = ds.get("client")
    
    dagent.behaviors = dagent.behaviors + ("newBehavior"->new Behavior("newBehavior"))
    
    var clientOldReactionName = dagent.reactions.name
    
    val behaviorNameVar = s"newBehavior${DELIM}BehaviorName"
    val rememberVar = s"true${DELIM}Boolean"
    
    dagent.localState(behaviorNameVar) = "newBehavior"
    dagent.localState(rememberVar) = true
    
    var s1 = Become(behaviorNameVar,rememberVar, true, true)
    s1.setAgent(dagent)
    s1.a.behaviors = s1.a.behaviors + ("newBehavior" -> new Behavior("newBehavior"))
    s1.apply
    
    assert(ds.get("client").oldBehaviors.contains(clientOldReactionName), "Old behavior is not remembered!")
    assert(ds.get("client").reactions == ds.get("client").behaviors("newBehavior"),"Become not working properly")
 
  }
}