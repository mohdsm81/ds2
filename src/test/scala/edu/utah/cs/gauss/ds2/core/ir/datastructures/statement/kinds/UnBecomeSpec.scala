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

class UnBecomeSpec extends MyTestSpecs {
  info("==================================")
  info("UnBecome Statement tests")
  info("==================================")
       
  test("UnBecome")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))
    val agent = ds.get("server")
    agent.behaviors = agent.behaviors + ("newBehavior"->new Behavior("newBehavior"))
    
    var b = Become("newBehavior",true)
    b.setAgent(agent)
    b.apply
    agent.behaviors = agent.behaviors + (agent.oldBehaviors.top->new Behavior(agent.oldBehaviors.top))
    var old  = agent.defaultBehavior
    if (agent.oldBehaviors.size != 0) {
      old = agent.behaviors(agent.oldBehaviors.top)
    }
    
    var ub = UnBecome.apply
    ub.setAgent(agent)
    ub.apply
  
    assert(ds.get("server").reactions == old, "Reactions set back to old")
  
  }
}