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

class UnStashAllSpec extends MyTestSpecs {
  info("==================================")
  info("UnStash All Statement tests")
  info("==================================")
       
  test("UnStashAll")
  {
    val ds = distributedSystemWithSchedulerInstance
    val agent = ds.get("server")
    ds.unlock(agent)
   
    agent.stash = agent.stash :+ messageInstance
    
    val msg = new Message("newMessage", agent, true)
    agent.stash = agent.stash :+ msg
    
    var s = UnStashAll.apply
    s.setAgent(agent)
    s.apply
   
    assert(agent.q.map{_.name}.contains(messageInstance.name), "the queue does not contain the first message")
    assert( agent.q.map{_.name}.contains(msg.name), "the queue does not contain the second message")
    assert( agent.stash.isEmpty,"Stash is still not empty!")    
  }
}