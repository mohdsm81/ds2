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

class UnStashSpec extends MyTestSpecs {
  info("==================================")
  info("UnStash Statement tests")
  info("==================================")
       
  test("UnStash")
  {
    val ds = distributedSystemWithSchedulerInstance
    val agent = ds.get("server")
    ds.unlock(agent)
   
    agent.stash = agent.stash :+ messageInstance
    
    var s = UnStash.apply
    s.setAgent(agent)
    s.apply
   
    assert(agent.q.map(_.name).contains(messageInstance.name) ,"The queue does not contain the message")
    assert(!agent.stash.map(_.name).contains(messageInstance.name),"Unstash did not work properly! Stash still contains the message.")
  }
}