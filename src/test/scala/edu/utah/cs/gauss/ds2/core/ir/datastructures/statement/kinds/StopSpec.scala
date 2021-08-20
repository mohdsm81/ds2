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

class StopSpec extends MyTestSpecs {
  info("==================================")
  info("Stop Statement tests")
  info("==================================")
  
     
  test("Stop")
  {
    
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val src = ds.get("client")
    val dst = ds.get("server")
    
    
    //Static test
    
    
    var r = Stop(dst)
    r.setAgent(src)
    r.apply
   
    assert(ds.get("server").q.size == 1, "Server didn't receive the stop message!")
    
    // not needed, stop implementation was too old, it does thing it shouldn't have! e.g. scheduling?!! it is a simple send
//    assert(ds.scheduler.taskQ.size == 0, "Task was not added to the scheduler's taskQ")
    
    //Dynamic Test
    
    val dstAgentVar = s"server${DELIM}Agent"    
    src.localState(dstAgentVar) = dst
    var s = Stop(dstAgentVar)
    s.setAgent(src)
    s.apply
    
    assert(ds.get("server").q.size == 2, "Server didn't receive the stop message!")
  }
  
}
