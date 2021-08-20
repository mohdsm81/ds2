package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM
import edu.utah.cs.gauss.ds2.core.ir.datastructures._

/**
  * @author <br>
  * 	Anushree Singh <br/>
  * 	anushree@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */

class KillSpec extends MyTestSpecs {
  info("==================================")
  info("Kill Statement tests")
  info("==================================")
       
  test("Kill")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val src = ds.get("client")
    val dst = ds.get("server")
    
    //Static test
    
    var r = Kill(dst)
    r.setAgent(src)
    r.apply
   
    assert(ds.get("server").q.size == 1, "Server didn't receive any message!")
    assert(ds.get("server").q.last.name.equals(PoisonPill().name),"Server did not recieve the kill message")
    
    //Dynamic Test
    
    val dstAgentVar = s"server${DELIM}Agent"    
    src.localState(dstAgentVar) = dst
    var s = Kill(dstAgentVar)
    s.setAgent(src)
    s.apply
    assert(ds.get("server").q.size == 2, "Server didn't receive the kill message!")
    assert(ds.get("server").q.last.name.equals(PoisonPill().name),"Server did not recieve the kill message")
    
  }
  
}