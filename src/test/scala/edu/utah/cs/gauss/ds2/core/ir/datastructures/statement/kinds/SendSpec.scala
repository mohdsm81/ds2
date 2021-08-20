package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM 

/**
  * @author <br>
  * 	Anushree Singh <br/>
  * 	anushree@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */

class SendSpec  extends MyTestSpecs {
  info("==================================")
  info("Send Statement tests")
  info("==================================")
  
  test("Send") {
    
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))
    
    val src = ds.get("client")
    val dst = ds.get("server")
    val msg = messageInstance
    
    //Static Test
    val m = messageInstance
    val s = Send(m, dst)

    assert(null != s.dstAgent, "dstAgent is NULL!")
    assert(null != s.msgOut, "msgOut is NULL!")
    s.setAgent(src)
    s.apply

    assert(ds.get("server").q.size == 1, "Server didn't receive the message!")
    
    //Dynamic Test
    
    val msgOutVar = s"msg${DELIM}Message"
    val dstAgentVar = s"server${DELIM}Agent"    
    
    src.localState(dstAgentVar) = dst
    src.localState(msgOutVar) = messageInstance
    
    var r = Send(msgOutVar, dstAgentVar)
    r.setAgent(src)
    r.apply

    s.apply
    assert(ds.get("server").q.size == 3, "Server didn't receive the message!")
  }
  
}