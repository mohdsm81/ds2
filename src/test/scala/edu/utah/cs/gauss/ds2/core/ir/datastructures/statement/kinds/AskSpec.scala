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

class AskSpec extends MyTestSpecs {
  info("==================================")
  info("Ask Statement tests")
  info("==================================")
  
       
  test("Ask")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val src = ds.get("client")
    val msgOut = messageInstance
    val dst = ds.get("server")
    
    //Static Test
    
    var s1 = Ask(msgOut, dst) // static
   
    s1.setAgent(src)

    s1.apply
    assert(null != s1.future && s1.future.isInstanceOf[DummyFuture], "There isn't future returned!")
    assert(ds.get("server").q.size == 1, "Server didn't receive the message!")
  
    //Dynamic Test
    val srcVar = s"client${DELIM}Agent"
    val msgOutVar = s"message${DELIM}Message"
    val dstVar = s"server${DELIM}Agent"
    val variableName = "myVar"+DELIM+"DummyFuture"
    
    src.localState(srcVar) = src
    src.localState(dstVar) = dst
    src.localState(msgOutVar) = msgOut
    
    var s = Ask(msgOutVar, dstVar, variableName) // dynamic
    s.setAgent(src)

    s.apply
    assert(null != s.future && s.future.isInstanceOf[DummyFuture], "There isn't future returned!")
    assert(s.future == src.localState[DummyFuture](variableName))
    assert(ds.get("server").q.size == 2, "Server didn't receive the message!")
  
  }
   
}