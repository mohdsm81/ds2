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

class TimedGetSpec  extends MyTestSpecs {
  info("==================================")
  info("Timed Get Statement tests")
  info("==================================")
       
  test("TimedGet")
  {
    val ds = distributedSystemWithSchedulerInstance
    
    var src = ds.get("client")
    var dst = ds.get("server")
    ds.unlock(dst)
    
    
    //Static Test
    
    var a = Ask(messageInstance,dst)
    a.setAgent(src)
    a.apply
    var f = a.future
    var s = TimedGet(f,"dstvar",BigInt(10)) 
    s.action = new Action()
    s.action.setAgent(src)
    s.setAgent(src)
    s.apply 
    
    assert(f.waitingFor.blocked == true, "It did not get blocked in the Scheduler.execWithPossibleBlocking")
    assert(f.waitingFor.blockedOn == Some(f),"Not blocked properly!")
    
    //Dynamic Test
    
    a = Ask(messageInstance,dst)
    a.setAgent(src)
    a.apply
    f = a.future
    
    var futureVar = "future"+DELIM+"DummyFuture"
    src.localState(futureVar) = f
    
    var timeoutVar = "timeOut"+DELIM+"BigInt"
    src.localState(timeoutVar) = BigInt(10)
    
    var dstVariableName = "dst"+DELIM+"String"
    
    s = TimedGet(futureVar,dstVariableName,timeoutVar)
    s.setAgent(src)
    s.action = new Action()
    s.action.setAgent(src)
    s.apply 
    
    assert(f.waitingFor.blocked == true, "It did not get blocked in the Scheduler.execWithPossibleBlocking")
    assert(f.waitingFor.blockedOn == Some(f),"Not blocked properly!")
    
    
  }
  
  
}