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

class BootStrapSpec extends MyTestSpecs {
  info("==================================")
  info("BootStrap Statement tests")
  info("==================================")
       
  test("BootStrap")
  {
    val ds = distributedSystemWithSchedulerInstance
    
    var a = Set(("newAgent", "args")) 
    ds + new Agent("newAgent") 
    
    //Static Test
    var s = BootStrap(a)
    s.setAgent(ds.bootStraper)
    s.apply 
    
    assert(ds.get("newAgent").q.size == 1, "The new agent did not receive the start message")
    assert(ds.get("newAgent").q.last.sender.name == "boot","The new agent was not bootstrapped")
   
    //Dynamic Test
    
    val dagent = ds.get("client")
    val bootStrappedVar = s"bootstrap${DELIM}Set((String,String))"
    dagent.localState(bootStrappedVar) = Set(("DnewAgent", "args")) 
    
    ds + new Agent("DnewAgent") 
    var s1 = BootStrap(bootStrappedVar)
    s1.setAgent(dagent)
    s1.apply
    
    assert(ds.get("DnewAgent").q.size == 1, "The new agent did not receive the start message")
    assert(ds.get("DnewAgent").q.last.sender.name == "boot","The new agent was not bootstrapped")
 
  }
  
}