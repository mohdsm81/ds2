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

class CreateSpec extends MyTestSpecs{
  info("==================================")
  info("Create Statement tests")
  info("==================================")
  
       
  test("Create")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val newAgent = "NewAgent"
    
    val src = ds.get("client")
    val childAgentOrVar = s"newAgent${DELIM}Agent"    
    src.localState(childAgentOrVar) = newAgent
        
    var s = Create(childAgentOrVar)
    s.setAgent(src)
    s.apply
    
    assert(null != ds.get("NewAgent") , "New agent not created!") 
  
  }
  
}