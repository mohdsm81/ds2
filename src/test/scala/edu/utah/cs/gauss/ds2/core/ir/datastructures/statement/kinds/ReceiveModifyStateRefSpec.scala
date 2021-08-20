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

class ReceiveModifyStateRefSpec extends MyTestSpecs {
  info("==================================")
  info("Receive Modify State Ref Statement tests")
  info("==================================")
       
  test("ReceiveModifyStateRef")
  {
    val ds = distributedSystemWithSchedulerInstance
    val src = ds.get("client")
        
    val variablePointedTo = "locked"+DELIM+"Function2[Message,Agent,Boolean]"       
    val variablePointing = "variablevar"+DELIM+"Function2[Message,Agent,Boolean]" 

    src.localState(variablePointedTo) = (m:Message,a:Agent) => true 
    src.localState(variablePointing) = (m:Message,a:Agent) => false 


    var s1 = ReceiveModifyStateRef(variablePointing,variablePointedTo,false,false) 
    s1.setAgent(src)
    s1.apply 

    val func: Any = src.localState(variablePointing).asInstanceOf[Function2[Message,Agent,Any]]
    val cond = func.asInstanceOf[Function2[Message,Agent,Boolean]].apply(s1.m,s1.a)
    assert(cond, "The reference was NOT updated correctly OR the value referenced is NOT the correct value of the referenced variable.")
  }
  
  
}
