package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM 

/**
  * @author <br>
  *         Anushree Singh <br/>
  *         anushree@cs.utah.edu <br/>
  *         SoC - Gauss Group <br/>
  * @author <br>
  *         Mohammed S. Al-Mahfoudh <br/>
  *         mahfoudh@cs.utah.edu <br/>
  *         SoC - Gauss Group <br/>
  */

class ModifyStateSpec extends MyTestSpecs {
  info("==================================")
  info("Modify State Statement tests")
  info("==================================")
       
  test("ModifyState")
  {
    val ds = distributedSystemWithSchedulerInstance
    val src = ds.get("client")
    
    
    //Static Test
    
   
    val variable = "locked"+DELIM+"Boolean"
    src.localState(variable) = false
    var s = ModifyState(variable,true) 
    s.setAgent(src)
    s.apply 
    
   assert(src.localState[Boolean](variable) == true, "Local State was not modified properly!")
 
    // functional test
    src.localState(variable) = false
    s = ModifyState(variable,(m: Message, a: Agent) => { var ans = true; ans})
    s.setAgent(src)
    s.apply

    assert(src.localState[Boolean](variable) == true, "Local State was not modified properly by the function passed!")
    
    //Dynamic Test
    
    val variableVar = "variablevar"+DELIM+"String"
    src.localState(variableVar) = variable
    src.localState(variable) = false
    
    val valueVar = "value"+DELIM+"Boolean"
    src.localState(valueVar) = true
    var s1 = ModifyState(variableVar,valueVar) 
    s1.setAgent(src)
    s1.apply 
    
   assert(src.localState[Boolean](variable) == true, "Local State was not modified properly!")
  }
  
  
}