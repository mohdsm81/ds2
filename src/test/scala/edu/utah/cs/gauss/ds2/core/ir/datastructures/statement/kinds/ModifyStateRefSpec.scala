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

class ModifyStateRefSpec extends MyTestSpecs {
  info("==================================")
  info("Modify State Ref Statement tests")
  info("==================================")
       
  test("ModifyStateRef")
  {
    val ds = distributedSystemWithSchedulerInstance
    val src = ds.get("client")
        
    val variablePointedTo = "locked"+DELIM+"Boolean"       
    // val variablePointing = "variablevar"+DELIM+"String" // how come?!!! in type safe languages like Scala, can a String variable point to a Boolean variable?
    // // it doesn't make sense then to have a type safe language. Note that both variable types, after the DELIM must agree.
    val variablePointing = "variablevar"+DELIM+"Boolean" // how come?!!! in type safe languages like Scala, can a String variable point to a Boolean variable?
    // it doesn't make sense then to have a type safe language. Note that both variable types, after the DELIM must agree.



    // src.localState(variablePointing) = variablePointedTo // what is this? I don't understand why is this one here
    // src.localState(variable) = false // either change this to true or assertion to equal false.

    src.localState(variablePointedTo) = true // makes sense, so that the pointed-to value has a meaningful predetermined value
    src.localState(variablePointing) = false // to double check the value changes after executing the statement cuz it points to variablepointedto

    // val valueVar = "value"+DELIM+"Boolean" // I don't know why is this either
    // src.localState(valueVar) = true // WRONG, you can't set a value-var like this for referencing
    // src.localState.setRef(variablePointing, variablePointedTo) // this or the remaining code, because the remaining code is ought to do this exact thing


    var s1 = ModifyStateRef(variablePointing,variablePointedTo,false,false) 
    s1.setAgent(src)
    s1.apply 

    // give a meaningful assertion failure message
    // assert(src.localState(variable) == true, "It did not get blocked in the Scheduler.execWithPossibleBlocking")

    // what is this testing for? it should test for whether the "variableVar" value is the same as "variable" value (because they point to the same thing)
    assert(src.localState[Boolean](variablePointing) == true, "The reference was NOT updated correctly OR the value referenced is NOT the correct value of the referenced variable.")
  }
  
  
}
