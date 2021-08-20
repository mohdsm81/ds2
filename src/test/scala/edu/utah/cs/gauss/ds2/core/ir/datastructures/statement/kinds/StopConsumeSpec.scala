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


class StopConsumeSpec extends MyTestSpecs {
  info("==================================")
  info("Stop Consume Statement tests")
  info("==================================")
       
  test("StopConsume")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))
    val agent = ds.get("server")
    var s = StopConsume.apply
    s.setAgent(agent)
    s.apply
   
    assert(ds.get("server").consuming == false, "Server did not stop consuming!")
   
  }
  
  
}