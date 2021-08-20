package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM 

/**
  * @author <br>
  * 	Anushree Singh <br/>
  * 	anushree@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */


class LockUnLockSpec extends MyTestSpecs {
  info("==================================")
  info("Lock/UnLock Statements tests")
  info("==================================")
       
  test("Lock")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))    
    val agent = ds.get("server")
    var s = Lock.apply
    s.setAgent(agent)
    s.apply
   
    assert(ds.get("server").locked == true, "Server did not lock properly!")
   
  }

  test("UnLock")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.lock(ds.get("server"))
    val agent = ds.get("server")
    var s = UnLock.apply
    s.setAgent(agent)
    s.apply

    assert(ds.get("server").locked == false, "Server did not UnLock properly!")

  }
  
  
}