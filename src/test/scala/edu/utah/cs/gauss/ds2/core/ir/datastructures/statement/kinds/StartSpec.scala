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


class StartSpec extends MyTestSpecs{
   info("==================================")
  info("Start Statement tests")
  info("==================================")
  
       
  test("Start")
  {
    val ds = distributedSystemWithSchedulerInstance
    ds.unlock(ds.get("server"))

    val src = ds.get("client")
    val dst = ds.get("server")
    val args = Seq[String]()
    
    //Static test
    var r = Start(dst, args)
    r.setAgent(src)
    r.apply
    assert(ds.get("server").q.size == 1, "Server didn't receive the start message!")
    
    //Dynamic Test
    val argsVar = s"args${DELIM}String"
    val dstAgentVar = s"server${DELIM}Agent"    
    
    src.localState(dstAgentVar) = dst
    src.localState(argsVar) = args
    
    var s = Start(dstAgentVar, argsVar)
    s.setAgent(src)

    s.apply
    assert(ds.get("server").q.size == 2, "Server didn't receive the start message!")
  
  }
  
}