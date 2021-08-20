package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._

/**
  * @author <br>
  * 	Anushree Singh <br/>
  * 	anushree@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
 */

class WhileSpec extends MyTestSpecs{
  
  info("==================================")
  info("While Statement tests")
  info("==================================")
       
  test("While")
  {
    val ds = distributedSystemWithSchedulerInstance
    val action = actionInstance
    ds + action.a
    ds.unlock(action.a)
    val ag = Set((action.a.name, "args"))
    var count = 3
    val someCondition = (_:Message, _:Agent) => { count > 0}

    val m = new Message("something")

    var stmts = Seq.empty[Statement]
    stmts = stmts :+ Statement((_, _) => println(s"count = $count"))
    stmts = stmts :+ Send(m,action.a)
    stmts = stmts :+ Statement((_, _) => {count -=1})

    val s = While(someCondition)(stmts:_*)
    action + s
    s.setAction(action)
    s.setAgent(action.a)
    s.setMessage(messageInstance)

    action.execute
    action.a.q.size should equal(3)
  }
  
}
