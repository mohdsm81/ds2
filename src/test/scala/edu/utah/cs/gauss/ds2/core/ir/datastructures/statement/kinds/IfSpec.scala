package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.BasicScheduler

/**
 * @author <br>
 *         Anushree Singh <br/>
 *         anushree@cs.utah.edu <br/>
 *         SoC - Gauss Group <br/>
 * @author
 *         Mohammed S. Al-Mahfoudh <p>
 *         mahfoudh@cs.utah.edu <p>
 *         Gauss Group - SoC <p>
 *         The University of Utah <p>
 */

class IfSpec extends MyTestSpecs {

  info("==================================")
  info("If Statement tests")
  info("==================================")

  test("If") {
    val ds = distributedSystemWithSchedulerInstance
    var ag = Set(("newAgent", "args"))
    var agent = new Agent("newAgent")
    ds + agent
    val someCondition = (m: Message, a: Agent) => {
      true
    }


    var stmts = Seq.empty[Statement];
    //    stmts = stmts :+ Statement((m:Message,a:Agent) => println("whatever"));
    stmts = stmts :+ BootStrap(ag);

    val action = actionInstance
    action.setAgent(agent)
    val s = If(someCondition)(stmts: _*)
    s.setAction(action)
    s.setAgent(agent)
    s.setMessage(messageInstance)
    s.apply

    action.execute

    agent.q.size should equal(1)
  }

  test("If-else") {
    val agent = new Agent("newAgent")
    val ds = new DistributedSystem("TestConditionals")
    ds + agent

    agent.locked = false

    var variable = 0

    val ifStmt = If((_, _) => true)(Statement { (_, _) => variable += 1 })
    val elseStmt = Else(Statement { (_, _) => variable += 2 })(ifStmt)

    val action = new Action + ifStmt + elseStmt
    action.setAction;
    action.setMessage(messageInstance);
    action.setAgent(agent)

    agent.defaultBehavior += messageInstance -> action
    ds.refresh

    val sch = new BasicScheduler
    sch.attach(ds)
    ds.send(agent, messageInstance, agent)

    sch.schedule(agent)
    sch.consumeAll(agent)
    sch.executeAll()

    variable should equal(1)

  }

  test("If-elseif-else chain") {

    val agent = new Agent("newAgent")
    val ds = new DistributedSystem("TestConditionals")
    ds + agent

    agent.locked = false

    var variable = 0

    val ifStmt = If((_, _) => false)(Statement { (_, _) => variable += 1 })
    val elseIfStmt = ElseIf((_, _) => true)(Statement { (_, _) => variable += 2 })(ifStmt)
    val elseStmt = Else(Statement { (_, _) => variable += 3 })(ifStmt)
    val prnt = Statement{(m:Message, a:Agent) => println("Great!")}

    val action = new Action + ifStmt + elseIfStmt + elseStmt + prnt
    action.setAction;
    action.setMessage(messageInstance);
    action.setAgent(agent)

    agent.defaultBehavior += messageInstance -> action
    ds.refresh

    val sch = new BasicScheduler
    sch.attach(ds)
    ds.send(agent, messageInstance, agent)

    sch.schedule(agent)
    sch.consumeAll(agent)
    sch.executeAll()

    variable should equal(2)
  }


}
