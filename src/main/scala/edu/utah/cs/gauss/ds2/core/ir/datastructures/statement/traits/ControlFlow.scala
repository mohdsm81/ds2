package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */
trait Conditional {

  var condition: (Message, Agent) => Boolean = (m: Message, a: Agent) => true
  var conditionVar: String = "" // in case the above is located at the agent.localstate
  var conditionEvaluation: Boolean = false // marks if this branch was executed
  // var returnValueVar: String = ""

  var body: Seq[Statement] = Seq()

  // def skip: Unit // skips whatever else-if or else in the action.toExectute once the condition evaluated to true. i.e. just removes those statements from there!

}
/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

trait Looping extends Conditional {
  condition = (m: Message, a: Agent) => false // no one likes infinite loops
  var count: Int = 0 // how many times the body was executed (i.e. the condition evaluated to true)
}

/**
 * @author <br>
 * 	Mohammed S. Al-Mahfoudh <br/>
 * 	mahfoudh@cs.utah.edu <br/>
 * 	SoC - Gauss Group <br/>
 */

trait FunctionCalling {
  var frame: ActivationFrame = new ActivationFrame
  var body: Seq[Statement] = Seq()
  var parameterListVar: String = ""

  def pushed: Boolean = frame.pushed
  def popped: Boolean = frame.popped

  def pop: Option[ActivationFrame] = {
    if (!frame.popped && top == frame) {
      frame.popped = true
      Some(frame.pop)
    } else
      None
  }

  def push: Boolean = {
    if (!frame.pushed && !frame.stack.isEmpty) {
      frame.pushed = true
      frame.push
      true
    } else false
  }

  def top: ActivationFrame = frame.stack.top
}

// /**
//  * @author <br>
//  * 	Mohammed S. Al-Mahfoudh <br/>
//  * 	mahfoudh@cs.utah.edu <br/>
//  * 	SoC - Gauss Group <br/>
//  */
// trait FunctionRecursion {
//   var function: Function
// }
