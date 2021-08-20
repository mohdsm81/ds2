package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement
/**
  * @author <br>
  * 	Mohammed S. Al-Mahfoudh <br/>
  * 	mahfoudh@cs.utah.edu <br/>
  * 	SoC - Gauss Group <br/>
  */
trait Returning {

  // destination variable in the callerLocalState
  var returnVariable: String = ""
  // sometimes we can say "return 1", se we use a direct value (not expression whose value should be stored)
  var returnValue: Any = _
  // the src variable that stores the value at the current scope (e.g. function call)
  var returnValueVariable: String = ""

}
