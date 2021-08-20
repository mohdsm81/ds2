package edu.utah.cs.gauss.ds2.core.schedulers.algorithms

import edu.utah.cs.gauss.ds2.core.schedulers.Scheduler
import edu.utah.cs.gauss.ds2.core.tracing.Trace
/**
 * @author
 *        Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 */
class BasicScheduler extends Scheduler {
  
  type Name = BasicScheduler

  import scala.collection.mutable.Seq
  override def explore(): Unit = {
    // this is just a manual scheduler so won't implement
    ???
  }
}
