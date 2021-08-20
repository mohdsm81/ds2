package edu.utah.cs.gauss.ds2.core.schedulers.algorithms.generic

import edu.utah.cs.gauss.ds2.core.schedulers.algorithms.AbstractTypes.Receive

/**
 * @author
 *        	Mohammed S. Al-Mahfoudh <p>
 * 		   	mahfoudh@cs.utah.edu <p>
 * 		   	Gauss Group - SoC <p>
 * 		   	The University of Utah <p>
 *
 */

trait DependecySet {
  // One receive may enable zero or more receives
  var dependencySet: Set[(Receive,Receive)] = Set.empty
}
