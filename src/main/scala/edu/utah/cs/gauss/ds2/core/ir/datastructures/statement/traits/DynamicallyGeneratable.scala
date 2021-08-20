package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.Statement

trait DynamicallyGeneratable[T <: Statement] {
  def generateDynamic: T
}
