package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits

import net.liftweb.json.JValue


trait JsonDeSerializable[T] {
  def fromJson(js: JValue): T
}
