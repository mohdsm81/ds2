// package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.expressions

// import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits._
// import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
// import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._
// import edu.utah.cs.gauss.ds2.core.ir.datastructures._
// import edu.utah.cs.gauss.ds2.core.ir.features._

// trait BooleanExpr extends JsonSerializable with TraceCopying[BooleanExpr] with Snapshot[BooleanExpr] with Printable
// case class Not(expr: BooleanExpr) extends BooleanExpr
// case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Andd(leftVar: String, rightVar:String) extends BooleanExpr
// case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Ord(leftVar: String, rightVar:String) extends BooleanExpr
// case class Lt(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Ltd(leftVar: String, rightVar:String) extends BooleanExpr
// case class Gt(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Gtd(leftVar: String, rightVar:String) extends BooleanExpr
// case class Eq(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Eq(leftVar: String, rightVar:String) extends BooleanExpr
// case class Arith(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
// case class Arith(arith: => Double) extends BooleanExpr
