/**
 NOTE: Decided to delay this one for later on. Now the simple solution in Statement.fromJson(json) works fine.
 */


// package edu.utah.cs.gauss.ds2.core.ir.datastructures.statement
// import  edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds._


// object	FromJsonLookup {

//   private val prefix = "edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.traits.JsonDeSerializable["

//   private val companionObjectPrefix = "edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds."

//   private val suffix = "]"
//   private val register: Set[String] = Set(
//     "Ask",
//     "Become",
//     "BootStrap",
//     "Create",
//     "Else",
//     "ElseIf",
//     "Function",
//     "Get",
//     "If",
//     "Kill",
//     "Lock",
//     "ModifyState",
//     "ModifyStateRef",
//     "ResumeConsume",
//     "Send",
//     "Start",
//     "Stop",
//     "StopConsume",
//     "TimedGet",
//     "UnBecome",
//     "UnLock",
//     "UnStash",
//     "UnStashAll",
//     "While"
//   )

//   // private val lookupTable: Map[String,Class] = 

//   /** this method is coming from :
//     http://stackoverflow.com/questions/3039822/how-do-i-call-a-scala-object-method-using-reflection

//    EXAMPLE call:
//    companion[JsonDeSerializable[Ask]]("edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.Ask")
//    Then you can call methods on the companion object like this: 
//    res17.fromJson(new Ask.toJson) 
//    */
//   private def companion[T](name : String)(implicit man: Manifest[T]) : T = 
//     Class.forName(name + "$").getField("MODULE$").get(man.erasure).asInstanceOf[T]

//   def apply()(stmtClassName: String): (Boolean, Class[Statement]) = {
    
//     ???
//   }
// }
