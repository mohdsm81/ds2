package edu.utah.cs.gauss.ds2.core.ir.datastructures

import edu.utah.cs.gauss.ds2.core.MyTestSpecs
import edu.utah.cs.gauss.ds2.core.ir.datastructures.Fixtures._

class LocalStateSpec extends MyTestSpecs {
  import edu.utah.cs.gauss.ds2.core.ir.datastructures.LocalState.DELIM

  val ls = new LocalState("hero")
  val variable1 = "myVar1"+DELIM+"Int"
  val variable2 = "myVar2"+DELIM+"Int"
  test("Set Var 1") {
  
    ls.setVar(variable1, 3)

    assert(ls.varToMem.contains(variable1))
    ls.getVal(variable1) should equal (3)
  }

  test("Set Var 2") {
    ls.setVar(variable2, 1)

    // assert(ls.varToMem.contains(variable2))
    assert(ls.varToMem.contains(variable2))
    ls.getVal(variable2) should equal (1)
  }

  test("var1 references var2"){
    val oldID = ls.varToMem(variable1)
    ls.setRef(variable1, variable2)
    assert(ls.garbageCollection.contains(ls.varToMem(variable1)))
    assert(!ls.garbageCollection.contains(oldID))
    assert(!ls.memToMem.contains(oldID))
    assert(!ls.memToVal.contains(oldID))
    ls.garbageCollection(ls.varToMem(variable2)) should be (2)
    ls.varToMem(variable1) should equal (ls.varToMem(variable2))
    ls.getVal(variable1) should equal (ls.getVal(variable2))
  }

  test("Var2 references itself"){
    val oldID = ls.varToMem(variable2)
    ls.setRef(variable2, variable2)
    ls.garbageCollection(ls.varToMem(variable2)) should be (2)
//    ls.garbageCollection(ls.varToMem(variable1)) should be (0)
  }

  test("Re-assignment to var2"){
    /*
     * This should assign a new object to var2, 
     * reducing its count and allocating new memory for the new object to
     * be assigned to var2. That is, it disconnects var1 from var2 (they 
     * no more point to the same value) 
     */
    
    ls.setVar(variable1, 4)
    
    ls.getVal(variable1) should equal (4)
    ls.garbageCollection(ls.varToMem(variable1)) should equal (1)
    ls.garbageCollection(ls.varToMem(variable2)) should equal (1)
    ls.varToMem(variable1) should not equal (ls.varToMem(variable2))
  }

  test("Flat collections of IR") {

    ls("agents$$List[Agent]") = List(agentInstance, agentInstance)

    val lsCopy = LocalState.fromJson(ls.toJson)


    lsCopy should equal(ls)

  }

}
