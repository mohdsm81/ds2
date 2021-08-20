package edu.utah.cs.gauss.ds2.core.frontend.akka

import java.io._
import java.lang.reflect.Method
import java.net._

import edu.utah.cs.gauss.ds2.core.ir.datastructures._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement._
import edu.utah.cs.gauss.ds2.core.ir.datastructures.statement.kinds.{Ask, Become, ReceiveModifyState, Send, TimedGet, UnBecome, While, Else => StatementElse, If => StatementIf}
import edu.utah.cs.gauss.ds2.core.schedulers.algorithms._
import edu.utah.cs.gauss.serialization.IO
import org.apache.commons.cli._

import scala.concurrent._
import scala.io.Source
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.Flag._
import scala.reflect.runtime.universe._
import scala.sys.process._
import scala.tools.reflect.ToolBox

@Deprecated
object FrontEnd extends App {

  var uniqueVariableValue: Long = 0
  var imports: List[Tree] = List[Tree]()
  var verbose = false
  var handleExceptions = false

  def addPath(s: String) = {
    val f: File = new File(s)
    val u: URL = f.toURL
//    val urlClassLoader: URLClassLoader = ClassLoader.getSystemClassLoader.asInstanceOf[URLClassLoader]
    val urlClassLoader: URLClassLoader = new URLClassLoader(Array(), ClassLoader.getSystemClassLoader)
    val urlClass = classOf[URLClassLoader]
    val method: Method = urlClass.getDeclaredMethod("addURL", classOf[URL])
    method.setAccessible(true)
    method.invoke(urlClassLoader, u)
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def addSubToString(name: String, subName: String, names: List[String]): String = {
    var result: String = null
    if (names != null && names.contains(name))
      result = subName + "$" + name
    else result = name
    result
  }

  /*
   * Uses the list of AST expressions to create
   * a function that accepts Message and Agent as
   * parameters and returns the result of the last
   * expression in the form of an Any object.
   */
  def generateFunction(trees: List[Tree]): (Message, Agent) => Any = {
    val tb = currentMirror.mkToolBox()
    val param = (1 << 13).asInstanceOf[Long]
    //val test = Modifiers(param.asInstanceOf[FlagSet])

    /*
     * stores the initial list of ASTs into a function which
     * takes the arguments m: Message and a:Agent. This way,
     * the function can be stored directly in a statement.
     */

    /*
    println("Condition Code:")
    for(currentTree <- trees){
      println(showRaw(currentTree))
      println(showCode(currentTree))
      println()
    }
    */

    val finalTree = Block(List(
      Import(Select(Select(Select(Select(Select(Select(Select(Ident(TermName("edu")), TermName("utah")), TermName("cs")), TermName("gauss")), TermName("ds2")), TermName("core")), TermName("ir")), TermName("datastructures")), List(ImportSelector(termNames.WILDCARD, 62, null, -1))),
      ValDef(Modifiers(), TermName("test"), TypeTree(),
        Function(List(
          ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("m"), Ident(TypeName("Message")), EmptyTree),
          ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("a"), Ident(TypeName("Agent")), EmptyTree)),
          Block(imports ++ trees.dropRight(1), trees.last)))),
      Return(Ident(TermName("test"))))

    /*
    println(showRaw(finalTree))
    println()
    println(showCode(finalTree))
    println()
    */

    val function = tb.eval(finalTree).asInstanceOf[(Message, Agent) => Any]
    function
  }

  /*
   * Creates a function that assumes that
   * the result of the function will be a Boolean.
   */
  def generateConditionFunction(trees: List[Tree]): (Message, Agent) => Boolean = {
    val function = generateFunction(trees).asInstanceOf[(Message, Agent) => Boolean]
    function
  }

  /*
   * Creates a function that assumes that
   * the result of the function will be a Unit.
   */
  def generateStatementFunction(trees: List[Tree]): (Message, Agent) => Unit = {
    val function = generateFunction(trees).asInstanceOf[(Message, Agent) => Unit]
    function
  }

  /*
   * Returns the Type of the final instruction
   * in the list of trees. Requires the same
   * list of instructions prior to the final one
   * for generating the different type of functions.
   */
  def generateType(trees: List[Tree]): Type = {
    val tb = currentMirror.mkToolBox()
    val initializeAgent = tb.parse("var a: Agent = null.asInstanceOf[Agent]")
    val initializeMessage = tb.parse("var m: Message = null.asInstanceOf[Message]")

    //variable is defined as the last value of the list which is what will determine the type of the block.
    var lastTree = trees.last
    var additionalTrees = List[Tree]()
    val fakeAgentString =
      """
              class FakeAgent() {
                // actorRef functions that need to be covered
                def isTerminated : Boolean = null.asInstanceOf[Boolean]
                def path : String = null.asInstanceOf[String]
                def compareTo(other : Agent) : Int = null.asInstanceOf[Int]
                def forward(message: Any) : Unit = Unit
                def tell(msg: Any, sender: Agent) : Unit = Unit
                def tell(msg: Any) : Unit = Unit
                def !(msg: Any) : Unit = Unit
                
                // extension of functions that cover actorContext
                def actorOf(props: Any, name: String) : Agent = null.asInstanceOf[Agent] // replaced instance of Prop with Any
                def actorOf(props: Any) : Agent = null.asInstanceOf[Agent] // replaced instance of Prop with Any
                def become(behavior: Any, discardOld: Boolean = true) : Unit = Unit // replaced instance of Receive with Any
                def children : Iterable[Agent] = null.asInstanceOf[Iterable[Agent]]
                def parent : Agent = null.asInstanceOf[Agent]
                def props : Any = null // replaced instance of Prop with Any                
                def self : Agent = null.asInstanceOf[Agent]
                def sender : Agent = null.asInstanceOf[Agent]
                def unbecome() : Unit = Unit
                def actorFor(path: Any) : Agent = null.asInstanceOf[Agent]
                def actorSelection(path: String) : Agent = null.asInstanceOf[Agent]
                
                // ask extension functions to cover
                def ask(actor: Agent, message: Any)(implicit timeout: Any) : Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any
                //def ask(actor: Agent, message: Any, timeout: Any) : Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any
                def ask(message: Any)(implicit timeout: Any) : Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any
                def ask(actor: Agent, message: Any, timeoutMillis: Long) : Future[Any] = null.asInstanceOf[Future[Any]]
                //def ask(message: Any) : Future[Any] = null.asInstanceOf[Future[Any]]
                def ?(message: Any) : Future[Any] = null.asInstanceOf[Future[Any]]
              }
              """

    //println("Determining Type of following tree:")
    //println(showRaw(lastTree))

    lastTree match {

      // checks if the last tree is an apply of some kind
      case Apply(Select(variable, functionName), args) =>
        //println("Generate Type Agent Check:")
        val agent = tb.parse("Agent(\"test\")")
        val agentType = generateType(List(agent))

        //println("typeOfVariable: " + agentType)

        val typeOfVariable = generateType(trees.dropRight(1) :+ variable)
        //println("agentType: " + typeOfVariable)

        // Apply is an instance of an Agent Function call
        if (typeOfVariable <:< agentType) {

          // treats agent as instance of fake agent which has all functions implemented
          // agent.function() -> agent.asInstanceOf[FakeAgent].function()
          lastTree = Apply(Select(TypeApply(Select(variable, TermName("asInstanceOf")), List(Ident(TypeName("FakeAgent")))), functionName), args)

          // creates a FakeAgent class and adds it to the list of trees
          // used to process the type
          val fakeAgent = tb.parse(fakeAgentString)

          additionalTrees = List(fakeAgent)
        }
      case Apply(Apply(Select(variable, functionName), args), moreArgs) =>
        //println("Generate Type Agent Check:")
        val agent = tb.parse("Agent(\"test\")")
        val agentType = generateType(List(agent))

        //println("typeOfVariable: " + agentType)

        val typeOfVariable = generateType(trees.dropRight(1) :+ variable)
        //println("agentType: " + typeOfVariable)

        // Apply is an instance of an Agent Function call
        if (typeOfVariable <:< agentType) {

          // treats agent as instance of fake agent which has all functions implemented
          // agent.function() -> agent.asInstanceOf[FakeAgent].function()()
          lastTree = Apply(Apply(Select(TypeApply(Select(variable, TermName("asInstanceOf")), List(Ident(TypeName("FakeAgent")))), functionName), args), moreArgs)

          // creates a FakeAgent class and adds it to the list of trees
          // used to process the type
          val fakeAgent = tb.parse(fakeAgentString)

          additionalTrees = List(fakeAgent)
        }
      case _ =>
    }


    val typed_statement = tb.typecheck(Block(((imports :+ initializeAgent) :+ initializeMessage) ++ additionalTrees ++ trees.dropRight(1), lastTree))
    //println("Type: " + typed_statement.tpe)
    typed_statement.tpe
  }

  /*
   * Converts full named variable into standard name
   * found in code. Exception thrown if input name is
   * not a valid fullName string.
   * Example: global$test$$String -> test
   * Example: test$$String -> test
   */
  def determineOriginalName(fullName: String): String = {

    //println("Determine Original Name")
    //println("Initial Name: " + fullName)

    val splitName = fullName.split("""\$\$""")
    /*if(splitName.length < 2){
      throw new Exception("string does not contain a type")
    }*/
    /*
    for (current <- splitName){
      println(current)
    }
    */

    val noTypeName = splitName(0)
    //println("No-type Name:" + noTypeName)

    if (noTypeName.startsWith("global$")) {
      //println("Resulting Name: " + noTypeName.substring(7))
      //println()
      noTypeName.substring(7)
    }
    else {
      //println("Resulting Name: " + noTypeName)
      //println()
      noTypeName
    }
  }

  /*
   * Determines the full name of a variable used in localstate
   * returns null if no variable is found
   */
  def determineFullName(localName: String, variables: Map[Tree, Type]): String = {
    //println("Determining Full Name")
    for (currentTree <- variables.keys) {
      //println("checking tree " + showRaw(currentTree))
      // determines if the current variable is a global variable
      currentTree match {
        case Ident(TermName(name)) if name.startsWith("global$") =>
          if (localName.compareTo(name.substring(7)) == 0) {

            // makes sure that a local version of the variable doesn't exist
            var localExists = false
            for (currentKey <- variables.keys)
              localExists = localExists || currentKey.equalsStructure(Ident(TermName(name.substring(7))))
            if (!localExists) return name + "$$" + variables(currentTree)
          }
        case Ident(TermName(name)) =>
          //println("checking for Local variable: " + showRaw(a))
          if (localName.compareTo(name) == 0)
            return name + "$$" + variables(currentTree)
        case x =>
          if (verbose) println("Invalid key in variables map.")
          throw new Exception("Invalid key in variables map: " + showRaw(x))
      }
    }
    if (verbose) println("Variable " + localName + " does not exist")
    null
  }

  /*
   * converts an akka send value into an Agent Message, assigns to new variable, 
   * and returns an updated list of statements, variables, and the name of the variable
   * assigned with the message object.
   */
  def generateMessage(tree: Tree, variables: Map[Tree, Type]): (List[Statement], Map[Tree, Type], String) = {
    var messageFunction: Tree = null
    tree match {

      // handles instances of case classes (add additional check to make sure this isn't any function)
      case Apply(Ident(TermName(name)), arguments: List[Tree]) =>
        messageFunction = Apply(Select(New(Ident(TypeName("Message"))), termNames.CONSTRUCTOR), List(Literal(Constant(name)), Apply(Ident(TermName("List")), arguments)))

      // dealing with non-case class instances
      case value =>
        messageFunction = Apply(Select(New(Ident(TypeName("Message"))), termNames.CONSTRUCTOR), List(Literal(Constant("Message")), Apply(Ident(TermName("List")), List(value))))
    }

    generateAssign(messageFunction, variables)
  }

  /*
   * Generates a new assign statement for the
   * current tree.
   */
  def generateAssign(tree: Tree, variables: Map[Tree, Type]): (List[Statement], Map[Tree, Type], String) = {
    var variableName = "$variable" + uniqueVariableValue
    uniqueVariableValue = uniqueVariableValue + 1

    // generates the required valdef
    val param = (1 << 13).asInstanceOf[Long]
    val valdef = ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(variableName), TypeTree(), tree)

    // converts valdef into a statement
    val (result, resultingVariables) = this.treeToStatements(valdef, variables, bootstrap = false, List[String]())

    // determines the full name for the variable
    variableName = determineFullName(variableName, resultingVariables)

    (result, resultingVariables, variableName)
  }

  /*
   * Looks through tree for any instances of variables.keys
   * returns a list of trees that initializes each missing variable
   */
  def findCodeDependencies(tree: Tree, variables: Map[Tree, Type]): (List[Tree], Seq[String]) = {
    val tb = currentMirror.mkToolBox()
    var result: List[Tree] = List[Tree]()
    var existingDependencies = List[Tree]()
    var globalReadVariables: Seq[String] = Seq()

    for (currentTree <- variables.keys) {

      // determines if the current variable is a global variable
      currentTree match {
        case a@Ident(TermName(name)) if name.startsWith("global$") =>
          //println("checking for Global variable: " + showRaw(Ident(TermName(name.substring(7)))))
          if (tree.exists { x => x.equalsStructure(Ident(TermName(name.substring(7)))) }) {
            //println("global variable may exist")
            // makes sure that a local version of the variable doesn't exist
            var localExists = false
            for (currentKey <- variables.keys) {
              localExists = localExists || currentKey.equalsStructure(Ident(TermName(name.substring(7))))
            }
            if (!localExists) {
              //println("local version of global variable does not exist")
              existingDependencies = existingDependencies :+ a
              globalReadVariables = globalReadVariables :+ a.name.toString
            }
          }
        case a@Ident(TermName(_)) =>
          //println("checking for Local variable: " + showRaw(a))
          if (tree.exists { x => x.equalsStructure(currentTree) }) {
            //println("local version of variable found")
            existingDependencies = existingDependencies :+ a
            globalReadVariables = globalReadVariables :+ a.name.toString
          }
        case x =>
          if (verbose) println("Invalid key in variables map.")
          throw new Exception("Invalid key in variables map: " + showRaw(x))
      }
    }

    for (currentTree <- existingDependencies) {
      currentTree match {
        case Ident(TermName(name)) if name.startsWith("global$") =>
          val localStateAccess = "val " + name.substring(7) + " = a.localState.getVal(\"" + name + "\").asInstanceOf[" + variables(currentTree).toString + "]"
          val localStateAccessTree = tb.parse(localStateAccess)
          result = localStateAccessTree :: result
        case Ident(TermName(name)) =>
          val localStateAccess = "val " + name + " = a.localState.getVal(\"" + name + "\").asInstanceOf[" + variables(currentTree).toString + "]"
          val localStateAccessTree = tb.parse(localStateAccess)
          result = localStateAccessTree :: result
        case x =>
          if (verbose) println("Invalid key in variables map.")
          throw new Exception("Invalid key in variables map: " + showRaw(x))
      }
    }

    (result, globalReadVariables)
  }

  def generateMessageActionPairs(cases: List[CaseDef]): Map[String, List[Tree]] = {
    var allCases: Map[String, List[Tree]] = Map[String, List[Tree]]()
    for (currentCase <- cases) {
      var caseName: String = null
      var caseInstructions: List[Tree] = List[Tree]()
      caseInstructions = caseInstructions :+ ValDef(Modifiers(), TermName("sender"), Ident(TypeName("Agent")), Ident(TermName("a")))

      // determines the string used for case
      currentCase.pat match {
        case Apply(Ident(TermName(name)), arguments) =>
          caseName = name
          var argumentNumber = 0
          for (tree <- arguments) {
            tree match {
              case Bind(TermName(name), Typed(_, tpt)) =>
                caseInstructions = caseInstructions :+ ValDef(Modifiers(), TermName(name), tpt, TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(argumentNumber)))), TermName("asInstanceOf")), List(tpt)))
              case Bind(TermName(name), _) =>
                caseInstructions = caseInstructions :+ ValDef(Modifiers(), TermName(name), Ident(TypeName("Any")), TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(argumentNumber)))), TermName("asInstanceOf")), List(Ident(TypeName("Any")))))
              case x =>
                if (verbose) println("generateMessageActionPairs couldn't match " + showCode(x))
                throw new Exception("generateMessageActionPairs function couldn't match " + showCode(x))
            }
            argumentNumber += 1
          }
        case x =>
          if (verbose) {
            println("received different kind of pattern")
            println(showCode(x))
            println(showRaw(x))
            println()
          }
          caseName = x.toString() // need to determine if there are any other cases that we need to deal with and how to handle invalid options
      }

      // stores case instructions in list of trees.
      if (!currentCase.guard.isEmpty)
        caseInstructions = caseInstructions :+ If(currentCase.guard, currentCase.body, EmptyTree)
      else {
        currentCase.body match {
          case Block(stats, expr) =>
            caseInstructions = caseInstructions ++ stats
            caseInstructions = caseInstructions :+ expr
          case x =>
            caseInstructions = caseInstructions :+ x
        }
      }
      allCases += (caseName -> caseInstructions)
    }
    allCases
  }

  def generateInlinedFunction(funName: String, currentDef: DefDef, applyArgs: List[Tree], functions: Map[String, DefDef], parentFunctions: List[String], assignLHS: Tree = null): List[Tree] = {
    if (parentFunctions.contains(funName)) {
      println("Recursive Function Found!")
      for (currentFunction <- parentFunctions)
        println(currentFunction)
      throw new Exception("Recursive Function: " + funName)
    }
    var result: List[Tree] = List[Tree]()
    val existingNames = listOfNames(currentDef)
    var arguments = currentDef.vparamss.head
    val newDef = addSubName(currentDef, funName, existingNames).asInstanceOf[DefDef]
    var newArguments = newDef.vparamss.head

    for (currentVariable <- applyArgs) {
      currentVariable match {
        case AssignOrNamedArg(Ident(TermName(variableName)), rhs) =>
          var currentVal: ValDef = null
          var newCurrentVal: ValDef = null
          for (x <- arguments.indices) {
            val currentArgument = arguments(x)
            if (currentArgument.name.toString.contentEquals(variableName)) {
              currentVal = currentArgument
              newCurrentVal = newArguments(x)
            }
          }
          if (currentVal == null)
            throw new Exception("Received argument for non-existent value: " + variableName)
          else {
            result = result :+ ValDef(newCurrentVal.mods, newCurrentVal.name, newCurrentVal.tpt, rhs)
            arguments = arguments.diff(List(currentVal))
            newArguments = newArguments.diff(List(newCurrentVal))
          }
        case x =>
          val currentVal = arguments.head
          val newCurrentVal = newArguments.head
          result = result :+ ValDef(newCurrentVal.mods, newCurrentVal.name, newCurrentVal.tpt, x)
          arguments = arguments.diff(List(currentVal))
          newArguments = newArguments.diff(List(newCurrentVal))
      }
    }

    // runs through the rest of the code and
    // determines if recursion is necessary.
    if (assignLHS == null) {
      newDef.rhs match {
        case Block(stats, expr) =>
          for (currentStats <- stats)
            result = result ++ pullOutFunction(currentStats, functions, parentFunctions :+ funName)
          val newExpr = pullOutFunction(expr, functions, parentFunctions :+ funName)
          result = result ++ newExpr
        case x =>
          val newX = pullOutFunction(x, functions, parentFunctions :+ funName)
          result = result ++ newX
      }
    }
    // if result of function is assigned to a variable,
    // makes sure that return and possibly the final variable
    // are assigned to the correct variable.
    else {
      newDef.rhs match {
        case Block(stats, expr) =>
          for (currentStats <- stats)
            result = result ++ pullOutFunction(currentStats, functions, parentFunctions :+ funName)
          val newExpr = pullOutFunction(expr, functions, parentFunctions :+ funName)
          result = result ++ newExpr.dropRight(1)
          newDef.tpt match {
            case Ident(TypeName("Unit")) => result = result :+ newExpr.last
            case Select(Ident(TermName("scala")), TypeName("Unit")) => result = result :+ newExpr.last
            case _ => result = result :+ Assign(assignLHS, newExpr.last)
          }
        case x =>
          val newX = pullOutFunction(x, functions, parentFunctions :+ funName)
          result = result ++ newX.dropRight(1)
          result = result :+ Assign(assignLHS, newX.last)
      }
    }
    result
  }

  /*
   * Used to inline first level function calls
   * in the message responses without creating
   * unnecessary 
   * .
   */
  def inlineFunctions(commands: List[Tree], functions: Map[String, DefDef]): List[Tree] = {
    var result = List[Tree]()
    for (currentCommand <- commands) {
      currentCommand match {
        case Apply(Ident(TermName(funName)), args) =>
          var newArgs = List[Tree]()
          for (currentArg <- args) {
            val values = pullOutFunction(currentArg, functions, List[String]())
            result = result ++ values.dropRight(1)
            newArgs = newArgs :+ values.last
          }
          if (functions.contains(funName))
            result = result ++ generateInlinedFunction(funName, functions(funName), args, functions, List[String](), null)
          else result = result :+ Apply(Ident(TermName(funName)), newArgs)
        case Apply(Select(This(typeNames.EMPTY), TermName(funName)), args) =>
          var newArgs = List[Tree]()
          for (currentArg <- args) {
            val values = pullOutFunction(currentArg, functions, List[String]())
            result = result ++ values.dropRight(1)
            newArgs = newArgs :+ values.last
          }
          if (functions.contains(funName))
            result = result ++ generateInlinedFunction(funName, functions(funName), args, functions, List[String](), null)
          else result = result :+ Apply(Ident(TermName(funName)), newArgs)
        /*
         * If the result of the function is being assigned directly,
         * will either modify the result of the function to directly assign the
         * variable or ignore if inlining isn't necessary.
         */
        /*case valdef @ ValDef(modifiers, term name, tpt, Apply(Ident(TermName(funName)), args)) => {
          println("Function call assignment:")
          println(showRaw(valdef))
          println(valdef)
          println()
          result = result :+ valdef
        }
        case valdef @ ValDef(modifiers, term name, tpt, Apply(Select(This(typeNames.EMPTY), TermName(funName)), args)) => {
          println("Function call assignment:")
          println(showRaw(valdef))
          println(valdef)
          println()
          result = result :+ valdef
        }
        case assign @ Assign(lhs, Apply(Ident(TermName(funName)), args)) => {
          println("Function call assignment:")
          println(showRaw(assign))
          println(assign)
          println()
          result = result :+ assign
        }
        case assign @ Assign(lhs, Apply(Select(This(typeNames.EMPTY), TermName(funName)), args)) => {
          println("Function call assignment:")
          println(showRaw(assign))
          println(assign)
          println()
          result = result :+ assign
        }*/
        /*
         * Check to see if there are any instances of apply inside of
         * the Tree. If so, create new variable that is result
         * of the function and replace the instance of the function
         * call in the tree with the variable.
         */
        case x => result = result ++ pullOutFunction(x, functions, List[String]())
      }
    }
    result
  }

  def listOfNames(currentFunction: Tree): List[String] = {
    var existingNames: List[String] = List[String]()
    currentFunction match {
      case tree@ValDef(_, TermName(name), _, _) =>
        existingNames = existingNames.+:(name)
        for (subTree <- tree.children)
          existingNames = existingNames ++ listOfNames(subTree)
      case x =>
        for (subTree <- x.children)
          existingNames = existingNames ++ listOfNames(subTree)
    }
    existingNames
  }

  def addSubName(tree: Tree, subName: String, existingNames: List[String]): Tree = {
    var result: Tree = null
    tree match {
      case EmptyTree =>
        result = EmptyTree
      // package pid { stats }
      case PackageDef(pid, stats) =>
        result = PackageDef(pid, stats.map { x => addSubName(x, subName, existingNames) })

      // mods class name [tparams] impl   where impl = extends parents { defs }
      case ClassDef(mods, name, tparams, impl) =>
        result = ClassDef(mods, name, tparams.map { x => addSubName(x, subName, existingNames).asInstanceOf[TypeDef] }, addSubName(impl, subName, existingNames).asInstanceOf[Template])

      // mods object name impl  where impl = extends parents { defs }
      case ModuleDef(mods, TermName(name), impl) =>
        result = ModuleDef(mods, TermName(addSubToString(name, subName, existingNames)), addSubName(impl, subName, existingNames).asInstanceOf[Template])

      // mods val name: tpt = rhs   
      // note missing type information is expressed by tpt = TypeTree()
      case ValDef(mods, TermName(name), tpt, rhs) =>
        result = ValDef(mods, TermName(addSubToString(name, subName, existingNames)), addSubName(tpt, subName, existingNames), addSubName(rhs, subName, existingNames))

      // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
      // note missing type information is expressed by tpt = TypeTree()
      case DefDef(mods, TermName(name), tparams, vparams, tpt, rhs) =>
        result = DefDef(mods, TermName(name), tparams.map { x => addSubName(x, subName, existingNames).asInstanceOf[TypeDef] }, vparams.map { x => x.map { y => addSubName(y, subName, existingNames).asInstanceOf[ValDef] } }, addSubName(tpt, subName, existingNames), addSubName(rhs, subName, existingNames))

      // mods type name[tparams] = rhs
      // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree, and DEFERRED is set in mods
      case TypeDef(mods, name, tparams, rhs) =>
        result = TypeDef(mods, name, tparams.map { x => addSubName(x, subName, existingNames).asInstanceOf[TypeDef] }, addSubName(rhs, subName, existingNames))

      // used for tail calls and like
      // while/do are desugared to label defs as follows:
      // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
      // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
      case LabelDef(TermName(name), params, rhs) =>
        result = LabelDef(TermName(name /*addSubToString(name, subName, existingNames)*/), params.map { x => addSubName(x, subName, existingNames).asInstanceOf[Ident] }, addSubName(rhs, subName, existingNames))

      // import expr.{selectors}
      // Selectors are a list of pairs of names (from, to).
      // The last (and maybe only name) may be a nme.WILDCARD
      // for instance
      //   import qual.{x, y => z, _}  would be represented as
      //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
      case Import(expr, selectors) =>
        result = Import(addSubName(expr, subName, existingNames), selectors.map { x => {
          var newName = x.name
          var newRename = x.rename
          if (x.name != null && x.name.isTermName)
            newName = TermName(addSubToString(x.name.toString, subName, existingNames))
          if (x.rename != null && x.rename.isTermName)
            newRename = TermName(addSubToString(x.rename.toString, subName, existingNames))
          ImportSelector(newName, x.namePos, newRename, x.renamePos)
        }
        })

      // extends parents { self => body }
      // if self is missing it is represented as emptyValDef
      case Template(parents, self, body) =>
        result = Template(parents, addSubName(self, subName, existingNames).asInstanceOf[ValDef], body.map { x => addSubName(x, subName, existingNames) })

      // { stats; expr }
      case Block(stats, expr) =>
        result = Block(stats.map { x => addSubName(x, subName, existingNames) }, addSubName(expr, subName, existingNames))

      // case pat if guard => body
      case CaseDef(pat, guard, body) =>
        result = CaseDef(addSubName(pat, subName, existingNames), addSubName(guard, subName, existingNames), addSubName(body, subName, existingNames))

      // pat1 | ... | patn
      case Alternative(trees) =>
        result = Alternative(trees.map { x => addSubName(x, subName, existingNames) })

      // pat*
      case Star(elem) =>
        result = Star(addSubName(elem, subName, existingNames))

      // name @ pat
      /*case Bind(TermName(name), body) => {
        result = Bind(TermName(addSubToString(name, subName, existingNames)), addSubName(body, subName, existingNames))
      }*/

      case Bind(name, body) =>
        result = Bind(name, addSubName(body, subName, existingNames))

      // used for unapply's
      case UnApply(fun, args) =>
        result = UnApply(addSubName(fun, subName, existingNames), args.map { x => addSubName(x, subName, existingNames) })

      // vparams => body  where vparams:List[ValDef]
      case Function(vparams, body) =>
        result = Function(vparams.map { x => addSubName(x, subName, existingNames).asInstanceOf[ValDef] }, addSubName(body, subName, existingNames))

      // lhs = rhs
      case Assign(lhs, rhs) =>
        result = Assign(addSubName(lhs, subName, existingNames), addSubName(rhs, subName, existingNames))

      // lhs = rhs
      case AssignOrNamedArg(lhs, rhs) =>
        result = AssignOrNamedArg(addSubName(lhs, subName, existingNames), addSubName(rhs, subName, existingNames))

      // if (cond) thenp else elsep
      case If(cond, thenp, elsep) =>
        result = If(addSubName(cond, subName, existingNames), addSubName(thenp, subName, existingNames), addSubName(elsep, subName, existingNames))

      // selector match { cases }
      case Match(selector, cases) =>
        result = Match(addSubName(selector, subName, existingNames), cases.map { x => addSubName(x, subName, existingNames).asInstanceOf[CaseDef] })

      // return expr
      case Return(expr) =>
        result = Return(addSubName(expr, subName, existingNames))

      // try block catch { catches } finally finalizer where catches: List[CaseDef]
      case Try(block, catches, finalizer) =>
        result = Try(addSubName(block, subName, existingNames), catches.map { x => addSubName(x, subName, existingNames).asInstanceOf[CaseDef] }, addSubName(finalizer, subName, existingNames))

      // throw expr
      case Throw(expr) =>
        result = Throw(addSubName(expr, subName, existingNames))

      // new tpt   always in the context: (new tpt).<init>[targs](args)
      case New(tpt) =>
        result = New(addSubName(tpt, subName, existingNames))

      // expr: tpt
      case Typed(expr, tpt) =>
        result = Typed(addSubName(expr, subName, existingNames), addSubName(tpt, subName, existingNames))

      // fun[args]
      case TypeApply(fun, args) =>
        result = TypeApply(fun, args.map { x => addSubName(x, subName, existingNames) })

      // fun(args)
      // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
      case Apply(fun, args) =>
        /*println(tree)
        println(fun)
        println(showRaw(fun))
        println(args)
        println(showRaw(args))
        println()*/
        result = Apply(addSubName(fun, subName, existingNames), args.map { x => addSubName(x, subName, existingNames) })

      // qual.super[mix]     if qual and/or mix is empty, there are nme.EMPTY.toTypeName
      case Super(qual, mix) =>
        result = Super(addSubName(qual, subName, existingNames), mix)

      // qual.this
      case This(qual) =>
        result = This(qual)

      // qualifier.selector
      case Select(qualifier, TermName(name)) =>
        result = Select(addSubName(qualifier, subName, existingNames), TermName(addSubToString(name, subName, existingNames)))

      case Select(qualifier, selector) =>
        result = Select(addSubName(qualifier, subName, existingNames), selector)

      // name
      // note: type checker converts idents that refer to enclosing fields or methods
      // to selects; name ==> this.name
      case Ident(TermName(name)) =>
        result = Ident(TermName(addSubToString(name, subName, existingNames)))

      case Ident(name) =>
        result = Ident(name)

      // value
      case Literal(value) =>
        result = Literal(value)

      // a type that's not written out, but given in the tpe attribute
      case TypeTree() =>
        result = TypeTree()

      // arg @annot  for types,  arg: @annot for exprs
      case Annotated(annot, arg) =>
        result = Annotated(addSubName(annot, subName, existingNames), addSubName(arg, subName, existingNames))

      // ref.type
      case SingletonTypeTree(ref) =>
        result = SingletonTypeTree(addSubName(ref, subName, existingNames))

      // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
      case SelectFromTypeTree(qualifier, selector) =>
        result = SelectFromTypeTree(addSubName(qualifier, subName, existingNames), selector)

      // parent1 with ... with parentN { refinement }
      case CompoundTypeTree(templ) =>
        result = CompoundTypeTree(addSubName(templ, subName, existingNames).asInstanceOf[Template])

      // tpt[args]
      case AppliedTypeTree(tpt, args) =>
        result = AppliedTypeTree(addSubName(tpt, subName, existingNames), args.map { x => addSubName(x, subName, existingNames) })

      // >: lo <: hi
      case TypeBoundsTree(lo, hi) =>
        result = TypeBoundsTree(addSubName(lo, subName, existingNames), addSubName(hi, subName, existingNames))

      // tpt forSome { whereClauses }
      case ExistentialTypeTree(tpt, whereClauses) =>
        result = ExistentialTypeTree(addSubName(tpt, subName, existingNames), whereClauses)

      case x =>
        result = x
    }

    result
  }

  /**
   * Modifies the given tree so that all objects
   * declared as either ActorRef or ActorSelection
   * are replaced with an instance of a DS2 Agent
   */
  def akkaToAgentSwap(tree: Tree): Tree = {
    var result: Tree = null
    tree match {
      case ValDef(mods, name, tpt, rhs) =>
        result = ValDef(mods, name, akkaToAgentSwap(tpt), akkaToAgentSwap(rhs))

      case AppliedTypeTree(tpt, args) =>
        result = AppliedTypeTree(tpt, args.map { x => akkaToAgentSwap(x) })

      case Apply(fun, args) =>
        result = Apply(akkaToAgentSwap(fun), args.map { x => akkaToAgentSwap(x) })

      case TypeApply(fun, args) =>
        result = TypeApply(fun, args.map { x => akkaToAgentSwap(x) })

      case New(tpt) =>
        result = New(akkaToAgentSwap(tpt))

      case Select(qualifier, name) =>
        if (name.isTypeName && (name.toString.contentEquals("ActorRef") || name.toString.contentEquals("ActorSelection"))) {
          result = Ident(TypeName("Agent"))
        } else {
          result = Select(akkaToAgentSwap(qualifier), name)
        }

      case Ident(TypeName("ActorRef")) =>
        result = Ident(TypeName("Agent"))

      case Ident(TypeName("ActorSelection")) =>
        result = Ident(TypeName("Agent"))

      case EmptyTree =>
        result = EmptyTree

      // package pid { stats }
      case PackageDef(pid, stats) =>
        result = PackageDef(pid, stats.map { x => akkaToAgentSwap(x) })

      // mods class name [tparams] impl   where impl = extends parents { defs }
      case ClassDef(mods, name, tparams, impl) =>
        result = ClassDef(mods, name, tparams.map { x => akkaToAgentSwap(x).asInstanceOf[TypeDef] }, akkaToAgentSwap(impl).asInstanceOf[Template])

      // mods object name impl  where impl = extends parents { defs }
      case ModuleDef(mods, TermName(name), impl) =>
        result = ModuleDef(mods, TermName(name), akkaToAgentSwap(impl).asInstanceOf[Template])


      // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
      // note missing type information is expressed by tpt = TypeTree()
      case DefDef(mods, TermName(name), tparams, vparams, tpt, rhs) =>
        result = DefDef(mods, TermName(name), tparams.map { x => akkaToAgentSwap(x).asInstanceOf[TypeDef] }, vparams.map { x => x.map { y => akkaToAgentSwap(y).asInstanceOf[ValDef] } }, akkaToAgentSwap(tpt), akkaToAgentSwap(rhs))

      // mods type name[tparams] = rhs
      // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree, and DEFERRED is set in mods
      case TypeDef(mods, name, tparams, rhs) =>
        result = TypeDef(mods, name, tparams.map { x => akkaToAgentSwap(x).asInstanceOf[TypeDef] }, akkaToAgentSwap(rhs))

      // used for tail calls and like
      // while/do are desugared to label defs as follows:
      // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
      // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
      case LabelDef(TermName(name), params, rhs) =>
        result = LabelDef(TermName(name), params.map { x => akkaToAgentSwap(x).asInstanceOf[Ident] }, akkaToAgentSwap(rhs))

      // import expr.{selectors}
      // Selectors are a list of pairs of names (from, to).
      // The last (and maybe only name) may be a nme.WILDCARD
      // for instance
      //   import qual.{x, y => z, _}  would be represented as
      //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
      case Import(expr, selectors) =>
        result = Import(akkaToAgentSwap(expr), selectors.map { x => {
          val newName = x.name
          val newRename = x.rename
          ImportSelector(newName, x.namePos, newRename, x.renamePos)
        }
        })

      // extends parents { self => body }
      // if self is missing it is represented as emptyValDef
      case Template(parents, self, body) =>
        result = Template(parents, akkaToAgentSwap(self).asInstanceOf[ValDef], body.map { x => akkaToAgentSwap(x) })

      // { stats; expr }
      case Block(stats, expr) =>
        result = Block(stats.map { x => akkaToAgentSwap(x) }, akkaToAgentSwap(expr))

      // case pat if guard => body
      case CaseDef(pat, guard, body) =>
        result = CaseDef(akkaToAgentSwap(pat), akkaToAgentSwap(guard), akkaToAgentSwap(body))

      // pat1 | ... | patn
      case Alternative(trees) =>
        result = Alternative(trees.map { x => akkaToAgentSwap(x) })

      // pat*
      case Star(elem) =>
        result = Star(akkaToAgentSwap(elem))

      // name @ pat
      /*case Bind(TermName(name), body) => {
        result = Bind(TermName(name), akkaToAgentSwap(body, subName, existingNames))
      }*/

      case Bind(name, body) =>
        result = Bind(name, akkaToAgentSwap(body))

      // used for unapply's
      case UnApply(fun, args) =>
        result = UnApply(akkaToAgentSwap(fun), args.map { x => akkaToAgentSwap(x) })

      // vparams => body  where vparams:List[ValDef]
      case Function(vparams, body) =>
        result = Function(vparams.map { x => akkaToAgentSwap(x).asInstanceOf[ValDef] }, akkaToAgentSwap(body))

      // lhs = rhs
      case Assign(lhs, rhs) =>
        result = Assign(akkaToAgentSwap(lhs), akkaToAgentSwap(rhs))

      // lhs = rhs
      case AssignOrNamedArg(lhs, rhs) =>
        result = AssignOrNamedArg(akkaToAgentSwap(lhs), akkaToAgentSwap(rhs))

      // if (cond) thenp else elsep
      case If(cond, thenp, elsep) =>
        result = If(akkaToAgentSwap(cond), akkaToAgentSwap(thenp), akkaToAgentSwap(elsep))

      // selector match { cases }
      case Match(selector, cases) =>
        result = Match(akkaToAgentSwap(selector), cases.map { x => akkaToAgentSwap(x).asInstanceOf[CaseDef] })

      // return expr
      case Return(expr) =>
        result = Return(akkaToAgentSwap(expr))

      // try block catch { catches } finally finalizer where catches: List[CaseDef]
      case Try(block, catches, finalizer) =>
        result = Try(akkaToAgentSwap(block), catches.map { x => akkaToAgentSwap(x).asInstanceOf[CaseDef] }, akkaToAgentSwap(finalizer))

      // throw expr
      case Throw(expr) =>
        result = Throw(akkaToAgentSwap(expr))

      // expr: tpt
      case Typed(expr, tpt) =>
        result = Typed(akkaToAgentSwap(expr), akkaToAgentSwap(tpt))

      // qual.super[mix]     if qual and/or mix is empty, there are nme.EMPTY.toTypeName
      case Super(qual, mix) =>
        result = Super(akkaToAgentSwap(qual), mix)

      // qual.this
      case This(qual) =>
        result = This(qual)

      // name
      // note: type checker converts idents that refer to enclosing fields or methods
      // to selects; name ==> this.name
      case Ident(name) =>
        result = Ident(name)

      // value
      case Literal(value) =>
        result = Literal(value)

      // a type that's not written out, but given in the tpe attribute
      case TypeTree() =>
        result = TypeTree()

      // arg @annot  for types,  arg: @annot for exprs
      case Annotated(annot, arg) =>
        result = Annotated(akkaToAgentSwap(annot), akkaToAgentSwap(arg))

      // ref.type
      case SingletonTypeTree(ref) =>
        result = SingletonTypeTree(akkaToAgentSwap(ref))

      // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
      case SelectFromTypeTree(qualifier, selector) =>
        result = SelectFromTypeTree(akkaToAgentSwap(qualifier), selector)

      // parent1 with ... with parentN { refinement }
      case CompoundTypeTree(templ) =>
        result = CompoundTypeTree(akkaToAgentSwap(templ).asInstanceOf[Template])

      // >: lo <: hi
      case TypeBoundsTree(lo, hi) =>
        result = TypeBoundsTree(akkaToAgentSwap(lo), akkaToAgentSwap(hi))

      // tpt forSome { whereClauses }
      case ExistentialTypeTree(tpt, whereClauses) =>
        result = ExistentialTypeTree(akkaToAgentSwap(tpt), whereClauses)

      case x =>
        result = x
    }

    result
  }

  def pullOutFunction(tree: Tree, functions: Map[String, DefDef], parentFunctions: List[String]): List[Tree] = {
    var result: List[Tree] = List[Tree]()
    tree match {
      case EmptyTree =>
        result = result ++ List(EmptyTree)
      // package pid { stats }
      case PackageDef(pid, stats) =>
        result = result ++ List(PackageDef(pid, stats.flatMap { x => pullOutFunction(x, functions, parentFunctions) }))

      // mods class name [tparams] impl   where impl = extends parents { defs }
      case ClassDef(mods, name, tparams, impl) =>
        val newImpl = pullOutFunction(impl, functions, parentFunctions)
        result = result ++ newImpl.dropRight(1)
        result = result ++ List(ClassDef(mods, name, tparams, newImpl.last.asInstanceOf[Template]))

      // mods object name impl  where impl = extends parents { defs }
      case ModuleDef(mods, name, impl) =>
        result = result ++ List(ModuleDef(mods, name, pullOutFunction(impl, functions, parentFunctions).last.asInstanceOf[Template]))

      // mods val name: tpt = rhs   
      // note missing type information is expressed by tpt = TypeTree()
      case ValDef(mods, name, tpt, rhs) =>
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = newTPT.dropRight(1)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(ValDef(mods, name, newTPT.last, newRHS.last))

      // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
      // note missing type information is expressed by tpt = TypeTree()
      case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
        var newTPARAMS = List[TypeDef]()
        for (currentTPARAM <- tparams) {
          val values = pullOutFunction(currentTPARAM, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newTPARAMS = newTPARAMS :+ values.last.asInstanceOf[TypeDef]
        }
        var newVPARAMS = List[List[ValDef]]()
        for (valdefs <- vparams) {
          var currentDefs = List[ValDef]()
          for (currentDef <- valdefs) {
            val values = pullOutFunction(currentDef, functions, parentFunctions)
            result = result ++ values.dropRight(1)
            currentDefs = currentDefs :+ values.last.asInstanceOf[ValDef]
          }
          newVPARAMS = newVPARAMS :+ currentDefs
        }
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = result ++ newTPT.dropRight(1)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(DefDef(mods, name, newTPARAMS, newVPARAMS, newTPT.last, newRHS.last))

      // mods type name[tparams] = rhs
      // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree, and DEFERRED is set in mods
      case TypeDef(mods, name, tparams, rhs) =>
        var newTPARAMS = List[TypeDef]()
        for (currentTPARAM <- tparams) {
          val values = pullOutFunction(currentTPARAM, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newTPARAMS = newTPARAMS :+ values.last.asInstanceOf[TypeDef]
        }
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(TypeDef(mods, name, newTPARAMS, newRHS.last))

      // used for tail calls and like
      // while/do are desugared to label defs as follows:
      // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
      // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
      case LabelDef(name, params, rhs) =>
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(LabelDef(name, params, newRHS.last))

      // import expr.{selectors}
      // Selectors are a list of pairs of names (from, to).
      // The last (and maybe only name) may be a nme.WILDCARD
      // for instance
      //   import qual.{x, y => z, _}  would be represented as
      //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
      case Import(expr, selectors) =>
        val newEXPR = pullOutFunction(expr, functions, parentFunctions)
        result = result ++ newEXPR.dropRight(1)
        result = result ++ List(Import(newEXPR.last, selectors))

      // extends parents { self => body }
      // if self is missing it is represented as emptyValDef
      case Template(parents, self, body) =>
        var newParents = List[Tree]()
        for (currentParent <- parents) {
          val values = pullOutFunction(currentParent, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newParents = newParents :+ values.last
        }
        val newSelf = pullOutFunction(self, functions, parentFunctions)
        result = result ++ newSelf.dropRight(1)
        result = result ++ List(Template(newParents, newSelf.last.asInstanceOf[ValDef], body.flatMap { x => pullOutFunction(x, functions, parentFunctions) }))

      // { stats; expr }
      case Block(stats, expr) =>
        var newStats = stats.flatMap { x => pullOutFunction(x, functions, parentFunctions) }
        val newExpr = pullOutFunction(expr, functions, parentFunctions)
        newStats = newStats ++ newExpr.dropRight(1)
        result = List(Block(newStats, newExpr.last))

      // case pat if guard => body
      case CaseDef(pat, guard, body) => // place guard into body if it exists.........................................................
        val newPat = pullOutFunction(pat, functions, parentFunctions)
        result = result ++ newPat.dropRight(1)
        var newBody = pullOutFunction(body, functions, parentFunctions)
        if (newBody.length > 1) {
          newBody = List(Block(newBody.dropRight(1), newBody.last))
        }
        if (!guard.isEmpty) {
          val newGuard = pullOutFunction(guard, functions, parentFunctions)
          newBody = List(Block(newGuard.dropRight(1), If(newGuard.last, newBody.last, EmptyTree)))
        }
        result = result ++ List(CaseDef(newPat.last, EmptyTree, newBody.last))

      // pat1 | ... | patn
      case Alternative(trees) =>
        var newTrees = List[Tree]()
        for (currentTree <- trees) {
          val values = pullOutFunction(currentTree, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newTrees = newTrees :+ values.last
        }
        result = result ++ List(Alternative(newTrees))

      // pat*
      case Star(elem) =>
        val newElem = pullOutFunction(elem, functions, parentFunctions)
        result = result ++ newElem.dropRight(1)
        result = result ++ List(Star(newElem.last))

      // name @ pat
      case Bind(name, body) =>
        val newBody = pullOutFunction(body, functions, parentFunctions)
        result = result ++ newBody.dropRight(1)
        result = result ++ List(Bind(name, newBody.last))

      // used for unapply's
      case UnApply(fun, args) =>
        val newFun = pullOutFunction(fun, functions, parentFunctions)
        result = result ++ newFun.dropRight(1)
        var newArgs = List[Tree]()
        for (currentArg <- args) {
          val values = pullOutFunction(currentArg, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        result = result ++ List(UnApply(newFun.last, newArgs))

      // vparams => body  where vparams:List[ValDef]
      case Function(vparams, body) =>
        var newVPARAMS = List[ValDef]()
        for (currentVPARAMS <- vparams) {
          val values = pullOutFunction(currentVPARAMS, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newVPARAMS = newVPARAMS :+ values.last.asInstanceOf[ValDef]
        }
        var newBody = pullOutFunction(body, functions, parentFunctions)
        if (newBody.length > 1) {
          newBody = List(Block(newBody.dropRight(1), newBody.last))
        }
        result = result ++ List(Function(newVPARAMS, newBody.last))

      // lhs = rhs      
      case Assign(lhs, rhs) =>
        val newLHS = pullOutFunction(lhs, functions, parentFunctions)
        result = result ++ newLHS.dropRight(1)
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(Assign(newLHS.last, newRHS.last))

      // lhs = rhs
      case AssignOrNamedArg(lhs, rhs) =>
        val newLHS = pullOutFunction(lhs, functions, parentFunctions)
        result = result ++ newLHS.dropRight(1)
        val newRHS = pullOutFunction(rhs, functions, parentFunctions)
        result = result ++ newRHS.dropRight(1)
        result = result ++ List(AssignOrNamedArg(newLHS.last, newRHS.last))

      // if (cond) thenp else elsep
      case If(cond, thenp, elsep) =>
        val newCond = pullOutFunction(cond, functions, parentFunctions)
        result = result ++ newCond.dropRight(1)
        var newThenp = pullOutFunction(thenp, functions, parentFunctions)
        if (newThenp.length > 1) {
          newThenp = List(Block(newThenp.dropRight(1), newThenp.last))
        }
        var newElsep = pullOutFunction(elsep, functions, parentFunctions)
        if (newElsep.length > 1) {
          newElsep = List(Block(newElsep.dropRight(1), newElsep.last))
        }
        result = result ++ List(If(newCond.last, newThenp.last, newElsep.last))

      // selector match { cases }
      case Match(selector, cases) =>
        val newSelector = pullOutFunction(selector, functions, parentFunctions)
        result = result ++ newSelector.dropRight(1)
        var newCases = List[CaseDef]()
        for (currentCase <- cases) {
          val values = pullOutFunction(currentCase, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newCases = newCases :+ values.last.asInstanceOf[CaseDef]
        }
        result = result ++ List(Match(newSelector.last, newCases))

      // return expr
      case Return(expr) =>
        val newExpr = pullOutFunction(expr, functions, parentFunctions)
        result = result ++ newExpr.dropRight(1)
        result = result ++ List(Return(newExpr.last))

      // try block catch { catches } finally finalizer where catches: List[CaseDef]
      case Try(block, catches, finalizer) =>
        val newBlock = pullOutFunction(block, functions, parentFunctions)
        result = result ++ newBlock.dropRight(1)
        var newCatches = List[CaseDef]()
        for (currentCatches <- catches) {
          val values = pullOutFunction(currentCatches, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newCatches = newCatches :+ values.last.asInstanceOf[CaseDef]
        }
        val newFinalizer = pullOutFunction(finalizer, functions, parentFunctions)
        result = result ++ newFinalizer.dropRight(1)
        result = result ++ List(Try(newBlock.last, newCatches, newFinalizer.last))

      // throw expr
      case Throw(expr) =>
        val newExpr = pullOutFunction(expr, functions, parentFunctions)
        result = result ++ newExpr.dropRight(1)
        result = result ++ List(Throw(newExpr.last))

      // new tpt   always in the context: (new tpt).<init>[targs](args)
      case New(tpt) =>
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        result = result ++ newTPT.dropRight(1)
        result = result ++ List(New(newTPT.last))

      // expr: tpt
      case Typed(expr, tpt) =>
        val newExpr = pullOutFunction(expr, functions, parentFunctions)
        result = result ++ newExpr.dropRight(1)
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        result = result ++ newTPT.dropRight(1)
        result = result ++ List(Typed(newExpr.last, newTPT.last))

      // fun[args]
      case TypeApply(fun, args) =>
        val newFun = pullOutFunction(fun, functions, parentFunctions)
        result = result ++ newFun.dropRight(1)
        var newArgs = List[Tree]()
        for (currentArg <- args) {
          val values = pullOutFunction(currentArg, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        result = result ++ List(TypeApply(newFun.last, newArgs))

      // fun(args)
      // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
      // modify following two functions to completely inline functions and map possible results to valdef.......................................................................................................
      case Apply(Ident(TermName(funName)), args) =>
        var newArgs = List[Tree]()
        for (currentArg <- args) {
          val values = pullOutFunction(currentArg, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        if (functions.contains(funName)) {
          val newName = "$variable" + uniqueVariableValue
          val param = (1 << 12).asInstanceOf[Long]
          result = result :+ ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newName), functions(funName).tpt, TypeApply(Select(Literal(Constant(null)), TermName("asInstanceOf")), List(functions(funName).tpt)))
          uniqueVariableValue = uniqueVariableValue + 1
          result = result ++ generateInlinedFunction(funName, functions(funName), newArgs, functions, parentFunctions, Ident(TermName(newName)))
          result = result :+ Ident(TermName(newName))
        }
        else {
          result = result :+ Apply(Ident(TermName(funName)), newArgs)
        }
      case Apply(Select(This(typeNames.EMPTY), TermName(funName)), args) =>
        var newArgs = List[Tree]()
        for (currentArg <- args) {
          val values = pullOutFunction(currentArg, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        if (functions.contains(funName)) {
          val newName = "$variable" + uniqueVariableValue
          val param = (1 << 12).asInstanceOf[Long]
          result = result :+ ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newName), functions(funName).tpt, TypeApply(Select(Literal(Constant(null)), TermName("asInstanceOf")), List(functions(funName).tpt)))
          uniqueVariableValue = uniqueVariableValue + 1
          result = result ++ generateInlinedFunction(funName, functions(funName), newArgs, functions, parentFunctions, Ident(TermName(newName)))
          result = result :+ Ident(TermName(newName))
        }
        else {
          result = result :+ Apply(Ident(TermName(funName)), newArgs)
        }
      case Apply(fun, args) =>
        val newFun = pullOutFunction(fun, functions, parentFunctions)
        result = result ++ newFun.dropRight(1)
        var newArgs = List[Tree]()
        for (currentArg <- args) {
          val values = pullOutFunction(currentArg, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        result = result ++ List(Apply(newFun.last, newArgs))

      // qual.super[mix]     if qual and/or mix is empty, there are nme.EMPTY.toTypeName
      case Super(qual, mix) =>
        val newQual = pullOutFunction(qual, functions, parentFunctions)
        result = result ++ newQual.dropRight(1)
        result = result ++ List(Super(newQual.last, mix))

      // qual.this
      case This(qual) =>
        result = result ++ List(This(qual))

      // qualifier.selector
      case Select(qualifier, selector) =>
        val newQualifier = pullOutFunction(qualifier, functions, parentFunctions)
        result = result ++ newQualifier.dropRight(1)
        result = result ++ List(Select(newQualifier.last, selector))

      // name
      // note: type checker converts idents that refer to enclosing fields or methods
      // to selects; name ==> this.name
      case Ident(name) =>
        result = result ++ List(Ident(name))

      // value
      case Literal(value) =>
        result = result ++ List(Literal(value))

      // a type that's not written out, but given in the tpe attribute
      case TypeTree() =>
        result = result ++ List(TypeTree())

      // arg @annot  for types,  arg: @annot for exprs
      case Annotated(annot, arg) =>
        val newAnnot = pullOutFunction(annot, functions, parentFunctions)
        result = result ++ newAnnot.dropRight(1)
        val newArg = pullOutFunction(arg, functions, parentFunctions)
        result = result ++ newArg.dropRight(1)
        result = result ++ List(Annotated(newAnnot.last, newArg.last))

      // ref.type
      case SingletonTypeTree(ref) =>
        val newRef = pullOutFunction(ref, functions, parentFunctions)
        result = result ++ newRef.dropRight(1)
        result = result ++ List(SingletonTypeTree(newRef.last))

      // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
      case SelectFromTypeTree(qualifier, selector) =>
        val newQualifier = pullOutFunction(qualifier, functions, parentFunctions)
        result = result ++ newQualifier.dropRight(1)
        result = result ++ List(SelectFromTypeTree(newQualifier.last, selector))

      // parent1 with ... with parentN { refinement }
      case CompoundTypeTree(templ) =>
        val newCompoundTypeTree = pullOutFunction(templ, functions, parentFunctions)
        result = result ++ newCompoundTypeTree.dropRight(1)
        result = result ++ List(CompoundTypeTree(newCompoundTypeTree.last.asInstanceOf[Template]))

      // tpt[args]
      case AppliedTypeTree(tpt, args) =>
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        result = result ++ newTPT.dropRight(1)
        var newArgs = List[Tree]()
        for (currentArgs <- args) {
          val values = pullOutFunction(currentArgs, functions, parentFunctions)
          result = result ++ values.dropRight(1)
          newArgs = newArgs :+ values.last
        }
        result = result ++ List(AppliedTypeTree(newTPT.last, newArgs))

      // >: lo <: hi
      case TypeBoundsTree(lo, hi) =>
        val newLO = pullOutFunction(lo, functions, parentFunctions)
        result = result ++ newLO.dropRight(1)
        val newHI = pullOutFunction(hi, functions, parentFunctions)
        result = result ++ newHI.dropRight(1)
        result = result ++ List(TypeBoundsTree(newLO.last, newHI.last))

      // tpt forSome { whereClauses }
      case ExistentialTypeTree(tpt, whereClauses) =>
        val newTPT = pullOutFunction(tpt, functions, parentFunctions)
        result = result ++ newTPT.dropRight(1)
        result = result ++ List(ExistentialTypeTree(newTPT.last, whereClauses))

      case x =>
        result = result ++ List(x)
    }

    result
  }

  /*
   *  Tree crawler that converts a tree into an appropriate list of statements
   */
  def treeToStatements(tree: Tree, variables: Map[Tree, Type], bootstrap: Boolean, assignTo: List[String]): (List[Statement], Map[Tree, Type]) = {
    var result: List[Statement] = List[Statement]()
    var resultingVariables: Map[Tree, Type] = Map[Tree, Type]()
    resultingVariables = resultingVariables ++ variables

    val tb = currentMirror.mkToolBox()
    tree match {

      // it's an empty tree. So, Nothing happens.
      // therefore, nothing takes place.
      case EmptyTree =>
      //result = EmptyTree


      // package pid { stats }
      /*case PackageDef(pid, stats) => {
        //result = PackageDef(pid, stats)
      }*/

      // mods class name [tparams] impl   where impl = extends parents { defs }
      /*case ClassDef(mods, name, tparams, impl) => {
        //result = ClassDef(mods, name, tparams, impl)
      }*/

      // mods object name impl  where impl = extends parents { defs }
      /*case ModuleDef(mods, name, impl) => {
        //result = ModuleDef(mods, name, impl)
      }*/

      // mods val name: tpt = rhs   
      // This is specifically for when values from payload are being
      // stored as variables
      case a@ValDef(_, TermName(name), tpt, rhs@TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(_)))), TermName("asInstanceOf")), _)) =>
        // variable + LocalState.DELIM + tpt
        //result = ValDef(mods, name, tpt, rhs)
        /*rhs match{
          case x => throw new Exception("Unexpected assignment: " + showCode(rhs))
        }*/
        if (verbose) {
          println("Payload Value")
          //println(showCode(rhs))
          println(showCode(a))
          println()
        }
        try {
          //var statement: List[Tree] = List(a) // program won't finish if using full instruction
          val statement: List[Tree] = List(rhs)

          /*
           * If the Value being defined is part of the bootstrap,
           * then, that value will be defined as a global value
           * in the Agent's local values.
           */
          if (bootstrap) {
            //result = result :+ ReceiveModifyState.apply("global$"+name+"$$"+tpt.toString(), generateFunction(statement))

            // adds new variable to list of known variables
            resultingVariables += (Ident(TermName("global$" + name)) -> generateType(statement))

            // performs tree crawling on rhs of valdef
            val (statements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ ("global$" + name + "$$" + tpt.toString()))
            result = result ++ statements
          }
          else {
            //result = result :+ ReceiveModifyState.apply(name+"$$"+tpt.toString(), generateFunction(statement))

            // adds new variable to list of known variables
            resultingVariables += (Ident(TermName(name)) -> generateType(statement))

            // performs tree crawling on rhs of valdef
            val (statements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ (name + "$$" + tpt.toString()))
            result = result ++ statements
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // mods val name: tpt = rhs
      case a@ValDef(_, TermName(name), tpt, rhs@(Match(_, _) | If(_, _, _) | Block(_, _))) =>
        try {
          if (verbose) {
            println("Non-Payload Value <- match/if/block:")
            println(showCode(a))
            println()
          }

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */
          var statement: List[Tree] = List(rhs)
          val (newStatements, globalReadVariables) = findCodeDependencies(a, variables)
          statement = newStatements ++ statement
          //statement = findCodeDependencies(a, variables) ++ statement

          // determines what the type string of the variable will be
          var variableTypeString: String = null
          if (tpt.isEmpty) {
            variableTypeString = generateType(statement).toString
          }
          else {
            variableTypeString = tpt.toString()
          }

          val rhsCode: List[Tree] = List(tb.parse("Unit.asInstanceOf[" + variableTypeString + "]"))

          // current assignedTo variable isn't included since these rhs's will always return Unit
          if (bootstrap) {
            val nextStatement = ReceiveModifyState.apply("global$" + name + "$$" + variableTypeString, generateFunction(rhsCode))
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply("global$"+name+"$$"+variableTypeString, generateFunction(rhsCode))

            resultingVariables += (Ident(TermName("global$" + name)) -> generateType(statement))
            val (additionalStatements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ ("global$" + name + "$$" + variableTypeString))
            result = result ++ additionalStatements
          }
          else {
            val nextStatement = ReceiveModifyState.apply(name + "$$" + variableTypeString, generateFunction(rhsCode))
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(name+"$$"+variableTypeString, generateFunction(rhsCode))

            resultingVariables += (Ident(TermName(name)) -> generateType(statement))
            val (additionalStatements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ (name + "$$" + variableTypeString))
            result = result ++ additionalStatements
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
          //println(x.getMessage())
        }
      //println()

      // mods val name: tpt = rhs
      case a@ValDef(_, TermName(name), tpt, rhs@TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(_)))), TermName("asInstanceOf")), _)) =>
        try {
          if (verbose) {
            println("Non-Payload Value <- for/while:")
            println(showCode(a))
            println()
          }

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */
          var statement: List[Tree] = List(rhs)
          val (newStatements, globalReadVariables) = findCodeDependencies(a, variables)
          statement = newStatements ++ statement
          //statement = findCodeDependencies(a, variables) ++ statement

          // determines what the type string of the variable will be
          var variableTypeString: String = null
          if (tpt.isEmpty) {
            variableTypeString = generateType(statement).toString
          }
          else {
            variableTypeString = tpt.toString()
          }

          val rhsCode: List[Tree] = List(tb.parse("Unit.asInstanceOf[" + variableTypeString + "]"))

          // current assignedTo variable isn't included since these rhs's will always return Unit
          if (bootstrap) {
            val nextStatement = ReceiveModifyState.apply("global$" + name + "$$" + variableTypeString, generateFunction(rhsCode))
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply("global$"+name+"$$"+variableTypeString, generateFunction(rhsCode))

            resultingVariables += (Ident(TermName("global$" + name)) -> generateType(statement))
            val (additionalStatements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, List[String]())
            result = result ++ additionalStatements
          }
          else {
            val nextStatement = ReceiveModifyState.apply(name + "$$" + variableTypeString, generateFunction(rhsCode))
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(name+"$$"+variableTypeString, generateFunction(rhsCode))

            resultingVariables += (Ident(TermName(name)) -> generateType(statement))
            val (additionalStatements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, List[String]())
            result = result ++ additionalStatements
          }

          // checks to see if there are variables in the assignTo list
          if (assignTo.nonEmpty) {
            // generates rhs function for all elements of the assignTo list
            var newAssign: List[Tree] = List(Ident(TermName(name)))
            val (newStatements, globalReadVariables) = findCodeDependencies(Ident(TermName(name)), resultingVariables)
            newAssign = newStatements ++ newAssign

            //newAssign = findCodeDependencies(Ident(TermName(name)), resultingVariables) ++ newAssign

            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for (currentVariableName <- assignTo.reverse) {
              val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
              nextStatement.readVariables = globalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
          //println(x.getMessage())
        }
      //println()

      // mods val name: tpt = rhs
      case a@ValDef(_, TermName(name), tpt, rhs) =>
        try {
          if (verbose) {
            println("Non-Payload Value")
            println(showCode(a))
            println()
          }

          //var statement: List[Tree] = List(a) // program won't end if full statement is used.
          var statement: List[Tree] = List(rhs)
          if (rhs.equalsStructure(EmptyTree) || rhs.equalsStructure(Literal(Constant(null)))) {
            statement = List(tb.parse("null.asInstanceOf[" + tpt.toString + "]"))
          }

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */
          val (newStatements, _) = findCodeDependencies(a, variables)
          statement = newStatements ++ statement

          //statement = findCodeDependencies(a, variables) ++ statement

          // determines what the type string of the variable will be
          var variableTypeString: String = null
          var variableType: Type = null
          if (tpt.isEmpty) {
            variableType = generateType(statement)
            variableTypeString = variableType.toString
          }
          else {
            variableType = generateType(List(tb.parse("null.asInstanceOf[" + tpt.toString + "]")))
            variableTypeString = variableType.toString
          }

          // generates current valdef statement and adds variable
          if (bootstrap) {
            // adds new variable to list of known variables
            resultingVariables += (Ident(TermName("global$" + name)) -> variableType) //generateType(statement))

            // performs tree crawling on rhs of valdef
            val (statements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ ("global$" + name + "$$" + variableTypeString))
            result = result ++ statements
          }
          else {
            // adds new variable to list of known variables

            //println("generating type")
            resultingVariables += (Ident(TermName(name)) -> variableType) //generateType(statement))
            //println("type generated")

            // performs tree crawling on rhs of valdef
            val (statements, _) = treeToStatements(rhs, resultingVariables, bootstrap = false, assignTo :+ (name + "$$" + variableTypeString))
            result = result ++ statements
          }

          //shouldn't be in here since we already pass this downwards

          // checks to see if there are variables in the assignTo list
          /*if(assignTo.length > 0){
            // generates rhs function for all elements of the assignTo list
            var newAssign: List[Tree] = List(Ident(TermName(name)))
            newAssign = findCodeDependencies(Ident(TermName(name)), resultingVariables) ++ newAssign
            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for(currentVariableName <- assignTo.reverse){
              result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }*/

        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
          //println(x.getMessage())
        }
      //println()

      // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
      // note missing type information is expressed by tpt = TypeTree()
      /*case DefDef(mods, name, tparams, vparams, tpt, rhs) => {
        //result = DefDef(mods, name, tparams, vparams, tpt, rhs)
      }*/

      // mods type name[tparams] = rhs
      // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree, and DEFERRED is set in mods
      /*case TypeDef(mods, name, tparams, rhs) => {
        //result = TypeDef(mods, name, tparams, rhs)
      }*/

      // used for tail calls and like
      // while/do are desugared to label defs as follows:
      // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
      // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
      // While statements always return Unit; so, assignTO isn't handled here.
      case LabelDef(name, _, rhs) =>
        try {
          rhs match {
            case If(cond, thenp: Block, _) =>
              if (verbose) println("While instance start: " + name)

              // generates condition for while loop
              val (newStatement, globalReadVariables) = findCodeDependencies(cond, variables)
              val updatedCondition = newStatement :+ cond

              //val updatedCondition: List[Tree] = findCodeDependencies(cond, variables) :+ cond

              val condition = generateConditionFunction(updatedCondition)

              // variables declared inside of a while statement cannot be global variables
              val (whileStatements, _) = treeToStatements(Block(thenp.stats.dropRight(1), thenp.stats.last), variables, bootstrap = false, List[String]())
              val whileStatement = While.apply(condition)(whileStatements: _*)
              whileStatement.readVariables = globalReadVariables

              result = result :+ whileStatement
              if (verbose) println("While instance end: " + name)
            case Block(stats, If(cond, _, _)) =>
              if (verbose) println("do while instance start: " + name)

              // creates initial boolean variable that determines when the while loop should end
              val newVariableName = "$variable" + uniqueVariableValue
              uniqueVariableValue = uniqueVariableValue + 1
              val initialCondition = tb.parse("var " + newVariableName + "= true")
              val (_, newVariables) = treeToStatements(initialCondition, variables, bootstrap = false, List[String]())

              // generates condition for while loop
              val (newStatement, globalReadVariables) = findCodeDependencies(Ident(newVariableName), newVariables)
              val updatedCondition: List[Tree] = newStatement :+ Ident(newVariableName)

              //val updatedCondition: List[Tree] = findCodeDependencies(Ident(newVariableName), newVariables) :+ Ident(newVariableName)

              val condition = generateConditionFunction(updatedCondition)

              // variables declared inside of a while statement cannot be global variables
              val conditionAssignment = Assign(Ident(newVariableName), cond)
              val (whileStatements, _) = treeToStatements(Block(stats, conditionAssignment), newVariables, bootstrap = false, List[String]())
              val whileStatement = While.apply(condition)(whileStatements: _*)
              whileStatement.readVariables = globalReadVariables

              result = result :+ whileStatement
              if (verbose) println("do while instance end: " + newVariableName)
            case _ =>
              if (verbose) {
                println("Unexpected LabelDef type:")
                println(showRaw(rhs))
              }
              throw new Exception("Unexpected LabelDef type: " + showRaw(rhs))
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
          //println(x.getMessage())
        }

      // import expr.{selectors}
      // Selectors are a list of pairs of names (from, to).
      // The last (and maybe only name) may be a nme.WILDCARD
      // for instance
      //   import qual.{x, y => z, _}  would be represented as
      //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
      /*case Import(expr, selectors) => {
        //result = Import(expr, selectors)
      }*/

      // extends parents { self => body }
      // if self is missing it is represented as emptyValDef
      /*case Template(parents, self, body) => {
        //result = Template(parents, self, body)
      }*/

      // { stats; expr }
      case Block(stats, expr) =>
        //result = Block(stats, expr)
        var currentVariables = variables
        for (currentInstruction <- stats) {

          // a block inside of the boostrap sequence will never create a new global variable
          val (statements, newVariables) = treeToStatements(currentInstruction, currentVariables, bootstrap = false, List[String]())
          result = result ++ statements
          currentVariables = newVariables
        }
        val (statements, newVariables) = treeToStatements(expr, currentVariables, bootstrap = false, assignTo)
        result = result ++ statements
        resultingVariables = newVariables

      // case pat if guard => body
      /*case CaseDef(pat, guard, body) => {
        //result = CaseDef(pat, guard, body)
      }*/

      // pat1 | ... | patn
      /*case Alternative(trees) => {
        //result = Alternative(trees)
      }*/

      // pat*
      /*case Star(elem) => {
        //result = Star(elem)
      }*/

      // name @ pat
      /*case Bind(name, body) => {
        //result = Bind(name, body)
      }*/

      // used for unapply's
      /*case UnApply(fun: Tree, args) => {
        //result = UnApply(fun: Tree, args)
      }*/

      // vparams => body  where vparams:List[ValDef]
      /*case Function(vparams, body) => {
        //result = Function(vparams, body)
      }*/

      //deals with the assigning of payload values
      case a@Assign(Ident(TermName(name)), rhs@TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(_)))), TermName("asInstanceOf")), _)) =>

        if (verbose) {
          println("Payload Value")
          println("Assign")
          println(showCode(a))
          println()
        }

        try {

          // Determines full name of the local variable.
          val fullVariableName = determineFullName(name, variables)

          // performs tree crawling for rhs of assignment
          if (fullVariableName != null) {
            val (statements, _) = treeToStatements(rhs, variables, bootstrap = false, assignTo :+ fullVariableName)
            result = result ++ statements
          }
          else {
            if (verbose) println("Variable: " + name + " was never initialized.\n")
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }
      //println()

      /*
       * Finds instances where the result of a while or for loop is being assigned to
       * a variable. 
       */
      case Assign(Ident(TermName(name)), rhs@(Apply(Select(_, TermName("foreach")), List(Function(List(ValDef(_, _, _, _)), _))) | LabelDef(_, _, _))) =>

        if (verbose) {
          println("Assign <- for/while Instance: ")
          println()
        }

        try {
          var variableType: String = null
          var globalVariable = false

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */

          // Checks if assignment is to a local variable.
          for (currentKey <- resultingVariables.keySet) {
            if (currentKey.equalsStructure(Ident(TermName(name)))) {
              variableType = resultingVariables(currentKey).toString
            }
          }

          // If not a local variable, checks for it as a global variable.
          if (variableType == null) {
            for (currentKey <- resultingVariables.keySet) {
              if (currentKey.equalsStructure(Ident(TermName("global$" + name)))) {
                variableType = resultingVariables(currentKey).toString
                globalVariable = true
              }
            }
          }

          // generates Unit instance for current value
          val unitCode: List[Tree] = List(tb.parse("Unit.asInstanceOf[" + variableType + "]"))

          // current and previous assignedTo variables aren't included since these rhs's will always return Unit
          if (variableType != null) {
            if (globalVariable) {
              val (statements, _) = treeToStatements(rhs, variables, bootstrap = false, List[String]())
              result = result ++ statements
              result = result :+ ReceiveModifyState.apply("global$" + name + "$$" + variableType, generateFunction(unitCode))
            }
            else {
              val (statements, _) = treeToStatements(rhs, variables, bootstrap = false, List[String]())
              result = result ++ statements
              result = result :+ ReceiveModifyState.apply(name + "$$" + variableType, generateFunction(unitCode))
            }

            // checks to see if there are variables in the assignTo list
            if (assignTo.nonEmpty) {
              // generates rhs function for all elements of the assignTo list
              var newAssign: List[Tree] = List(Ident(TermName(name)))
              val (newStatement, globalReadVariables) = findCodeDependencies(Ident(TermName(name)), variables)
              newAssign = newStatement ++ newAssign

              //newAssign = findCodeDependencies(Ident(TermName(name)), variables) ++ newAssign

              val rhsFunction = generateFunction(newAssign)

              // adds additional assign statements for all additional assignTo variables
              for (currentVariableName <- assignTo.reverse) {
                val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
                nextStatement.readVariables = globalReadVariables
                result = result :+ nextStatement

                //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
              }
            }
          }
          else {
            if (verbose) println("Variable: " + name + " was never initialized.\n")
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // lhs = rhs
      case Assign(Ident(TermName(name)), rhs@(Match(_, _) | If(_, _, _) | Block(_, _))) =>

        if (verbose) {
          println("Assign <- Match/If/Block Instance: ")
          println()
        }

        try {

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */

          // Determines full name of the local variable.
          val fullVariableName = determineFullName(name, variables)

          // performs tree crawling for rhs of assignment
          if (fullVariableName != null) {
            val (statements, _) = treeToStatements(rhs, variables, bootstrap = false, assignTo :+ fullVariableName)
            result = result ++ statements
          }
          else {
            if (verbose) println("Variable: " + name + " was never initialized.\n")
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // lhs = rhs
      case a@Assign(Ident(TermName(name)), rhs) =>

        if (verbose) {
          println("Assign Instance: ")
          println(showCode(a))
          println()
        }

        try {
          var statement: List[Tree] = List(rhs)

          /*
           * If a previously declared variable is being used,
           * then the statement will need to make sure to grab
           * said variable from the Agent's localstate.
           */
          val (newStatement, _) = findCodeDependencies(a, variables)
          statement = newStatement ++ statement

          //statement = findCodeDependencies(a, variables) ++ statement

          // determines the full name of the variable
          val fullVariableName = determineFullName(name, variables)

          if (fullVariableName != null) {

            val (statements, _) = treeToStatements(rhs, variables, bootstrap = false, assignTo :+ fullVariableName)
            result = result ++ statements

            /*
             * Initial way assigns were handled
            result = result :+ ReceiveModifyState.apply(fullVariableName, generateFunction(statement))

            // checks to see if there are variables in the assignTo list
            if(assignTo.length > 0){
              // generates rhs function for all elements of the assignTo list
              var newAssign: List[Tree] = List(Ident(TermName(name)))
              newAssign = findCodeDependencies(Ident(TermName(name)), resultingVariables) ++ newAssign
              val rhsFunction = generateFunction(newAssign)

              // adds additional assign statements for all additional assignTo variables
              for(currentVariableName <- assignTo.reverse){
                result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
              }
            }
            */
          }
          else {
            if (verbose) println("Variable: " + name + " was never initialized.\n")
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // lhs = rhs
      /*case Assign(lhs, rhs) => {
        //result = Assign(lhs, rhs)
      }*/

      // lhs = rhs
      /*case AssignOrNamedArg(lhs, rhs) => {
        //result = AssignOrNamedArg(lhs, rhs)
      }*/

      // if (cond) thenp else elsep
      case If(cond, thenp, elsep) =>
        //result = If(cond, thenp, elsep)

        try {
          //Converts tree that produces Boolean into function (a:Agent, m:Message) => Boolean
          val (newStatement, globalReadVariables) = findCodeDependencies(cond, variables)
          val updatedCondition: List[Tree] = newStatement :+ cond

          //val updatedCondition: List[Tree] = findCodeDependencies(cond, variables) :+ cond

          val condition = generateConditionFunction(updatedCondition)

          // Generates If Statement
          if (verbose) println("IF: (" + showCode(cond) + ") {")

          // variables declared inside of an if statement cannot be global variables
          val (thenStatements, _) = treeToStatements(thenp, variables, bootstrap = false, assignTo)
          val ifStatement = StatementIf.apply(condition)(thenStatements: _*)
          ifStatement.readVariables = globalReadVariables

          result = result :+ ifStatement
          if (verbose) println("}\n")

          // Generates Else Statement if one exists
          if (!elsep.equalsStructure(Literal(Constant(())))) {
            if (verbose) println("ELSE: {")

            // variables declared in an else statement cannot be global variables
            val (elseStatements, _) = treeToStatements(elsep, variables, bootstrap = false, assignTo)
            val elseStatement = StatementElse.apply(elseStatements: _*)(ifStatement)
            elseStatement.readVariables = globalReadVariables

            result = result :+ elseStatement
            if (verbose) println("}\n")
          }
          else {
            if (verbose) println("No Else Statement\n")
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }
      //println()

      // selector match { cases }
      case Match(selector, cases) =>
        try {
          if (verbose) println("Match Instance: " + selector)
          //result = Match(selector, cases)

          // creates initial if statement and list of else statements
          var currentIf: StatementIf = null
          var currentElse: StatementElse = null
          var ifStatements = List[Statement]()
          var (elseStatements, _) = treeToStatements(tb.parse("throw new Exception(\"Didn't match any case\")"), variables, bootstrap = false, List[String]())

          // creates the if and else statements for match cases in reverse order.
          for (currentCase <- cases.reverse) {

            if (verbose) {
              println("current case:")
              println(showRaw(currentCase))
              println()
            }

            // creates condition for current if else statement
            val currentCondition = Match(selector, List(CaseDef(currentCase.pat, currentCase.guard, Literal(Constant(true))), CaseDef(Ident(termNames.WILDCARD), EmptyTree, Literal(Constant(false)))))
            val (newStatement, globalReadVariables) = findCodeDependencies(currentCondition, variables)
            val updatedCondition = newStatement :+ currentCondition

            //val updatedCondition = findCodeDependencies(currentCondition, variables) :+ currentCondition

            // debug checking for failure condition...................................................................................................
            /*
            println("Condition Function")
            for (currentItem <- updatedCondition) {
              println(showRaw(currentItem))
              println(showCode(currentItem))
            }
            println()
            */

            val condition = generateConditionFunction(updatedCondition)
            // more debug code..........................................................................................................................
            //println("Condition Generated")
            //println()

            // creates a variables for each binding in pattern matching
            var additionalVariables: Map[Tree, Type] = Map[Tree, Type]() ++ variables
            val allBinds = currentCase.pat.filter(f => f.isInstanceOf[Bind]).asInstanceOf[List[Bind]]
            for (currentBind <- allBinds) {
              val currentMatch: Tree = Match(selector, List(CaseDef(currentCase.pat, currentCase.guard, Ident(currentBind.name))))

              val (moreStatements, moreGlobalVariables) = findCodeDependencies(currentMatch, additionalVariables)
              val statement: List[Tree] = moreStatements :+ currentMatch
              //var statement: List[Tree] = findCodeDependencies(currentMatch, additionalVariables) :+ currentMatch

              val currentVariableType: Type = generateType(statement)

              val nextStatement = ReceiveModifyState.apply(currentBind.name.toString + "$$" + currentVariableType.toString, generateFunction(statement))
              nextStatement.readVariables = moreGlobalVariables
              ifStatements = ifStatements :+ nextStatement

              //ifStatements = ifStatements :+ ReceiveModifyState.apply(currentBind.name.toString()+"$$"+currentVariableType.toString(), generateFunction(statement))

              additionalVariables = additionalVariables + (Ident(currentBind.name) -> currentVariableType)
            }

            //generates the rest of the thenp statements
            val (caseStatements, _) = treeToStatements(currentCase.body, additionalVariables, bootstrap = false, assignTo)
            ifStatements = ifStatements ++ caseStatements

            // creates current if else statement
            currentIf = StatementIf.apply(condition)(ifStatements: _*)
            currentElse = StatementElse.apply(elseStatements: _*)(currentIf)

            currentIf.readVariables = globalReadVariables
            currentElse.readVariables = globalReadVariables

            // resets the else and if statement lists
            elseStatements = List(currentIf)
            ifStatements = List[Statement]()
          }
          /*
          println("initial code:")
          println(showCode(a))
          println()

          println("final code:")
          println(showCode(currentIf))
          println()
          */

          // adds the final if and else statements to the original list
          result = result :+ currentIf
          result = result :+ currentElse

        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      /*
       * return expr
       * The use of Return anywhere but at the end
       * of a function is not allowed; so, return
       * trees are treated simply as the expr they contain.
       */
      case Return(expr) =>

        // checks to see if there are variables in the assignTo list
        if (assignTo.nonEmpty) {
          // generates rhs function for all elements of the assignTo list
          // Return portion of teh tree is ignored
          var newAssign: List[Tree] = List(expr)
          val (newStatement, globalReadVariables) = findCodeDependencies(expr, variables)
          newAssign = newStatement ++ newAssign

          //newAssign = findCodeDependencies(expr, variables) ++ newAssign

          val rhsFunction = generateFunction(newAssign)

          // adds additional assign statements for all additional assignTo variables
          for (currentVariableName <- assignTo.reverse) {
            val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
          }
        }

      // try block catch { catches } finally finalizer where catches: List[CaseDef]
      /*case Try(block, catches, finalizer) => {
        //result = Try(block, catches, finalizer)
      }*/

      // throw expr
      case a@Throw(_) =>
        //result = Throw(expr)
        try {

          if (verbose) {
            println("Throw Instance:")
            println(showCode(a))
            println()
          }

          val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
          val statement: List[Tree] = newStatement :+ a

          //val statement: List[Tree] = findCodeDependencies(a, variables) :+ a

          val throwStatement = Statement.apply(generateStatementFunction(statement))
          throwStatement.readVariables = globalReadVariables
          result = result :+ throwStatement
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // new tpt   always in the context: (new tpt).<init>[targs](args)
      /*case New(tpt) => {
        //result = New(tpt)
      }*/

      // expr: tpt
      /*case Typed(expr, tpt) => {
        //result = Typed(expr, tpt)
      }*/

      // fun[args]
      case a@TypeApply(_, _) =>
        //result = TypeApply(fun, args)

        // checks to see if there are variables in the assignTo list
        if (assignTo.nonEmpty) {
          // generates rhs function for all elements of the assignTo list
          var newAssign: List[Tree] = List(a)
          val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
          newAssign = newStatement ++ newAssign

          //newAssign = findCodeDependencies(a, variables) ++ newAssign

          val rhsFunction = generateFunction(newAssign)

          // adds additional assign statements for all additional assignTo variables
          for (currentVariableName <- assignTo.reverse) {
            val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
          }
        }

      case Apply(Select(iteratorObject, TermName("foreach")), List(Function(List(ValDef(modifier, currentTermName, typeTree, _)), forBlock))) =>
        try {

          if (verbose) println("For Instance:")

          // generates unique variable that will store an Iterator for the while loop
          val newIterator = "$variable" + uniqueVariableValue
          uniqueVariableValue = uniqueVariableValue + 1

          val param = (1 << 13).asInstanceOf[Long]
          //val iterator = ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newIterator), TypeTree(), Select(Apply(Select(iteratorObject, TermName("iterator")), List()))
          //val initialIterator = ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newIterator), TypeTree(), Apply(Select(iteratorObject, TermName("map")), List(Function(List(ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("x"), TypeTree(), EmptyTree)), Ident(TermName("x"))))))
          val initialIterator = Apply(Select(iteratorObject, TermName("map")), List(Function(List(ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("x"), TypeTree(), EmptyTree)), Ident(TermName("x")))))
          val secondIterator = ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newIterator), TypeTree(), Select(Apply(Select(iteratorObject, TermName("map")), List(Function(List(ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("x"), TypeTree(), EmptyTree)), Ident(TermName("x"))))), TermName("iterator")))

          val (newStatement, _) = findCodeDependencies(initialIterator, variables)
          val fullIterator = newStatement :+ initialIterator

          //val fullIterator = findCodeDependencies(initialIterator, variables) :+ initialIterator

          val iteratorType = generateType(fullIterator)

          val exceptableIterator = tb.parse("List[Any]().iterator")
          val exceptableType = generateType(List(exceptableIterator))

          // determines if the original iterable list is already an iterator
          // if so, uses that instead of converting said iterable into an iterator
          var iterator: Tree = null
          if (iteratorType <:< exceptableType) {
            iterator = ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName(newIterator), TypeTree(), Apply(Select(iteratorObject, TermName("map")), List(Function(List(ValDef(Modifiers(param.asInstanceOf[FlagSet]), TermName("x"), TypeTree(), EmptyTree)), Ident(TermName("x"))))))
          }
          else {
            iterator = secondIterator
          }

          // creates statement for iterator and adds to result
          val (iteratorStatements, newVariables) = treeToStatements(iterator, variables, bootstrap = false, List[String]())
          result = result ++ iteratorStatements

          // creates condition for while loop
          val cond = tb.parse(newIterator + ".hasNext")
          val (moreStatement, moreGlobalReadVariables) = findCodeDependencies(cond, newVariables)
          val updatedCondition: List[Tree] = moreStatement :+ cond

          //val updatedCondition: List[Tree] = findCodeDependencies(cond, newVariables) :+ cond

          val condition = generateConditionFunction(updatedCondition)

          // assigns current iterator value to predetermined variable
          val currentIteratorValue = ValDef(modifier, currentTermName, typeTree, Apply(Select(Ident(TermName(newIterator)), TermName("next")), List()))
          val (assignIteratorStatement, moreVariables) = treeToStatements(currentIteratorValue, newVariables, bootstrap = false, List[String]())

          // generates statements from block for For loop

          // Creates the block used in the while loop
          var (forBlockStatements, _) = treeToStatements(forBlock, moreVariables, bootstrap = false, assignTo)
          forBlockStatements = assignIteratorStatement ++ forBlockStatements

          // generates while block instance of for loop
          val whileStatement = While.apply(condition)(forBlockStatements: _*)
          whileStatement.readVariables = moreGlobalReadVariables
          result = result :+ whileStatement
          if (verbose) println()
        }

        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // used for schedule once functions!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      case a@Apply(Select(Select(Select(Ident(TermName("context")), TermName("system")), TermName("scheduler")), _), _) =>

        if (verbose) {
          println("Apply Instance:")
          //println("Variable used: " + showRaw(variable))
          //println("Function Name: " + showRaw(functionName))
          println(showCode(a))
          //println(showRaw(a))
          println("Agent Instance:")
          println()
        }
        if (handleExceptions) {
          println("schedule once functions aren't implemented")
        }
        else {
          throw new Exception("schedule once functions aren't implemented")
        }

      // case to catch applies that use Agent types and an implicit values
      case a@Apply(Apply(Select(variable, functionName), args), _) =>
        try {

          var initialAssign: String = null
          var remainingAssigns: List[String] = List[String]()

          if (assignTo != null && assignTo.nonEmpty) {
            initialAssign = assignTo.last
            remainingAssigns = assignTo.dropRight(1).reverse
          }

          //println("Type Check:")
          val agent = tb.parse("Agent(\"test\")")
          val agentType = generateType(List(agent))

          val (moreStatement, _) = findCodeDependencies(variable, variables)
          val typeOfVariable = generateType(moreStatement :+ variable)

          //val typeOfVariable = generateType(findCodeDependencies(variable, variables) :+ variable)

          //println("typeOfVariable: " + typeOfVariable)
          //println("agentType: " + agentType)

          // Apply is an instance of an Agent Function call
          if (typeOfVariable <:< agentType) {
            if (verbose) println("Agent Instance: ")

            functionName match {

              // agent.ask(x) handler still need to deal with timeout value!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
              case TermName("ask") => //if(args.length == 1) => {

                /*
                println("Ask Arguments with Implicit\n")
                for(currentArg <- args){
                  println(showRaw(currentArg))
                }
                println()
                */

                // creates statements that generates a message.
                val (statements, additionalVariables, messageVariable) = generateMessage(args.head, variables)
                result = result ++ statements

                // determines name of dst agent variable for message.
                var agentVariable: String = null
                variable match {
                  case Ident(TermName(variableName: String)) =>
                    agentVariable = determineFullName(variableName, additionalVariables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, additionalVariables)
                    result = result ++ newStatements
                    agentVariable = variableName
                }

                // determines if a value should be assigned to an AssignTo value
                // or a newly generated variable.
                if (initialAssign == null) {
                  //val applyType = generateType(List(a)) // currently crashing because ? doesn't exist in the agent
                  val apply = tb.parse("null.asInstanceOf[Future[Any]]") //" + applyType + "]")

                  val (newStatements, _, variableName) = generateAssign(apply, additionalVariables)

                  result = result ++ newStatements
                  initialAssign = variableName
                }

                if (verbose) {
                  println("ask Instance:")
                  println("future: " + initialAssign)
                  println("dstAgent: " + agentVariable)
                  println("Message: " + messageVariable)
                  println()
                }

                val nextStatement = Ask.apply(messageVariable, agentVariable, initialAssign)
                nextStatement.readVariables = List(messageVariable, agentVariable)
                result = result :+ nextStatement

                //result = result :+ Ask.apply(messageVariable, agentVariable, initialAssign)

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              case _ =>
                if (verbose) {
                  println("Undefined AST Structure: ")
                  println(showRaw(a))
                  println(showCode(a))
                }
                throw new Exception("Undefined AST Structure: " + showRaw(a))
            }
            //println()
          }

          // Apply isn't an agent.
          else {

            if (verbose) {
              println("Apply Instance:")
              println("Variable used: " + showRaw(variable))
              println("Function Name: " + showRaw(functionName))
              println()
            }

            // converts apply instance into a function
            val (moreStatement, moreGlobalReadVariables) = findCodeDependencies(a, variables)
            val newAssign = moreStatement :+ a

            //val newAssign = findCodeDependencies(a, variables) :+ a

            // determines if apply should be directly assigned to a variable
            if (initialAssign != null) {
              val rhsFunction = generateFunction(newAssign)
              val nextStatement = ReceiveModifyState.apply(initialAssign, rhsFunction)
              nextStatement.readVariables = moreGlobalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(initialAssign, rhsFunction)
            }
            else {
              val rhsFunction = generateStatementFunction(newAssign)
              val nextStatement = Statement.apply(rhsFunction)
              nextStatement.readVariables = moreGlobalReadVariables
              result = result :+ nextStatement

              //result = result :+ Statement.apply(rhsFunction)
            }
          }

          // checks to see if there are any additional variables to assign.
          if (remainingAssigns.nonEmpty) {
            // generates rhs function for all elements of the assignTo list
            val originalInitialAssign = determineOriginalName(initialAssign)
            var newAssign: List[Tree] = List(Ident(TermName(originalInitialAssign)))
            val (newStatement, globalReadVariables) = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables)
            newAssign = newStatement ++ newAssign

            //newAssign = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables) ++ newAssign

            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for (currentVariableName <- remainingAssigns) {
              val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
              nextStatement.readVariables = globalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // case to catch applies that use Agent types
      case a@Apply(Select(variable, functionName), args) =>
        try {

          var initialAssign: String = null
          var remainingAssigns: List[String] = List[String]()

          if (assignTo != null && assignTo.nonEmpty) {
            initialAssign = assignTo.last
            remainingAssigns = assignTo.dropRight(1).reverse
          }

          //println("Type Check:")
          val agent = tb.parse("Agent(\"test\")")
          val agentType = generateType(List(agent))

          val await = tb.parse("Await")
          val awaitType = generateType(List(await))

          val (moreStatement, _) = findCodeDependencies(variable, variables)
          val typeOfVariable = generateType(moreStatement :+ variable)

          //val typeOfVariable = generateType(findCodeDependencies(variable, variables) :+ variable)
          //println("typeOfVariable: " + typeOfVariable)
          //println("agentType: " + agentType)
          //println("awaitType: " + awaitType)

          // Apply is an instance of an Await Function call
          if (typeOfVariable <:< awaitType) {
            if (verbose) println("Await Instance: ")

            functionName match {

              // Await.result instance
              case TermName("result") =>
                if (verbose) println("Await.result instance")

                // generates the future variable used for the await function
                var futureVal: String = ""
                args.head match {
                  case Ident(TermName(variableName: String)) =>
                    futureVal = determineFullName(variableName, variables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, variables)
                    result = result ++ newStatements
                    futureVal = variableName
                }
                if (verbose) println("Future variable: " + futureVal)

                //generates variable containing duration information in milliseconds
                val durationInMillis = Select(args(1), TermName("toMillis"))
                val (newStatements, _, variableName) = generateAssign(durationInMillis, variables)
                result = result ++ newStatements
                val timeoutVar = variableName

                if (verbose) println("duration: " + timeoutVar)


                if (initialAssign != null) {
                  if (verbose) println("Assign future to " + initialAssign)
                  val dstVariableName = initialAssign
                  val nextStatement = TimedGet.apply(futureVal, dstVariableName, timeoutVar)
                  nextStatement.readVariables = List(futureVal, timeoutVar)
                  result = result :+ nextStatement

                  //result =  result :+ TimedGet.apply(futureVal, dstVariableName, timeoutVar)
                }
                else {
                  // generates an initial assign variable that can be used for future assigns.
                  val (typeStatement, _) = findCodeDependencies(a, variables)
                  val assignType = generateType(typeStatement :+ a)

                  //val assignType = generateType(findCodeDependencies(a, variables) :+ a)

                  val apply = tb.parse("null.asInstanceOf[" + assignType.toString + "]") //" + applyType + "]")
                  val (newStatements, moreVariables, initialAssignVariableName) = generateAssign(apply, variables)
                  initialAssign = initialAssignVariableName

                  resultingVariables = resultingVariables ++ moreVariables
                  result = result ++ newStatements

                  // // creates an instance of TimedGet using new initial assign variable
                  val nextStatement = TimedGet.apply(futureVal, initialAssignVariableName, timeoutVar)
                  nextStatement.readVariables = List(futureVal, timeoutVar)
                  result = result :+ nextStatement

                  //result =  result :+ TimedGet.apply(futureVal, initialAssignVariableName, timeoutVar)
                }
                if (verbose) println()
              case x =>
                throw new Error("Unexpected Await instance of " + x)
            }
          }

          // Apply is an instance of an Agent Function call
          else if (typeOfVariable <:< agentType) {
            if (verbose) println("Agent Instance: ")

            functionName match {

              // agent ? x handler
              case TermName("$qmark") =>

                // creates statements that generates a message.
                val (statements, additionalVariables, messageVariable) = generateMessage(args.head, variables)
                result = result ++ statements

                // determines name of dst agent variable for message.
                var agentVariable: String = null
                variable match {
                  case Ident(TermName(variableName: String)) =>
                    agentVariable = determineFullName(variableName, additionalVariables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, additionalVariables)
                    result = result ++ newStatements
                    agentVariable = variableName
                }

                // determines if a value should be assigned to an AssignTo value
                // or a newly generated variable.
                if (initialAssign == null) {
                  //val applyType = generateType(List(a)) // currently crashing because ? doesn't exist in the agent
                  val apply = tb.parse("null.asInstanceOf[Future[Any]]") //" + applyType + "]")

                  val (newStatements, _, variableName) = generateAssign(apply, additionalVariables)

                  result = result ++ newStatements
                  initialAssign = variableName
                }

                if (verbose) {
                  println("qmark Instance:")
                  println("future: " + initialAssign)
                  println("dstAgent: " + agentVariable)
                  println("Message: " + messageVariable)
                  println()
                }

                val nextStatement = Ask.apply(messageVariable, agentVariable, initialAssign)
                nextStatement.readVariables = List(messageVariable, agentVariable)
                result = result :+ nextStatement

                //result = result :+ Ask.apply(messageVariable, agentVariable, initialAssign)

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              // agent.ask(x) handler still need to deal with timeout value!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
              case TermName("ask") => //if(args.length == 1) => {

                /*
                println("Ask Arguments")
                for(currentArg <- args){
                  println(showRaw(currentArg))
                }
                println()
                */

                // creates statements that generates a message.
                val (statements, additionalVariables, messageVariable) = generateMessage(args.head, variables)
                result = result ++ statements

                // determines name of dst agent variable for message.
                var agentVariable: String = null
                variable match {
                  case Ident(TermName(variableName: String)) =>
                    agentVariable = determineFullName(variableName, additionalVariables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, additionalVariables)
                    result = result ++ newStatements
                    agentVariable = variableName
                }

                // determines if a value should be assigned to an AssignTo value
                // or a newly generated variable.
                if (initialAssign == null) {
                  //val applyType = generateType(List(a)) // currently crashing because ? doesn't exist in the agent
                  val apply = tb.parse("null.asInstanceOf[Future[Any]]") //" + applyType + "]")

                  val (newStatements, _, variableName) = generateAssign(apply, additionalVariables)

                  result = result ++ newStatements
                  initialAssign = variableName
                }

                if (verbose) {
                  println("ask Instance:")
                  println("future: " + initialAssign)
                  println("dstAgent: " + agentVariable)
                  println("Message: " + messageVariable)
                  println()
                }

                val nextStatement = Ask.apply(messageVariable, agentVariable, initialAssign)
                nextStatement.readVariables = List(messageVariable, agentVariable)
                result = result :+ nextStatement

                //result = result :+ Ask.apply(messageVariable, agentVariable, initialAssign)

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              // agent ! x handler
              case TermName("$bang") =>

                // creates statements that generates a message.
                val (statements, additionalVariables, messageVariable) = generateMessage(args.head, variables)
                result = result ++ statements

                // determines name of dst agent variable for message.
                var agentVariable: String = null
                variable match {
                  case Ident(TermName(variableName: String)) =>
                    agentVariable = determineFullName(variableName, additionalVariables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, additionalVariables)
                    result = result ++ newStatements
                    resultingVariables = resultingVariables ++ additionalVariables
                    agentVariable = variableName
                }

                if (verbose) {
                  println("bang Instance:")
                  println("dstAgent: " + agentVariable)
                  println("Message: " + messageVariable)
                  println()
                }

                val nextStatement = Send.apply(messageVariable, agentVariable)
                nextStatement.readVariables = List(messageVariable, agentVariable)
                result = result :+ nextStatement

                //result = result :+ Send.apply(messageVariable, agentVariable)

                //println("bang Instance Finished")
                //println()

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              // agent.tell(x)
              case TermName("tell") =>

                var sourceAgentVariable: String = null

                // in the future will handle changing source agent
                if (args.length >= 2) {
                  sourceAgentVariable = determineFullName(args(1).toString(), variables)
                }

                // creates statements that generates a message.
                val (statements, additionalVariables, messageVariable) = generateMessage(args.head, variables)
                result = result ++ statements

                // determines name of dst agent variable for message.
                var agentVariable: String = null
                variable match {
                  case Ident(TermName(variableName: String)) =>
                    agentVariable = determineFullName(variableName, additionalVariables)
                  case x: Tree =>
                    val (newStatements, _, variableName) = generateAssign(x, additionalVariables)
                    result = result ++ newStatements
                    resultingVariables = resultingVariables ++ additionalVariables
                    agentVariable = variableName
                }

                if (verbose) {
                  println("tell Instance:")
                  println("dstAgent: " + agentVariable)
                  println("Message: " + messageVariable)
                }

                if (sourceAgentVariable == null) {
                  val nextStatement = Send.apply(messageVariable, agentVariable)
                  nextStatement.readVariables = List(messageVariable, agentVariable)
                  result = result :+ nextStatement

                  //result = result :+ Send.apply(messageVariable, agentVariable)
                }
                else {
                  if (verbose) println("srcAgent: " + sourceAgentVariable)
                  throw new Exception("Source isn't implemented")
                }
                if (verbose) println()

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              // context.actorFor
              case TermName("actorFor") =>
                if (verbose) println("actorFor arguments: " + showRaw(args))
                throw new Exception("actorFor isn't implemented")

              // context.become
              case TermName("become") =>

                var discardOldVariable: String = null
                var behaviorNameVariable: String = null

                // Determines if the discardOld Value is a variable or other
                if (args.length > 1) {
                  args(1) match {
                    case Ident(TermName(variableName: String)) =>
                      discardOldVariable = determineFullName(variableName, variables)

                    case x: Tree =>
                      val (newStatements, additionalVariables, variableName) = generateAssign(x, variables)
                      result = result ++ newStatements
                      resultingVariables = resultingVariables ++ additionalVariables
                      discardOldVariable = variableName
                  }
                }

                // determines if the behaviorName is a variable or other
                args.head match {
                  case Ident(TermName(variableName: String)) =>
                    //behaviorNameVariable = determineFullName(variableName, variables)
                    behaviorNameVariable = variableName

                  case x: Tree =>
                    val (newStatements, additionalVariables, variableName) = generateAssign(x, variables)
                    result = result ++ newStatements
                    resultingVariables = resultingVariables ++ additionalVariables
                    behaviorNameVariable = variableName
                }

                // determines if the default value should be used for discardOld
                if (discardOldVariable == null) {

                  // creates constant variable that is false.
                  val (newStatements, additionalVariables, variableName) = generateAssign(Literal(Constant(false)), variables)
                  result = result ++ newStatements
                  resultingVariables = resultingVariables ++ additionalVariables
                  discardOldVariable = variableName
                }

                // generates Become statement

                if (verbose) {
                  println("Become Instance:")
                  println("Behaviour: " + behaviorNameVariable)
                  println("discardOld: " + discardOldVariable)
                }
                val nextStatement = Become.apply(args.head.toString(), discardOldVariable)
                nextStatement.readVariables = List(args.head.toString(), discardOldVariable)
                result = result :+ nextStatement

                //result = result :+ Become.apply(args(0).toString(), discardOldVariable)

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              // context.unbecome
              case TermName("unbecome") =>
                if (verbose) println("Unbecome Instance:")
                result = result :+ UnBecome.apply

                // determines if a value should be assigned to an AssignTo value.
                if (initialAssign != null) {
                  val function = List[Tree](tb.parse("Unit"))
                  result = result :+ ReceiveModifyState.apply(initialAssign, generateFunction(function))
                }

              case x =>
                if (verbose) {
                  println("Undefined AST Structure: ")
                  println(showRaw(a))
                  println(showCode(a))
                }
                throw new Exception("Undefined AST Structure: " + x.toString)
            }
            if (verbose) println()
          }

          // Apply isn't an agent.
          else {

            if (verbose) {
              println("Apply Instance:")
              println("Variable used: " + showRaw(variable))
              println("Function Name: " + showRaw(functionName))
              println()
            }

            // converts apply instance into a function
            val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
            val newAssign = newStatement :+ a

            //val newAssign = findCodeDependencies(a, variables) :+ a

            // determines if apply should be directly assigned to a variable
            if (initialAssign != null) {
              val rhsFunction = generateFunction(newAssign)
              val nextStatement = ReceiveModifyState.apply(initialAssign, rhsFunction)
              nextStatement.readVariables = globalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(initialAssign, rhsFunction)
            }
            else {
              val rhsFunction = generateStatementFunction(newAssign)
              val nextStatement = Statement.apply(rhsFunction)
              nextStatement.readVariables = globalReadVariables
              result = result :+ nextStatement

              //result = result :+ Statement.apply(rhsFunction)
            }
          }

          // checks to see if there are any additional variables to assign.
          if (remainingAssigns.nonEmpty) {
            // generates rhs function for all elements of the assignTo list
            val originalInitialAssign = determineOriginalName(initialAssign)
            var newAssign: List[Tree] = List(Ident(TermName(originalInitialAssign)))
            val (moreStatement, moreGlobalReadVariables) = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables)
            newAssign = moreStatement ++ newAssign

            //newAssign = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables) ++ newAssign

            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for (currentVariableName <- remainingAssigns) {
              val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
              nextStatement.readVariables = moreGlobalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // Default Apply handler
      case a@Apply(_, _) =>
        try {

          if (verbose) {
            println("Apply Instance:")
            println(showRaw(a))
            println(showCode(a))
            println()
          }

          var initialAssign: String = null
          var remainingAssigns: List[String] = List[String]()

          if (assignTo != null && assignTo.nonEmpty) {
            initialAssign = assignTo.last
            remainingAssigns = assignTo.dropRight(1).reverse
          }

          // converts apply instance into a function
          val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
          val newAssign = newStatement :+ a

          //val newAssign = findCodeDependencies(a, variables) :+ a

          // determines if apply should be directly assigned to a variable
          if (initialAssign != null) {
            val rhsFunction = generateFunction(newAssign)
            val nextStatement = ReceiveModifyState.apply(initialAssign, rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(initialAssign, rhsFunction)
          }
          else {
            val rhsFunction = generateStatementFunction(newAssign)
            val nextStatement = Statement.apply(rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ Statement.apply(rhsFunction)
          }

          // checks to see if there are any additional variables to assign.
          if (remainingAssigns.nonEmpty) {
            // generates rhs function for all elements of the assignTo list
            val originalInitialAssign = determineOriginalName(initialAssign)
            var newAssign: List[Tree] = List(Ident(TermName(originalInitialAssign)))
            val (moreStatement, moreGlobalReadVariables) = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables)
            newAssign = moreStatement ++ newAssign

            //newAssign = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables) ++ newAssign

            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for (currentVariableName <- remainingAssigns) {
              val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
              nextStatement.readVariables = moreGlobalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // qual.super[mix]     if qual and/or mix is empty, they are nme.EMPTY.toTypeName
      /*case Super(qual, mix) => {
        //result = Super(qual, mix)
      }*/

      // qual.this
      /*case This(qual) => {
        //result = This(qual)
      }*/

      // qualifier.selector
      case a@Select(_, _) =>
        //result = Select(qualifier, selector)

        try {

          if (verbose) {
            println("Select Instance:")
            println(showRaw(a))
            println(showCode(a))
            println()
          }

          var initialAssign: String = null
          var remainingAssigns: List[String] = List[String]()

          if (assignTo != null && assignTo.nonEmpty) {
            initialAssign = assignTo.last
            remainingAssigns = assignTo.dropRight(1).reverse
          }

          // converts apply instance into a function
          val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
          val newAssign = newStatement :+ a

          //val newAssign = findCodeDependencies(a, variables) :+ a

          // determines if apply should be directly assigned to a variable
          if (initialAssign != null) {
            val rhsFunction = generateFunction(newAssign)
            val nextStatement = ReceiveModifyState.apply(initialAssign, rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(initialAssign, rhsFunction)
          }
          else {
            val rhsFunction = generateStatementFunction(newAssign)
            val nextStatement = Statement.apply(rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ Statement.apply(rhsFunction)
          }

          // checks to see if there are any additional variables to assign.
          if (remainingAssigns.nonEmpty) {
            // generates rhs function for all elements of the assignTo list
            val originalInitialAssign = determineOriginalName(initialAssign)
            var newAssign: List[Tree] = List(Ident(TermName(originalInitialAssign)))
            val (moreStatement, moreGlobalReadVariables) = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables)
            newAssign = moreStatement ++ newAssign

            //newAssign = findCodeDependencies(Ident(TermName(originalInitialAssign)), variables) ++ newAssign

            val rhsFunction = generateFunction(newAssign)

            // adds additional assign statements for all additional assignTo variables
            for (currentVariableName <- remainingAssigns) {
              val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
              nextStatement.readVariables = moreGlobalReadVariables
              result = result :+ nextStatement

              //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
            }
          }
        }
        catch {
          case x: Throwable if handleExceptions =>
            println("something happened")
            val sw = new StringWriter
            x.printStackTrace(new PrintWriter(sw))
            println(sw.toString)
            println()
        }

      // name
      // note: type checker converts idents that refer to enclosing fields or methods
      // to selects; name ==> this.name
      case a@Ident(_) =>

        // checks to see if there are variables in the assignTo list
        if (assignTo.nonEmpty) {
          // generates rhs function for all elements of the assignTo list
          var newAssign: List[Tree] = List(a)
          val (newStatement, globalReadVariables) = findCodeDependencies(a, variables)
          newAssign = newStatement ++ newAssign

          //newAssign = findCodeDependencies(a, variables) ++ newAssign
          val rhsFunction = generateFunction(newAssign)

          // adds additional assign statements for all additional assignTo variables
          for (currentVariableName <- assignTo.reverse) {
            val nextStatement = ReceiveModifyState.apply(currentVariableName, rhsFunction)
            nextStatement.readVariables = globalReadVariables
            result = result :+ nextStatement

            //result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
          }
        }

      // value
      case a@Literal(_) =>

        // checks to see if there are variables in the assignTo list
        if (assignTo.nonEmpty) {

          /*
           *  generates rhs function for all elements of the assignTo list
           * a literal cannot have nested variables so a search for any
           * is unnecessary
           */
          val newAssign: List[Tree] = List(a)
          val rhsFunction = generateFunction(newAssign)

          // adds additional assign statements for all additional assignTo variables
          for (currentVariableName <- assignTo.reverse) {
            result = result :+ ReceiveModifyState.apply(currentVariableName, rhsFunction)
          }
        }

      // a type that's not written out, but given in the tpe attribute
      /*case TypeTree() => {
        //result = TypeTree()
      }*/

      // arg @annot  for types,  arg: @annot for exprs
      /*case Annotated(annot, arg) => {
        //result = Annotated(annot, arg)
      }*/

      // ref.type
      /*case SingletonTypeTree(ref) => {
        //result = SingletonTypeTree(ref)
      }*/

      // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
      /*case SelectFromTypeTree(qualifier, selector) => {
        //result = SelectFromTypeTree(qualifier, selector)
      }*/

      // parent1 with ... with parentN { refinement }
      /*case CompoundTypeTree(templ: Template) => {
        //result = CompoundTypeTree(templ: Template)
      }*/

      // tpt[args]
      /*case AppliedTypeTree(tpt, args) => {
        //result = AppliedTypeTree(tpt, args)
      }*/

      // >: lo <: hi
      /*case TypeBoundsTree(lo, hi) => {
        //result = TypeBoundsTree(lo, hi)
      }*/

      // tpt forSome { whereClauses }
      /*case ExistentialTypeTree(tpt, whereClauses) => {
        //result = ExistentialTypeTree(tpt, whereClauses)
      }*/

      case x =>
        if (handleExceptions) {
          println("Undefined AST Structure: " + showRaw(x))
          if (verbose) {
            println(showCode(x))
            println()
          }
        }
        else {
          throw new Exception("Undefined AST Structure: " + showRaw(x))
        }
    }

    (result, resultingVariables)
  }

  /*
   * creates and stores a list of agents into
   * a directory. Returns true if this was successful
   */
  def saveAgents(directory: String, agents: List[Agent]): Boolean = {

    val agentFolder: File = new File(directory)

    // Checks if the directory location already exists
    if (!agentFolder.exists()) {

      // creates a folder in the current location
      val createdDir = agentFolder.mkdir()
      if (!createdDir) {
        println("Couldn't create the necessary directory.")
        return false
      }
    }

    // makes sure that the directory path
    // is a folder
    if (agentFolder.isDirectory) {

      // starts adding each agent into the folder
      // in the form of a file.

      var storedAllAgents = true
      for (currentAgent <- agents) {
        val currentAgentName = currentAgent.name
        val currentAgentFileName = agentFolder.getAbsolutePath + File.pathSeparator + currentAgentName + ".agent"
        val currentAgentFile = new File(currentAgentFileName)
        currentAgentFile.delete()
        storedAllAgents = storedAllAgents || currentAgentFile.createNewFile()
        IO.saveObjToFile(currentAgent, currentAgentFile)
      }

      if (storedAllAgents) {
        println("Current Distributed System successfully saved.")
        return true
      }

    }

    println(directory + " isn't a Directory")
    false
  }

  /*
   * returns a list of agents that are stored in
   * a directory. returns null if unsuccessful.
   */
  def loadAgents(directory: String): List[Agent] = {

    // checks if the current path exists and is directory
    val agentFolder = new File(directory)
    if (agentFolder.exists() && agentFolder.isDirectory) {

      var agents: List[Agent] = List[Agent]()

      // looks at each agent file and attempts to convert
      // them back into instances of agents.
      for (currentAgentFile <- agentFolder.listFiles().filter(f => f.isFile && f.getName.endsWith(".agent"))) {
        val possibleAgent = IO.readObjFromFile(currentAgentFile)

        possibleAgent match {
          case Some(agent: Agent) =>
            agents = agents :+ agent
          case None =>
            println(currentAgentFile.getAbsolutePath + " not an instance of an Agent.")
            return null
          case _ => throw new Error("Strange case encountered here!")
        }
      }
      return agents
    }

    println(directory + " is an invalid path.")
    null
  }

  /*
   * Takes a list of agents and converts them into a DS2 Distributed System.
   * Continues to allow the user to perform tests using DS2.
   */
  def handleDS2Instances(agents: List[Agent], skipTesting: Boolean): Unit = {

    val distributedSystem = new DistributedSystem("ds")
    for (currentAgent <- agents) {
      distributedSystem + currentAgent
    }

    /*
     * Once the distributed system is finished being generated,
     * the frontend will then enter a continuous loop where
     * the user can use ds2 to debug their distributed system
     * in a variety of ways.
     */
    println("Distributed system has been successfully generated.")
    println("Proceed to debug the distributed system using the command line.")
    var continue = true

    /*
     * Continuously loop through user input unless told
     * beforehand or in the loop to quit. 
     */
    while (continue && !skipTesting) {
      val ln = readLine()
      val arguments = ln.split(" ")

      /*
       * Determines what type of scheduler to run best on
       * the first word in the argument. 
       */
      arguments.head match {
        case "linearizability-scheduler" =>
        //val scheduler = new LinearizabilityScheduler(schedulerArguments)
        //scheduler.attach(distributedSystem)

        case "basic-scheduler" =>
          val scheduler = new BasicScheduler()
          scheduler.attach(distributedSystem)

        case "help" =>
          println("Non-useful help message")

        case "quit" =>
          continue = false
        case _ =>
          println("Invalid argument. Type \"help\" for a list of valid commands.")
      }
      println()
    }
  }

  override def main(args: Array[String]) {
    val options: Options = new Options()
    options.addOption("parse", true, "Has the program parse the classes in the given folder.")
    options.addOption("verbose", false, "Verbosely print out information about the Distributed System being generated.")
    options.addOption("ignoreExceptions", false, "Ignores errors that are thrown during Distributed System generation.")
    options.addOption("quit", false, "Immediately ends program after generation of Distributed System.")
    options.addOption("save", true, "Saves the parsed Distributed System in the requested folder.")
    options.addOption("load", true, "Loads a previously saved Distributed System from the requested folder")

    val parser: CommandLineParser = new DefaultParser()
    val cmd: CommandLine = parser.parse(options, args)
    val tb = currentMirror.mkToolBox()

    val pluginLocation = new File(FrontEnd.getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
    val skipTesting = cmd.hasOption("quit")
    verbose = cmd.hasOption("verbose")
    handleExceptions = cmd.hasOption("ignoreExceptions")
    val save = cmd.hasOption("save")
    val saveArg = cmd.getOptionValue("parse")
    if (save && saveArg == null) {
      println("save argument isn't valid.")
      System.exit(1)
    }

    println("Current Location:" + pluginLocation.getCanonicalPath)

    // handles the load option
    if (cmd.hasOption("load")) {
      val arg = cmd.getOptionValue("parse")

      // checks if the load argument isn't valid
      if (arg == null) {
        println("Invalid argument received")
        System.exit(1)
      }

      val agents = loadAgents(arg)
      handleDS2Instances(agents, skipTesting)
    }

    // handles the parse option
    else if (cmd.hasOption("parse")) {
      val arg = cmd.getOptionValue("parse")
      uniqueVariableValue = 0
      if (arg == null) {
        println("No argument received for parse")
        println(cmd.getOptionValue("parse"))
        System.exit(1)
      } else {
        /*
         * Verifies that the path we were given
         * exists and is a file
         */
        val rootFolder: File = new File(arg)
        if (rootFolder.exists() && rootFolder.isDirectory) {

          println("Attempting to rebuild selected project.")


          // Reason: look at "assembly" file path, it is correct in Windows, not in *nix

          /*
           * Creates an additional sbt file in the selected project
           * that adds the assembly function we use for creating
           * Fat jars.
           */
          val assembly: File = new File(rootFolder.getAbsolutePath + s"${File.separator}project${File.separator}assembly.sbt")
          assembly.createNewFile()
          val writer = new PrintWriter(assembly)

          writer.write("""addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")""")
          writer.close()

//          // DEBUG
//          println(rootFolder)
          Process("pwd",rootFolder).!
          Process("ls -l",rootFolder).!

          /*
           * Runs sbt on the sbt project path given.
           * The way the external process is called is
           * dependent on if it is a windows operating system
           * or not.
           */
          var result = 1
          if (System.getProperty("os.name").toLowerCase().contains("windows"))
            result = Process("cmd.exe /C sbt clean compile assembly", rootFolder).!
          else
            result = Process("sbt clean compile assembly", rootFolder).!

          /*
         * Once finished running the external process,
         * the assembly file we created earlier is
         * removed and the external process is checked
         * for any exiting errors.
         */

          assembly.delete()

          if (result != 0) {
            println("Error: Unable to rebuild project.")
            System.exit(result)
//            return
          }

          // Reason: look at "target" file path, it is correct in Windows, not in *nix
          /*
           * After the generation of the FAT Jar,
           * determines the location of the jar
           * so that it can be added to the classpath.
           */
          val target = new File(rootFolder.getAbsolutePath + s"${File.separator}target${File.separator}")

          // finds the scala folder that is
          // being used in the current sbt project.
          val scalaName = target.list().filter { x => x.startsWith("scala") }.head
          val scala = new File(target.getAbsolutePath + File.separator + scalaName + File.separator)

          // finds the jar file that is
          // in the current sbt project
          val jar = scala.listFiles().filter { x => x.isFile && x.getName.endsWith(".jar") }.head

          //addPath(rootFolder.getAbsolutePath + "\\target\\scala-2.10\\Akka-ZAB-assembly-0.2.jar")
          // Adds jar to the classpath
          println("Jar path location:")
          println(jar.getAbsolutePath)
          addPath(jar.getAbsolutePath)

          println(rootFolder.toString)
          println("ClassPaths:")
          val cl = ClassLoader.getSystemClassLoader
//          cl.asInstanceOf[java.net.URLClassLoader].getURLs.foreach(println)
          new URLClassLoader(Array(),cl).getURLs.foreach(println)

          val scalaFolder: File = new File(rootFolder.getAbsolutePath, s"src${File.separator}main${File.separator}scala${File.separator}")

          val scalaFiles = recursiveListFiles(scalaFolder)

          var agents: Map[String, Agent] = Map[String, Agent]()

          for (currentFile <- scalaFiles) {
            if (currentFile.isFile) {
              println("Parsing File: " + currentFile.getAbsolutePath)
              imports = List[Tree]()

              /*
               * Since all instances of Akka Actors are being replaced with
               * DS2 Agents, it is necessary that we import ds2 instances of
               * Agents so that way the types of future variables can be
               * properly determined.
               */
              imports = imports :+ tb.parse("import edu.utah.cs.gauss.ds2.core.ir.datastructures.Agent")
              imports = imports :+ tb.parse("import edu.utah.cs.gauss.ds2.core.ir.datastructures.Message")

              /*
               * Determines the folder location for the current file
               * and then imports its entire directory. This is necessary
               * because all classes in the same directory of a file is
               * by default imported in the original program; however,
               * the frontend isn't running in the classes original
               * directory. Therefore, these classes must be imported
               * to guarantee that the frontend doesn't fail.
               */
              var importLocation: String = currentFile.getAbsolutePath.substring((rootFolder.getAbsolutePath + s"src${File.separator}").length() + 1, currentFile.getAbsolutePath.length() - currentFile.getName.length())
              importLocation = importLocation.replace(File.separatorChar, '.')
              importLocation = "import " + importLocation + "_"
              imports = imports :+ tb.parse(importLocation)

              val src = Source.fromFile(currentFile)
              var data = src.mkString
              data = data.replaceAll("package", s"${File.separator}package")
              val tree = tb.parse(data)
              src.close

              /*
              val test = "var test:Int = null.asInstanceOf[Int]"
              val testString = tb.parse(test)
              println(testString)
              println(showRaw(testString))
              */

              for (currentTree <- tree.children) {
                currentTree match {
                  /*
                   * Finds all imports used in the current file
                   * and creates a list of them that can be used
                   * in future ds2 statements so that all requirements
                   * are met.
                   * 
                   * Akka based imports are removed since they
                   * are replaced by the ds2 objects
                   */
                  case x@Import(_, _) =>
                    if (verbose) println("Import: " + showCode(x))
                    //println(showCode(x))
                    //println(showRaw(x))
                    if (true) { //!x.exists { x => x.equalsStructure(Ident(TermName("akka"))) }){
                      imports = imports :+ x
                    }

                  /*
                   * Includes all instances of case classes in the current file
                   * into the list of imports attached to each snippet of code.
                   */
                  case x@ClassDef(Modifiers(CASE, _, _), _, _, _) =>
                    if (verbose) println("Case Class: " + showCode(x))
                    //println(showCode(x))
                    //println("Modifier details")
                    //println(showRaw(modName))
                    //println(showRaw(modTrees))
                    //println()
                    imports = imports :+ akkaToAgentSwap(x)

                  /*
                   * Finds each instance of a Class in the file
                   * and checks if it is an Akka.Actor inheriting
                   * class.
                   */
                  case ClassDef(_, name, _, impl) =>

                    var correctExtension: Boolean = false
                    for (x <- impl.parents) {
                      if (x.toString().endsWith("Actor")) {
                        correctExtension = true
                      }
                    }

                    /*
                   * if the class is determined to inherit from akka.actor.Actor
                   * then the plugin will proceed to generate a DS2 Agent for that
                   * class.
                   */
                    if (correctExtension) {
                      println("Class " + name.toString)

                      if (verbose) {
                        println("Imports used")
                        for (currentImport <- imports) {
                          println(showCode(currentImport))
                        }
                        println()
                      }
                      val agent: Agent = new Agent(name.toString)
                      var start: List[Tree] = List[Tree]()

                      // sets vals self and context to be instances of the current agent
                      // as global variables.
                      start = start :+ tb.parse("val self = a")
                      start = start :+ tb.parse("val context = a")

                      var behaviors: Map[String, Map[String, List[Tree]]] = Map[String, Map[String, List[Tree]]]()
                      var functions: Map[String, DefDef] = Map[String, DefDef]()
                      for (currentNode <- impl.body) {

                        // replaces all instances of ActorRef or ActorSelect types with Agent types
                        val modifiedNode = akkaToAgentSwap(currentNode)

                        // Determines what should be done with the current
                        // instruction in the class.
                        modifiedNode match {
                          case DefDef(_, TermName("receive"), _, _, _, Match(_, cases)) =>
                            val actionTable = generateMessageActionPairs(cases)
                            behaviors += ("default" -> actionTable)
                          case DefDef(_, TermName(name), _, _, Ident(TypeName("Receive")), Match(_, cases)) =>
                            val actionTable = generateMessageActionPairs(cases)
                            behaviors += (name -> actionTable)

                          // def used by the class to implement additional arguments
                          case DefDef(_, termNames.CONSTRUCTOR, _, vparams, _, _) =>
                            var payloadValue = 0
                            for (currentVparams <- vparams.head) {
                              start = start :+ Assign(Ident(currentVparams.name), TypeApply(Select(Apply(Select(Ident(TermName("m")), TermName("payload")), List(Literal(Constant(payloadValue)))), TermName("asInstanceOf")), List(currentVparams.tpt)))
                              payloadValue = payloadValue + 1
                            }
                          case tree@DefDef(_, TermName(name), _, _, _, _) =>
                            functions += (name -> tree)
                          case x =>
                            start = start :+ x
                        }
                      }
                      /*println("Initial Start")
                      for(currentInstruction <- start){
                        println(showRaw(currentInstruction))
                      }
                      println()*/
                      /*
                       * Since the action $start contains all of
                       * the code that would usually run when
                       * initializing the Akka Actor, this makes
                       * sure that the default behavior contains
                       * the mapping to the $start action or creates
                       * a default behavior that does.
                       */
                      if (behaviors.contains("default")) {
                        behaviors = behaviors + ("default" -> (behaviors("default") + ("$start" -> start)))
                      } else {
                        behaviors = behaviors + ("default" -> Map("$start" -> start))
                      }

                      /*
                       * Goes through all of the actions and inlines the function calls.
                       */
                      for (currentBehaviorName <- behaviors.keys) {
                        val cases = behaviors(currentBehaviorName)
                        for (currentCaseName <- cases.keys) {
                          val currentAction = cases(currentCaseName)
                          val inlinedAction = inlineFunctions(currentAction, functions)

                          behaviors = behaviors + (currentBehaviorName -> (behaviors(currentBehaviorName) + (currentCaseName -> inlinedAction)))
                        }
                      }

                      /*
                       * generates the reaction for the start sequence.
                       */
                      var globalVariables: Map[Tree, Type] = Map[Tree, Type]()
                      var bootstrapReaction: (Message, Action) = null
                      if (behaviors.contains("default")) {
                        val cases = behaviors("default")
                        if (cases.contains("$start")) {
                          val currentCaseName = cases("$start")
                          var reaction = new Action
                          println(currentCaseName)
                          val currentAction = cases("$start")
                          var variables: Map[Tree, Type] = Map[Tree, Type]()

                          for (currentInstruction <- currentAction) {
                            /*
                             * looks at each expression in the current
                             * behavior case and converts it into the
                             * appropriate statement that will then
                             * be added to reaction.
                             */
                            try {

                              // variables declared directly in the $start action can be global variables
                              val (statements, newVariables) = treeToStatements(currentInstruction, variables, bootstrap = true, List[String]())
                              reaction = reaction ++ statements
                              variables = newVariables
                            }
                            catch {
                              case x: Throwable =>
                                //println("handleExceptions = " + handleExceptions)
                                println("Exception caught in following instruction: ")
                                println(currentInstruction)
                                val sw = new StringWriter
                                x.printStackTrace(new PrintWriter(sw))
                                println(sw.toString)
                                println()

                                //println(x.getMessage())
                                //throw new Exception("Exception caught in the following instruction:\n" + currentInstruction)
                                return
                            }
                            //println(showRaw(currentInstruction))
                            //println(currentInstruction)
                          }
                          globalVariables = variables

                          /*
                           * Maps the current reaction to the message it is
                           * connected to and then places it in the current
                           * behavior.
                           */

                          val messageReactions = (new Message(currentCaseName), reaction)
                          //println(messageReactions)
                          bootstrapReaction = messageReactions
                          //println()
                          println()
                        }
                        else {
                          println("Start event in default behavior doesn't exist.")
                        }
                      }
                      else {
                        println("Default behavior does not exist.")
                      }




                      /*
                       * Looks at all behaviors created during parsing
                       */
                      try {
                        for (currentBehaviorName <- behaviors.keys) {
                          var currentBehavior = new Behavior(currentBehaviorName)
                          println("Behavior: " + currentBehaviorName)
                          val cases = behaviors(currentBehaviorName)

                          /*
                           * Looks at each case inside of the current behavior
                           */
                          for (currentCaseName <- cases.keys) {

                            /*
                             * Generates actions for all cases
                             * that aren't the initial start
                             * case.
                             */
                            if (!currentCaseName.equals("$start")) {
                              var reaction = new Action
                              println(currentCaseName)
                              val currentAction = cases(currentCaseName)
                              var variables: Map[Tree, Type] = globalVariables

                              for (currentInstruction <- currentAction) {
                                /*
                                 * looks at each expression in the current
                                 * behavior case and converts it into the
                                 * appropriate statement that will then
                                 * be added to reaction.
                                 */
                                try {
                                  //println(showCode(currentInstruction))
                                  val (statements, newVariables) = treeToStatements(currentInstruction, variables, bootstrap = false, List[String]())
                                  reaction = reaction ++ statements
                                  variables = newVariables
                                }
                                catch {
                                  case x: Throwable =>
                                    //println("handleExceptions = " + handleExceptions)
                                    println("Exception caught in following instruction: ")
                                    println(currentInstruction)
                                    val sw = new StringWriter
                                    x.printStackTrace(new PrintWriter(sw))
                                    println(sw.toString)
                                    println()

                                    return
                                  //throw new Exception("Exception caught in the following instruction:\n" + currentInstruction)
                                }
                                //println(showRaw(currentInstruction))
                                //println(currentInstruction)
                              }

                              /*
                               * Maps the current reaction to the message it is
                               * connected to and then places it in the current
                               * behavior.
                               */

                              val messageReactions = (new Message(currentCaseName), reaction)
                              currentBehavior += messageReactions
                              println()
                            }
                            else {
                              currentBehavior += bootstrapReaction
                            }
                          }

                          /*
                           * Adds the current behavior to the
                           * current agent.
                           */
                          agent.behaviors = agent.behaviors + ((currentBehaviorName, currentBehavior))
                          if (currentBehaviorName.contains("default")) {
                            agent.defaultBehavior = currentBehavior
                            agent.reactions = agent.defaultBehavior
                          }
                          println()
                        }
                      }
                      catch {
                        case x: Throwable =>
                          println("something happened")
                          val sw = new StringWriter
                          x.printStackTrace(new PrintWriter(sw))
                          println(sw.toString)
                          println()
                          return
                      }

                      /*
                       * Adds the finished Agent to the list
                       * of other possible agents generated from
                       * this akka system.
                       */
                      agents += (agent.name -> agent)
                      println(agent)
                    }
                  case x if verbose =>
                      println("Unhandled Tree in File:")
                      println(showRaw(x))
                      println(showCode(x))
                }
              }
            }
          }

          /*
           * After generating all of the agents that will
           * be used in the distributed system, we now
           * create the distributed system using the topology
           * that the user gave to the frontend in the form
           * of an argument.
           */

          var agentsList = List[Agent]()
          for (currentAgent <- agents.keys)
            agentsList = agentsList :+ agents(currentAgent)

          // Saves the agents in a folder if requested
          if (save) saveAgents(saveArg, agentsList)

          handleDS2Instances(agentsList, skipTesting)

        }
        else println("Parsing path doesn't point to a directory.")
      }
    }
  }

}

/*
 * a fake agent class that is used strictly to determine the proper 
 * type of functions that are used by regular agents
 */
class FakeAgent() {


  // actorRef functions that need to be covered
  def isTerminated: Boolean = null.asInstanceOf[Boolean]

  def path: String = null.asInstanceOf[String]

  def compareTo(other: Agent): Int = null.asInstanceOf[Int]

  def forward(message: Any): Unit = Unit

  def tell(msg: Any, sender: Agent): Unit = Unit

  def tell(msg: Any): Unit = Unit

  def !(msg: Any): Unit = Unit

  // extension of functions that cover actorContext
  def actorOf(props: Any, name: String): Agent = null.asInstanceOf[Agent] // replaced instance of Prop with Any

  def actorOf(props: Any): Agent = null.asInstanceOf[Agent] // replaced instance of Prop with Any

  def become(behavior: Any, discardOld: Boolean = true): Unit = Unit // replaced instance of Receive with Any

  def children: Iterable[Agent] = null.asInstanceOf[Iterable[Agent]]

  def parent: Agent = null.asInstanceOf[Agent]

  def props: Any = null // replaced instance of Prop with Any

  def self: Agent = null.asInstanceOf[Agent]

  def sender: Agent = null.asInstanceOf[Agent]

  def unbecome(): Unit = Unit

  def actorFor(path: Any): Agent = null.asInstanceOf[Agent]

  def actorSelection(path: String): Agent = null.asInstanceOf[Agent]

  // ask extension functions to cover
  def ask(actor: Agent, message: Any)(implicit timeout: Any): Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any

  def ask(message: Any)(implicit timeout: Any): Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any

  //def ask(actor: Agent, message: Any, timeout: Any) : Future[Any] = null.asInstanceOf[Future[Any]] // replaced instance of Timeout with Any

  def ask(actor: Agent, message: Any, timeoutMillis: Long): Future[Any] = null.asInstanceOf[Future[Any]]

  //def ask(message: Any) : Future[Any] = null.asInstanceOf[Future[Any]]

  def ?(message: Any): Future[Any] = null.asInstanceOf[Future[Any]]

  //val result = new Agent("test").asInstanceOf[FakeAgent].ask(new Agent("test"), "message")("timeout")

}

class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream) extends ObjectInputStream(fileInputStream) {
  override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
    try {
      Class.forName(desc.getName, false, getClass.getClassLoader)
    }
    catch {
      case _: ClassNotFoundException => super.resolveClass(desc)
    }
  }
}
