import scala.meta._

val tree = q"a + b"

println("Struncture: "+tree.structure)

println("Syntax: " + tree.syntax)

q"val x = 2".transform { case q"2" => q"42" }

val transformer = new Transformer {
  override def apply(tree: Tree): Tree = tree match {
    case name @ Term.Name("b") => q"function($name)"
    case node => super.apply(node)
  }
}
transformer(q"a + b")

q""""hello duuuude""""

q"this"
q"super"

val q"import ..$selection" = q"import edu.utah.cs.gauss.ds2.core"

val q"{..$stats}" = q"println(1); return 0"

val q"(..$params) => $expr" = q"(m: Message, a: Agent) => true"

val q"{ ..case $casesnel }" =
q"""{
     case 1 if true => println(1)
     case 2 => 'yay
     case _ => "whatever"
   }"""


val q"..$mods class $name[..$tparams](..$params) extends ..$pars {..$body}" =
q"class Mo[T](id: T = null) extends Serializable {println(1); return 'yay}"

"package a".parse[Source].get.structure

q"val a: Int"

t"Int with String"


t"Int"
q"name"
//p"`name`"
p"name"

"_: A | _: B".parse[Pat].get.structure
"import a.b".parse[Source].get.structure
Lit.Null()
q"null"


q"def myFunc(i: Int) = 2*i"

val importer"$eref.{..$importeesnel}" = importer"edu.utah.cs.gauss.ds2"

