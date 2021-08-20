object SomeJValue {
	import net.liftweb.json._
	import net.liftweb.json.JsonDSL._
	
	
	
	val v1 = JField("n", Some(1))             //> v1  : net.liftweb.json.JsonAST.JField = JField(n,JInt(1))
	val v2 = JField("n",Some(JString("None")))//> v2  : net.liftweb.json.JsonAST.JField = JField(n,JString(None))
	

	val v3: JValue = Some("A String")         //> v3  : net.liftweb.json.JValue = JString(A String)
	
	v3 match {
	case JString(x) => x
	case _ => None
	}                                         //> res0: java.io.Serializable = A String
	
}