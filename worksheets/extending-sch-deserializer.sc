

object tryingthings {

  trait A {
    def toJson: String = {
      getClass.getSimpleName
    }
  }

  class B extends A
  class C extends A
  class D extends C

  trait DeSerializableFromJson {
    def fromJson(js: String): String
  }

  object DeSerialization {

    implicit class FromJsonScheduler(val sch: A) extends DeSerializableFromJson {
      override def fromJson(js: String) = {
        sch.getClass.getSimpleName
        //sch.getClass.getSimpleName + " -- " + js
      }
    }

    implicit class FromJsonScheduler2(val sch: D) extends DeSerializableFromJson {
      val wrapper = new DeSerialization.FromJsonScheduler(sch)
      
      override def fromJson(js: String) = {
        wrapper.fromJson(js) + " -- " + js
        
      }
    }
  }

  //import DeSerialization.FromJsonScheduler
  import DeSerialization.FromJsonScheduler2

  new B toJson                                    //> res0: String = B

  // but i don't have the instance when i want it to be importent from json
  //new B fromJson ("whatever")

  //new C fromJson ("YAY!")

  new D fromJson ("SUPER!")                       //> res1: String = D -- SUPER!

}