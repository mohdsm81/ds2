

object tryingthings {

  trait A {

    def toJson: String = {
      getClass.getSimpleName
    }
  }

  class B extends A
  class C extends A
  class D extends C

  implicit class FromJsonScheduler(val sch: A) {
    def fromJson(js: String) = {
    sch.getClass
    //sch.getClass.getSimpleName + " -- " + js
    }
  }

  new B toJson                                    //> res0: String = B

  // but i don't have the instance when i want it to be importent from json
  new B fromJson ("whatever")                     //> res1: Class[?0] = class tryingthings$B

  new C fromJson ("YAY!")                         //> res2: Class[?0] = class tryingthings$C

  new D fromJson ("SUPER!")                       //> res3: Class[?0] = class tryingthings$D

}