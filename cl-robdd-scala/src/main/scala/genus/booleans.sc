true.getClass
false.getClass
Vector(true,false).map(_.getClass)
true.getClass == classOf[java.lang.Boolean]
true.getClass == classOf[Boolean]
classOf[Boolean]
classOf[java.lang.Boolean]
classOf[Boolean].isInstance(true)
((b:Any) => classOf[Boolean].isInstance(b))(true)
((b:Boolean) => classOf[Boolean].isInstance(b))(true)

classOf[java.lang.Boolean].isInstance(true)
val t1 = true
t1.getClass
true.getClass
List(t1,t1).map(x => x.getClass)

def foo(xs:Seq[Any]) = {
  xs.map(t=>t.getClass)
}

foo(Seq(true))
List(true).map((x:Any) => x.getClass)
List(true).map(x => x.getClass)
(((x:Any) => x.getClass)(true))

1

