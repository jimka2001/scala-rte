import scala.reflect.runtime.universe._
def getType[A: TypeTag](a: A): Type = typeOf[A]

val data = List(1, 2.0, "three")

data.map(_.getClass)

data.map(getType)

val str:String = data(2).asInstanceOf[String]

scala.util.Properties.releaseVersion


