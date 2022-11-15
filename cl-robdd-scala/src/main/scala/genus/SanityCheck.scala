// Copyright (©) 2022 EPITA Research and Development Laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package genus

object SanityCheck {
  import org.reflections.util.ConfigurationBuilder

  // it is suggested to use ConfigurationBuilder rather than empty argument
  // list, but this doesn't seem to work.
  // https://github.com/ronmamo/reflections/issues/324#issuecomment-1246432941
  //val reflect = new org.reflections.Reflections(new ConfigurationBuilder())
  val reflect = new org.reflections.Reflections()

  def main(argv:Array[String]):Unit = {
    describeSubclasses(classOf[java.lang.Number])
    makeNumber()
    describeSubclasses(classOf[List[Any]])
    describeSubclasses(List(1,2,3).getClass)
  }
  def describeSubclasses(cl:Class[_]):Unit = {
    import java.lang.reflect.Modifier
    // getSubTypesOf does not realize that a type is a subtype of itself
    val subs:List[Class[_]] = cl :: reflect.getSubTypesOf(cl).toArray.toList.collect {
      case c: Class[_] => c
    }
    println(s"--------- subclasses of $cl")
    println("   superclass is " + cl.getSuperclass)
    for{ (sub,i) <- subs.zipWithIndex
         } println( s"$i: $sub is " + Modifier.toString(sub.getModifiers))
  }
  def makeNumber():Unit = {
    class MyNumber extends Number {
      def doubleValue():Double = 0.0
      def longValue():Long = 0
      def intValue():Int = 0
      def floatValue():Float = 0.0F
    }
    new MyNumber
  }
}
