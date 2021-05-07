// Copyright (c) 2021 EPITA Research and Development Laboratory
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
//

package rte

abstract class Rte {
  def |(r: Rte): Rte = Or(this, r)
  def &(r: Rte): Rte = And(this, r)
  def ++(r: Rte): Rte = Cat(this,r)
  def unary_! : Rte = Not(this)
  def ?():Rte = Or(this,EmptyWord)
  def *():Rte = Star(this) // postfix operator
  def +():Rte = Cat(this,Star(this)) // postfix operator

  def toLaTeX:String
  override def toString:String = toLaTeX
  def nullable:Boolean
  def firstTypes:Set[genus.SimpleTypeD]
}

object Rte {
  def randomSeq(depth:Int):Seq[Rte] = {
    val maxCompoundSize = 2
    (0 until maxCompoundSize).map{ _ => randomRte(depth)}
  }
  def randomRte(depth:Int):Rte = {
    import scala.util.Random
    val random = new Random
    val generators:Seq[()=>Rte] = Vector(
      () => Not(randomRte(depth - 1)),
      () => Star(randomRte(depth - 1)),
      () => And(randomSeq(depth-1)),
      () => Cat(randomSeq(depth-1)),
      () => Or(randomSeq(depth-1)),
      () => Singleton(genus.Types.randomType(0))
      )
    if (depth <= 0)
      Singleton(genus.Types.randomType(0))
    else {
      val g = generators(random.nextInt(generators.length))
      g()
    }
  }
}

object sanityTest {
  def main(argv: Array[String]):Unit = {
    import genus._
    println(Or(And(Singleton(SAtomic(classOf[Integer])),
                   Not(Singleton(SAtomic(classOf[Long])))),
               Not(Singleton(SEql(42)))))

    import RteImplicits._
    println(Or(And(SAtomic(classOf[Integer]),
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(43))))

    println(Or(And(classOf[Integer],
                   Not(SAtomic(classOf[Long]))),
               Not(SEql(44))))

    println(Rte.randomRte(2))
  }
}