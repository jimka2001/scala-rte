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
  def ? :Rte = Or(this,EmptyWord)
  def * :Rte = Star(this) // postfix operator
  def + :Rte = Cat(this, Star(this)) // postfix operator
  def ^(n:Short):Rte = {
    n match {
      case 0 => EmptyWord
      case 1 => this
      case i if i > 1 => Cat(Seq.fill(n)(this.canonicalize))
      case i if i < 0 => throw new Error("^ operator does not work with negative numbers: $n")
    }
  }
  // isomorphic
  def ~=(that:Rte):Boolean = {
    if (this == that)
      true
    else (this,that) match {
        // compare the arguments of And and Or in any order
      case (Or(Seq(r1s@_*)),Or(Seq(r2s@_*))) => r1s.toSet == r2s.toSet
      case (And(Seq(r1s@_*)),And(Seq(r2s@_*))) => r1s.toSet == r2s.toSet
      case _ => false
    }
  }
  def toLaTeX:String
  //override def toString:String = toLaTeX
  def nullable:Boolean
  def firstTypes:Set[genus.SimpleTypeD]
  def canonicalize:Rte = Rte.fixedPoint(this, (r:Rte) => r.canonicalizeOnce, (r1:Rte,r2:Rte)=>r1==r2)
  def canonicalizeOnce:Rte = this

  def derivative(wrt:Option[genus.SimpleTypeD]):Rte = wrt match {
    case None => this
    case Some(td) if td.inhabited.contains(false) => EmptySet
    case Some(td) => derivativeDown(td)
  }

  def derivativeDown(wrt:genus.SimpleTypeD):Rte
}

object Rte {

  import scala.annotation.tailrec

  val sigmaStar:Rte = Star(Sigma)
  val notSigma:Rte = Or(Cat(Sigma,Sigma,sigmaStar),EmptyWord)
  val notEpsilon:Rte = Cat(Sigma,sigmaStar)

  def isAnd(rt:Rte):Boolean = rt match {
    case And(Seq(_*)) => true
    case _ => false
  }

  def isCat(rt:Rte):Boolean = rt match {
    case Cat(Seq(_*)) => true
    case _ => false
  }

  def isPlus(rt:Rte):Boolean = rt match {
    case Cat(Seq(x,Star(y))) => x == y
    case _ => false
  }

  def isStar(rt:Rte):Boolean = rt match {
    case Star(_) => true
    case _ => false
  }

  def isOr(rt:Rte):Boolean = rt match {
    case Or(Seq(_*)) => true
    case _ => false
  }
  def searchReplace[A](xs: Seq[A], search:A, replace:A):Seq[A] = {
    xs.map(x => if (search == x) replace else x)
  }
  def findAdjacent[A](xs: Seq[A], cmp: (A, A) => Boolean): Boolean =
    xs.toList.tails.exists {
      case Nil => false
      case x :: Nil => false
      case x1 :: x2 :: _ => cmp(x1, x2)
    }

  def removeAdjacent[A](xs: Seq[A], cmp: (A, A) => Boolean): Seq[A] =
    xs.toList.tails.flatMap {
      case Nil => Nil
      case x :: Nil => List(x)
      case x1 :: x2 :: _ if cmp(x1, x2) => Nil
      case x :: _ => List(x)
    }.toSeq

  private def fixedPoint[V](value: V, f: V => V, cmp: (V, V) => Boolean): V = {
    @tailrec
    def recur(value: V): V = {
      val newValue = f(value)
      if (cmp(value, newValue))
        value
      else
        recur(newValue)
    }

    recur(value)
  }

  def randomSeq(depth: Int): Seq[Rte] = {
    val maxCompoundSize = 2
    (0 until maxCompoundSize).map { _ => randomRte(depth) }
  }

  def randomRte(depth: Int): Rte = {
    import scala.util.Random
    val random = new Random

    val rteVector = Vector(EmptySet,
                           EmptyWord,
                           Sigma,
                           sigmaStar,
                           notSigma,
                           notEpsilon)
    val generators: Seq[() => Rte] = Vector(
      () => rteVector(random.nextInt(rteVector.length)),
      () => Not(randomRte(depth - 1)),
      () => Star(randomRte(depth - 1)),
      () => And(randomSeq(depth - 1)),
      () => Cat(randomSeq(depth - 1)),
      () => Or(randomSeq(depth - 1)),
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