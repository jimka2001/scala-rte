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

final case class Cat(operands:Seq[Rte]) extends Rte {
  override def toLaTeX: String = "(" ++ operands.map(_.toLaTeX).mkString("\\cdot ") ++ ")"

  override def toString: String = operands.map(_.toString).mkString("Cat(", ",", ")")

  def nullable: Boolean = operands.forall(_.nullable)

  def minLength: Int = operands.count(r => !r.nullable)

  def firstTypes: Set[genus.SimpleTypeD] = {
    operands match {
      case Seq() => EmptyWord.firstTypes
      case Seq(r) => r.firstTypes
      case Seq(r, rs@_*) =>
        if (r.nullable)
          r.firstTypes union Cat(rs).firstTypes
        else
          r.firstTypes
    }
  }

  override def canonicalizeOnce: Rte = {
    val betterOperands = Cat.stripRedundant(operands)
      .map(_.canonicalizeOnce)
      .flatMap { // Cat( x, Cat(a, b), y) --> Cat(x,a,b,y)
        case Cat(Seq(rs@_*)) => rs
        case rt => Seq(rt)
      }
      .filterNot(_ == EmptyWord) //  Cat( x, EmptyWord, y) --> Cat( x, y)

    if (betterOperands.isEmpty)
      EmptyWord
    else if (betterOperands.sizeIs == 1)
      betterOperands.head
    else if (betterOperands.contains(EmptySet))
    //  Cat( x, EmptySet, y) --> EmptySet
      EmptySet
    else
      Cat(betterOperands)
  }

  def derivativeDown(wrt: genus.SimpleTypeD): Rte = operands.toList match {
    case Nil => EmptyWord.derivative(Some(wrt))
    case rt :: Nil => rt.derivative(Some(wrt))
    case head :: tail =>
      lazy val term1: Rte = Cat((head.derivative(Some(wrt)) :: tail).toSeq)
      lazy val term2: Rte = Cat(tail).derivative(Some(wrt))
      if (head.nullable)
        Or(Seq(term1, term2))
      else
        term1
  }
}

object Cat {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Cat(operands)

  //  (..., x*, x, x* ...) --> (..., x*, x, ...)
  //  and (..., x*, x* ...) --> (..., x*, ...)
  def stripRedundant(rs:Seq[Rte]):Seq[Rte] = {
    rs.toList.tails.flatMap{
      case Star(r1)::r2::Star(r3)::_ if r1 == r2 && r2 == r3 => Nil
      case Star(r1)::Star(r2)::_ if r1 == r2 => Nil
      case Nil => Nil
      case rt::_ => List(rt)
    }.toSeq
  }
}
