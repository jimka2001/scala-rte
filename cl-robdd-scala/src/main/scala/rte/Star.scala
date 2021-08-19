// Copyright (©) 2021 EPITA Research and Development Laboratory
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

package rte

import adjuvant.Adjuvant.findSimplifier
import genus._

case class Star(operand:Rte) extends Rte {
  override def toLaTeX: String = "(" ++ operand.toLaTeX ++ ")^{*}"

  override def toString: String = "(" + operand.toString + ")*"

  def nullable: Boolean = true

  def firstTypes: Set[genus.SimpleTypeD] = operand.firstTypes
  def getStarCatOperands(rt: Rte): Seq[Rte] = {
    rt match {
      case Star(Cat(xs)) => xs
      case _ => Seq.empty
    }
  }

  def conversion1():Rte = {
    this match {
      case Star(EmptyWord) => EmptyWord
      case Star(EmptySet) => EmptyWord
      // x** -> x*
      case Star(s@Star(r)) => s
      case _ => this
    }
  }

  def conversion2():Rte = {
    this match {
      case Star(Cat(Seq(x, ys@Star(y)))) if x == y => ys // (x x*)* = x*
      case Star(Cat(Seq(xs@Star(x), y))) if x == y => xs // (x* x)* = x*
      case Star(Cat(Seq(xs@Star(u), x, Star(v)))) if x == u && x == v => xs // (x* x x*)* = x*
      case _ => this
    }
  }

  def conversion3():Rte = {
    this match {
      // Star(Cat(X, Y, Z, Star( Cat(X, Y, Z))))
      //   -->    Star( Cat(X, Y, Z))
      case Star(Cat(xs)) if Rte.isStarCat(xs.last) && getStarCatOperands(xs.last) == xs.dropRight(1) =>
        xs.last
      // Star(Cat(Star( Cat(X, Y, Z)), X, Y, Z))
      //   -->    Star( Cat(X, Y, Z))
      case Star(Cat(xs)) if Rte.isStarCat(xs.head) && getStarCatOperands(xs.head) == xs.tail =>
        xs.head
      // Star(Cat(Star( Cat(X, Y, Z)), X, Y, Z, Star(Cat(X,Y,Z)))
      //   -->    Star( Cat(X, Y, Z))
      case Star(Cat(xs)) if xs.size > 2 &&
        Rte.isStarCat(xs.head)
        && Rte.isStarCat(xs.last)
        && xs.head == xs.last
        && getStarCatOperands(xs.head) == xs.tail.dropRight(1) =>
        xs.head
      case _ => this
    }
  }

  def conversion99():Rte = {
    Star(operand.canonicalizeOnce)
  }

  override def canonicalizeOnce:Rte = {
    findSimplifier(tag="Star",this,verbose=false,List[(String,() => Rte)](
      "1" -> (() => { conversion1() }),
      "2" -> (() => { conversion2() }),
      "3" -> (() => { conversion3() }),
      "4" -> (() => { conversion99() })
      ))
  }

  def derivativeDown(wrt: SimpleTypeD, factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]): Rte =
    Cat(operand.derivative(Some(wrt), factors, disjoints), this)
}
