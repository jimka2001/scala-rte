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

case class Star(operand:Rte) extends Rte {
  override def toLaTeX: String = "(" ++ operand.toLaTeX ++ ")^{*}"
  override def toString: String = "(" + operand.toString + ")*"
  def nullable: Boolean = true

  def firstTypes: Set[genus.SimpleTypeD] = operand.firstTypes

  override def canonicalizeOnce: Rte = operand.canonicalizeOnce match {
    case EmptyWord => EmptyWord
    case EmptySet => EmptyWord
    case Star(rt) => Star(rt) // x** -> x*
    case Cat(Seq(x,ys@Star(y))) if x == y => ys // (x x*)* = x*
    case Cat(Seq(xs@Star(x),y)) if x == y => xs // (x* x)* = x*
    // TODO, Star(Cat(X, Y, Z, Star( Cat(X, Y, Z))))
    //   -->    Star( Cat(X, Y, Z))
    case rt => Star(rt)
  }

  def derivativeDown(wrt: genus.SimpleTypeD): Rte = Cat(operand.canonicalize.derivative(Some(wrt)),
                                                        this)
}
