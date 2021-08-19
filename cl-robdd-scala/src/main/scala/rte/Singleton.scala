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
import genus._

case class Singleton(td:SimpleTypeD) extends Rte {
  override def toLaTeX: String = td.toString

  override def toString: String = "<" + td.toString + ">"

  def nullable: Boolean = false

  def firstTypes: Set[SimpleTypeD] = Set(td)

  def inhabited: Option[Boolean] = td.inhabited

  override def canonicalizeOnce: Rte = {
    td.canonicalize() match {
      case STop => Sigma
      case SEmpty => EmptySet
      // TODO I think this can be removed because if td is not inhabited it has already been reduced to SEmpty
      // case td if td.inhabited.contains(false) => EmptySet
      case SAnd(operands@_*) => And.createAnd(operands.map(Singleton))
      case SOr(operands@_*) => Or.createOr(operands.map(Singleton))
      case SNot(operand) => And(Not(Singleton(operand)),
                                Sigma)
      case td2 => Singleton(td2)
    }
  }

  def derivativeDown(wrt:SimpleTypeD):Rte = wrt match {
    case `td` => EmptyWord
    case STop => EmptyWord
    case td2:SimpleTypeD if td2.disjoint(td).contains(true) => EmptySet
    case td2:SimpleTypeD if td2.subtypep(td).contains(true) => EmptyWord
    case SAnd(tds@ _*) if tds.contains(SNot(td)) => EmptySet
    case SAnd(tds@ _*) if tds.contains(td) => EmptyWord
    case _ => throw new CannotComputeDerivative(s"cannot compute derivative of $this wrt=$wrt, disjoint= "
                                                  + wrt.disjoint(td)
                                                  + " subtypep = "
                                                  + wrt.subtypep(td),
                                                wrt=wrt,
                                                rte=this)
  }

  override def derivative(wrt:Option[SimpleTypeD]):Rte = (wrt,td) match {
    case (_,genus.SEmpty) => EmptySet.derivative(wrt)
    case (_,genus.STop) => Sigma.derivative(wrt)
    case (None,_) => this
    case (_,td) if td.inhabited.contains(false) => EmptySet.derivative(wrt)
    case _ => super.derivative(wrt)
  }
}
