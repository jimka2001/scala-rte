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

case class Not(operand:Rte) extends RteNode {
  override def toLaTeX():String = "\\overline{" ++  operand.toLaTeX() ++ "}"
  override def toDot():String = "!" + operand.toDot()
  override def toMachineReadable():String = "Not(" + operand.toMachineReadable() + ")"
  override def toString = "!" + operand.toString

  def children():Vector[Rte] = Vector(operand)

  def nullable:Boolean = ! operand.nullable
  def firstTypes:Set[SimpleTypeD] = operand.firstTypes

  override def canonicalizeOnce:Rte = {
    operand match {
      case Sigma => Rte.notSigma
      case Singleton(STop) => Rte.notSigma
      case Rte.sigmaStar => EmptySet
      case EmptySeq => Rte.notEmptySeq
      case EmptySet => Rte.sigmaStar
      case Singleton(SEmpty) => Rte.sigmaStar

      // Not(Not(r)) -> r
      case Not(r) => r
      // Not(And(a,b)) -> Or(Not(a),Not(b))
      case And(Seq(rs@_*)) => Or.createOr(rs.map(Not))
      // Not(Or(a,b)) -> And(Not(a),Not(b))
      case Or(Seq(rs@_*)) => And.createAnd(rs.map(Not))

      case _ => Not(operand.canonicalizeOnce)
    }
  }
  def derivativeDown(wrt:SimpleTypeD, factors:List[SimpleTypeD], disjoints:List[SimpleTypeD]):Rte =
    Not(operand.derivative(Some(wrt), factors, disjoints))

  override def search(test:Rte=>Boolean):Option[Rte] = {
    this.operand.search(test) orElse super.search(test)
  }
}
