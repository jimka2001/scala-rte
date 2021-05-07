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

case class Not(operand:Rte) extends Rte {
  override def toLaTeX:String = "\\overline{" ++  operand.toLaTeX ++ "}"
  def nullable:Boolean = ! operand.nullable
  def firstTypes:Set[genus.SimpleTypeD] = operand.firstTypes
  val sigmaStar:Rte = Star(Sigma)
  val notSigma:Rte = Or(Cat(Sigma,Sigma,sigmaStar),EmptyWord)
  val notEpsilon:Rte = Cat(Sigma,sigmaStar)

  override def canonicalizeOnce:Rte = {
    val betterOperand: Rte = operand.canonicalizeOnce
    betterOperand match {
      case Sigma => notSigma
      case `sigmaStar` => EmptySet
      case EmptyWord => notEpsilon
      case EmptySet => sigmaStar

      // Not(Not(r)) -> Not(r)
      case Not(r) => r.canonicalizeOnce
      // Not(And(a,b)) -> Or(Not(a),Not(b))
      case And(Seq(rs@_*)) => Or(rs.map(r => Not(r)))
      // Not(Or(a,b)) -> And(Not(a),Not(b))
      case Or(Seq(rs@_*)) => And(rs.map(r => Not(r)))
      case _ => Not(betterOperand)
    }
  }
}
