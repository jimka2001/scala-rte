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

case class Singleton(td:genus.SimpleTypeD) extends Rte {
  override def toLaTeX:String = td.toString
  override def toString:String = "<" + td.toString + ">"
  def nullable:Boolean = false
  def firstTypes:Set[genus.SimpleTypeD] = Set(td)
  override def canonicalizeOnce:Rte = {
    td match {
      case genus.SAnd(operands@_*) => And( operands.map(td => Singleton(td.canonicalize()).canonicalizeOnce))
      case genus.SOr(operands@_*) => Or( operands.map(td => Singleton(td.canonicalize()).canonicalizeOnce))
      case genus.SNot(operand) => And( Not(Singleton(operand.canonicalize()).canonicalizeOnce),
                                Sigma)
      case genus.STop => Sigma
      case genus.SEmpty => EmptySet
      case td if td.inhabited.contains(false) => EmptySet
      case _ => Singleton(td.canonicalize())
    }
  }
}
