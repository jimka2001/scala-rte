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

case class Or(operands:Seq[Rte]) extends Rte {
  override def toLaTeX:String = "(" +  operands.map(_.toLaTeX).mkString("\\vee ")  + ")"
  override def toString:String = operands.map(_.toString).mkString("Or(", ",", ")")
  def nullable:Boolean = operands.exists{_.nullable}
  def firstTypes:Set[genus.SimpleTypeD] = operands.toSet.flatMap((r:Rte) => r.firstTypes)
  override def canonicalizeOnce:Rte = {
    val betterOperands = operands
      .distinct
      .map(_.canonicalizeOnce)
      .distinct
      .filterNot(_ == EmptySet)
    val singletons:List[genus.SimpleTypeD] = betterOperands.flatMap{
      case Singleton(td) => List(td)
      case _ => List.empty
    }.toList
    lazy val maybeSub = singletons.find{sub =>
      singletons.exists { sup =>
        sub != sup && sub.subtypep(sup).contains(true)
      }}
    if (betterOperands.isEmpty)
      EmptySet
    else if (betterOperands.sizeIs == 1)
      betterOperands.head
    else if (betterOperands.exists(Rte.isOr)) {
      val orops = betterOperands.flatMap {
        case Or(Seq(rs@_*)) => rs
        case r => Seq(r)
      }
      Or(orops)
    }
    else if (maybeSub.nonEmpty)
      Or(betterOperands.filterNot(_ == Singleton(maybeSub.get)))
    else
      Or(betterOperands)
  }
}

object Or {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Or(operands)
}
