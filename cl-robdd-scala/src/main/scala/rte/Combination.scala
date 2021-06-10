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

import adjuvant.Adjuvant.{findSimplifier, searchReplace, uniquify}
import genus._

abstract class Combination(val operands:Seq[Rte]) extends Rte {
  val zero:Rte
  val one:Rte
  def create(operands: Seq[Rte]):Rte
  def createDual(operands: Seq[Rte]):Rte
  def sameCombination(c:Combination):Boolean
  def dualCombination(d:Combination):Boolean
  def firstTypes: Set[SimpleTypeD] = operands.toSet.flatMap((r: Rte) => r.firstTypes)

  def conversion3():Rte = {
    // Or(... Sigma* ....) -> Sigma*
    // And(... EmptySet ....) -> EmptySet
    if (operands.contains(zero))
      zero
    else
      this
  }

  def conversion4():Rte = {
    create(uniquify(operands))
  }

  def conversion5():Rte = {
    create(Rte.sortAlphabetically(operands))
  }

  def conversion6():Rte = {
    // remove Sigma* and flatten And(And(...)...)
    // remove EmptySet and flatten Or(Or(...)...)
    create(operands.flatMap{
      case r if r == one => Seq()
      case c:Combination if sameCombination(c) => c.operands
      case r => Seq(r)
    })
  }

  def conversionC7():Rte = {
    // (:or A B (:* B) C)
    // --> (:or A (:* B) C)
    // (:and A B (:* B) C)
    // --> (:and A B C)
    val stars = operands.collect{
      case rt@Star(_) => rt
    }
    if (stars.isEmpty)
      this
    else
      create(operands.flatMap{
        case r1 if Rte.isOr(this) && stars.contains(Star(r1))  => Seq()
        case Star(r1) if Rte.isAnd(this) && operands.contains(r1) => Seq()
        case r => Seq(r)
      })
  }

  def conversionC11():Rte = {
    // And(...,x,Not(x)...)
    if (operands.exists(r1 => operands.contains(Not(r1))))
      zero
    else
      this
  }

  override def canonicalizeOnce: Rte = {
    findSimplifier(tag="combination",target=this,step=0,verbose=false,List[() => Rte](
      () => { super.canonicalizeOnce }
      ))
  }

  def derivativeDown(wrt:SimpleTypeD):Rte = create(operands.map(rt => rt.derivative(Some(wrt))))
}


