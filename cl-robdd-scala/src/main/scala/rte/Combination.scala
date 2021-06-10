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
  def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean]

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

  def conversionC16():Rte = {
    // remove superclasses
    //  and remove Not(disjoint) if

    val ss = operands.collect{
      case Singleton(td) => td
    }
    val filtered = operands.flatMap{
      // And(super,sub) --> And(Sigma,sub)
      // Or(super,sub) --> Or(super,EmptySet)
      // case Singleton(sup) if ss.exists(sub => sub != sup && sup.supertypep(sub).contains(true)) => Sigma
      // TODO, we should remove the object rather than replacing with one
      //    be careful not to convert something that matches only singleton
      //    to match more than singleton
      case Singleton(sup) if ss.exists(sub => sub != sup && annihilator(sub,sup).contains(true)) => Seq()
      case r => Seq(r)
    }
    create(filtered)
  }

  def conversionC16b():Rte

  def conversion99():Rte =
    create(operands.map(_.canonicalizeOnce))

  def conversion1():Rte = create(operands)

  override def canonicalizeOnce: Rte = {
    findSimplifier(tag="combination",target=this,step=0,verbose=false,List[() => Rte](
      () => { super.canonicalizeOnce }
      ))
  }

  def derivativeDown(wrt:SimpleTypeD):Rte = create(operands.map(rt => rt.derivative(Some(wrt))))
}


