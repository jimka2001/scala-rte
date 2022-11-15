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
import adjuvant.Adjuvant.findSimplifier
import genus._

case class Or(override val operands:Seq[Rte]) extends Combination(operands) {
  override def toMachineReadable(): String = operands.map(_.toMachineReadable()).mkString("Or(", ",", ")")
  override def toString: String = operands.map(_.toString).mkString("Or(", ",", ")")
  override def toDot(): String = operands.map(_.toDot()).mkString("Or(", ",", ")")
  override def toLaTeX():String = operands.map(_.toLaTeX()).mkString("(", "\\vee ", ")")

  val zero:Rte = Rte.sigmaStar
  val one:Rte = EmptySet
  def sameCombination(c:Combination):Boolean = {
    c match {
      case _:Or => true
      case _ => false
    }
  }
  def create(operands: Seq[Rte]):Rte = {
    Or.createOr(operands)
  }
  def createTypeD(operands: Seq[SimpleTypeD]):SimpleTypeD = SOr.createOr(operands)
  def dualCombination(c:Combination):Boolean = {
    c match {
      case _:And => true
      case _ => false
    }
  }
  def createDual(operands: Seq[Rte]):Rte = {
    And.createAnd(operands)
  }
  override def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean] = {
    a.supertypep(b)
  }
  def orInvert(x:Boolean):Boolean = ! x
  def setDualOperation(a:Seq[Any],b:Seq[Any]):Seq[Any] = a.filter(x => b.contains(x)) // intersection
  def setOperation(a:Seq[Any],b:Seq[Any]):Seq[Any] = a ++ b.diff(a) // union

  def nullable: Boolean = operands.exists {
    _.nullable
  }

  def conversionO8(existsNullable : => Boolean):Rte = {
    val maybePlus = operands.find(Rte.isPlus)

    // (:or A :epsilon B (:cat X (:* X)) C)
    //   --> (:or A :epsilon B (:* X) C )
    // (:or :epsilon (:cat X (:* X)))
    //   --> (:or :epsilon (:* X))
    // (:or (:* Y) (:cat X (:* X)))
    //   --> (:or (:* Y) (:* X))

    if (existsNullable && maybePlus.nonEmpty)
      create(operands.map {
        case Cat(Seq(x, z@Star(y))) if x == y => z
        case Cat(Seq(z@Star(x), y)) if x == y => z
        case rt => rt
      })
    else
      this
  }

  def conversionO9(existsNullable: =>Boolean):Rte = {
    lazy val maybeCatxyz = operands.find(Rte.catxyzp) // (:cat X Y Z (:* (:cat X Y Z)))

    // (:or A :epsilon B (:cat X Y Z (:* (:cat X Y Z))) C)
    //   --> (:or A :epsilon B (:* (:cat X Y Z)) C )
    // (:or :epsilon (:cat X Y Z (:* (:cat X Y Z))))
    //   --> (:or :epsilon (:* (:cat X Y Z)))
    if (existsNullable && maybeCatxyz.nonEmpty)
      create(operands.map{
        case c@Cat(Seq(rs@_*)) if maybeCatxyz.contains(c) => rs.last
        case rt => rt
      })
    else
      this
  }

  def conversionO10():Rte = {
    // (:or A :epsilon B (:* X) C)
    //   --> (:or A B (:* X) C)
    if (operands.contains(EmptyWord) && operands.exists(r => r != EmptyWord && r.nullable))
      create(operands.filterNot(_ == EmptyWord))
    else
      this
  }

  def conversionO11b():Rte = {
    // if Sigma is in the operands, then filter out all singletons
    // Or(Singleton(A),Sigma,...) -> Or(Sigma,...)
    if (operands.contains(Sigma))
      create(operands.flatMap{
        case Singleton(_) => Seq()
        case td => Seq(td)
      })
    else
      this
  }

  def conversionO15():Rte = {
    // Or(Not(A),B*,C) = Or(Not(A),C) if A and B  disjoint,
    //   i.e. remove all B* where B is disjoint from A

    val as = operands.collect{
      case Not(Singleton(td)) => td
    }
    val bs: Seq[Star] = operands.collect{
      // set of b's which are disjoint with at least one a in as
      case s@Star(Singleton(b)) if as.exists(a => a.disjoint(b).contains(true)) => s
    }

    bs match {
      case Seq() => this
      case _ => create(operands.diff(bs))
    }
  }

  def conversionD16b():Rte = {
    val nn = operands.collect{
      case Not(Singleton(td)) => td
    }
    val filtered = operands.flatMap{
      // Or(A,x,Not(y))  --> Or(A,Not(y))
      case Singleton(td) if nn.exists(d => td.disjoint(d).contains(true)) => Seq()
      case r => Seq(r)
    }
    create(filtered)
  }

  override def canonicalizeOnce: Rte = {
    lazy val existsNullable = operands.exists(_.nullable)
    findSimplifier(tag="or",target=this,verbose=false,List[(String,() => Rte)](
      // the following is a series of pairs (String, ()=>Rte)
      //   the String is used only for debugging, to aid in detecting which
      //   of the function fails to convert something it is intended to convert.
      //   To enable the debugging, change verbose=false to verbose=true
      //   in the call to findSimplifier.
      "1" -> (() => { conversionC1() }),
      "3" -> (() => { conversionC3() }),
      "4" -> (() => { conversionC4() }),
      "6" -> (() => { conversionC6() }),
      "7" -> (() => { conversionC7() }),
      "8" -> (() => { conversionO8(existsNullable)}),
      "9" -> (() => { conversionO9(existsNullable)}),
      "10" -> (() => { conversionO10()}),
      "C11" -> (() => { conversionC11()}),
      "C14" -> (() => { conversionC14()}),
      "11b" -> (() => { conversionO11b()}),
      "C16" -> (() => { conversionC16()}),
      "C16b" -> (() => { conversionD16b()}),
      "C12" -> (() => { conversionC12()}),
      "15" -> (() => { conversionO15()}),
      "21" -> (() => { conversionC21()}),
      "C15" -> (() => { conversionC15()}),
      "C17" -> (() => { conversionC17()}),
      "99" -> (() => { conversionC99() }),
      "5" -> (() => { conversionC5() }),
      "super" -> (() => { super.canonicalizeOnce })
      ))
  }

}

object Or {
  def apply(operands: Rte*)(implicit ev: DummyImplicit) = new Or(operands)
  def createOr(operands: Seq[Rte]):Rte = {
    operands match {
      case Seq() => EmptySet
      case Seq(rt) => rt
      case _ => Or(operands)
    }
  }
}

object Xor {
  def apply(a:Rte, b:Rte):Rte = Or(And(a,Not(b)),
                                   And(Not(a),b))
}
