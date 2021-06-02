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

package genus

import Types._
import NormalForm._

// The purpose of this class, SCombination, is to serve as a superclass
// of both SAnd and SOr, as there is quite a bit of common code between
// the two classes (SAnd and SOr).  In some cases factoring the code
// out means replacing SEmpty with STop, or vice versa, and
// replacing subtypep with supertypep, etc.
abstract class SCombination(val tds: SimpleTypeD*) extends SimpleTypeD {
  def create(tds: Seq[SimpleTypeD]): SimpleTypeD

  def createDual(tds: Seq[SimpleTypeD]): SimpleTypeD

  val unit: SimpleTypeD
  val zero: SimpleTypeD

  // TODO, not sure what is the correct name for this function.
  //   it is the method which is b.subtypep(a) for SOr  equiv a.supertypep(b)
  //                         and a.subtypep(b) for SAnd equiv b.supertypep(a)
  def annihilator(a: SimpleTypeD, b: SimpleTypeD): Option[Boolean]

  def sameCombination(td: SimpleTypeD): Boolean = false

  def dualCombination(td: SimpleTypeD): Boolean = false

  def conversion1(): SimpleTypeD = {
    if (tds.isEmpty) {
      // (and) -> STop,  unit=STop,   zero=SEmpty
      // (or) -> SEmpty, unit=SEmpty,   zero=STop
      unit
      // (and A) -> A
      // (or A) -> A
    } else if (tds.tail.isEmpty)
      tds.head
    else
      this
  }

  def conversion2(): SimpleTypeD = {
    // (and A B SEmpty C D) -> SEmpty,  unit=STop,   zero=SEmpty
    // (or A B STop C D) -> STop,     unit=SEmpty,   zero=STop
    if (tds.contains(zero))
      zero
    else
      this
  }

  def conversion3(): SimpleTypeD = {
    // (and A (not A)) --> SEmpty,  unit=STop,   zero=SEmpty
    // (or A (not A)) --> STop,     unit=SEmpty, zero=STop
    if (tds.exists(td => tds.contains(SNot(td))))
      zero
    else
      this
  }

  def conversion4(): SimpleTypeD = {
    // SAnd(A,STop,B) ==> SAnd(A,B),  unit=STop,   zero=SEmpty
    // SOr(A,SEmpty,B) ==> SOr(A,B),  unit=SEmpty, zero=STop
    if (tds.contains(unit))
      create(tds.filterNot(_ == unit))
    else
      this
  }

  def conversion5(): SimpleTypeD = {
    // (and A B A C) -> (and A B C)
    // (or A B A C) -> (or A B C)
    create(adjuvant.Adjuvant.uniquify(tds))
  }

  def conversion6(): SimpleTypeD = {
    // (and A (and B C) D) --> (and A B C D)
    // (or A (or B C) D) --> (or A B C D)
    if (!tds.exists(sameCombination))
      this
    else {
      create(tds.flatMap {
        case td: SCombination if sameCombination(td) => td.tds
        case x => Seq(x)
      })
    }
  }

  def conversion7(nf: Option[NormalForm]): SimpleTypeD = {
    val i2 = create(tds.map((t: SimpleTypeD) => t.canonicalize(nf = nf))
                      .sortWith(cmpTypeDesignators))
      .maybeDnf(nf).maybeCnf(nf)
    if (this == i2)
      this // return the older object, hoping the newer one is more easily GC'ed, also preserves EQ-ness
    else {
      i2
    }
  }

  def conversion8(): SimpleTypeD = {
    // (or A (not B)) --> STop   if B is subtype of A,    zero=STop
    // (and A (not B)) --> SEmpty   if B is supertype of A,   zero=SEmpty
    tds.find { a =>
      tds.exists {
        case SNot(b) if annihilator(a, b).contains(true) => true
        case _ => false
      }
    } match {
      case None => this
      case Some(_) => zero
    }
  }

  def conversion9(): SimpleTypeD = {
    // (A+B+C)(A+!B+C)(X) -> (A+B+C)(A+C)(X)  (later -> (A+C)(X))
    // (A+B+!C)(A+!B+C)(A+!B+!C) -> (A+B+!C)(A+!B+C)(A+!C) ->
    // (A+B+!C)(A+!B+C)(A+!B+!C) -> does not reduce to (A+B+!C)(A+!B+C)(A)
    import adjuvant.Adjuvant.searchReplace
    val duals = tds.collect { case td: SCombination if dualCombination(td) => td }
    val outerArgs = tds.map {
      // A+!B+C -> A+C
      // A+B+C -> A+B+C
      // X -> X
      case td1: SCombination if dualCombination(td1) =>
        // A+!B+C -> A+C
        // A+B+C -> A+B+C
        val toRemove = td1.tds.collectFirst {
          case td@SNot(n) if duals.exists {
            case td2: SCombination if dualCombination(td2) => td2.tds == searchReplace(td1.tds, td, Seq(n))
          } => td
        } // Some(!B) or None
        toRemove match {
          case None => td1
          case Some(td) => createDual(td1.tds.filterNot(_ == td))
        }
      case td => td // X -> X
    }
    create(outerArgs)
  }

  def conversion10(): SimpleTypeD = {
    // (and A B C) --> (and A C) if  A is subtype of B
    tds.find(sub => tds.exists { sup =>
      ((sub != sup)
        && annihilator(sub, sup).contains(true))
    }) match {
      case None => this
      case Some(sub) =>
        // throw away all proper superclasses of sub, i.e., keep everything that is not a superclass
        // of sub and also keep sub itself.   keep false and dont-know
        val keep = tds.filter(sup => sup == sub || !annihilator(sub, sup).contains(true))
        create(keep)
    }
  }

  def conversion11(): SimpleTypeD = {

    // A + A! B -> A + B
    // A + A! BX + Y = (A + BX + Y)
    // A + ABX + Y = (A + Y)

    // TODO need to look at these relations if A < B or B < A,
    //  I'm not yet sure what will happen, but I think there are some simplifications to be made
    val ao = tds.find(a =>
                        tds.exists {
                          case td: SCombination if dualCombination(td) =>
                            td.tds.exists {
                              case SNot(b) if a == b => true
                              case b if a == b => true
                              case _ => false
                            }
                          case _ => false
                        })
    ao match {
      case None => this
      case Some(a) =>
        create(
          tds.flatMap {
            case td: SCombination if sameCombination(td) => Seq(td)
            case td: SCombination if td.tds.contains(a) => Seq()
            case td: SCombination if td.tds.contains(SNot(a)) => Seq(createDual(td.tds.filterNot(_ == SNot(a))))
            case b => Seq(b)
          })
    }
  }

  def conversion12(): SimpleTypeD = {
    // AXBC + !X = ABC + !X
    tds.find {
      case SNot(x) => tds.exists {
        case td: SCombination if dualCombination(td) => td.tds.contains(x)
        case _ => false
      }
      case _ => false
    } match {
      case Some(SNot(x)) => create(tds.map {
        case td: SCombination if dualCombination(td) => createDual(td.tds.filter(_ != x))
        case a => a
      })
      case _ => this
    }
  }

  // SCombination(tds: SimpleTypeD*)
  override def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => { conversion1() },
      () => { conversion2() },
      () => { conversion3() },
      () => { conversion4() },
      () => { conversion5() },
      () => { conversion6() },
      () => { conversion7(nf) },
      () => { conversion8() },
      () => { conversion9() },
      () => { conversion10() },
      () => { conversion11() },
      () => { conversion12() }
      ))
  }

  // SCombination(tds: SimpleTypeD*)
  override def cmpToSameClassObj(td: SimpleTypeD): Boolean = {
    // this method is only called if td and this have exactly the same class
    if (this == td)
      false // sortWith requires returning false if A not < B, including the case that A == B.
    else td match {
      case sc: SCombination => compareSequence(this.tds, sc.tds)
      case _ => super.cmpToSameClassObj(td) // throws an exception
    }
  }
}