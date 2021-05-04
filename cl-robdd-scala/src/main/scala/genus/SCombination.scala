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
  def create(tds: SimpleTypeD*):SimpleTypeD
  val unit:SimpleTypeD
  val zero:SimpleTypeD
  // TODO, not sure what is the correct name for this function.
  //   it is the method which is b.subtypep(a) for SOr  equiv a.supertypep(b)
  //                         and a.subtypep(b) for SAnd equiv b.supertypep(a)
  def annihilator(a:SimpleTypeD,b:SimpleTypeD):Option[Boolean]
  def sameCombination(td:SimpleTypeD):Boolean = false

  // UnionType(tds: Type*)
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    findSimplifier(List[() => SimpleTypeD](
      () => {
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
      },
      () => {
        // (and A B SEmpty C D) -> SEmpty,  unit=STop,   zero=SEmpty
        // (or A B STop C D) -> STop,     unit=SEmpty,   zero=STop
        if (tds.contains(zero))
          zero
        else
          this
      },
      () => {
        // (and A (not A)) --> SEmpty,  unit=STop,   zero=SEmpty
        // (or A (not A)) --> STop,     unit=SEmpty, zero=STop
        if (tds.exists(td => tds.contains(SNot(td))))
          zero
        else
          this
      },
      () => {
        // SAnd(A,STop,B) ==> SAnd(A,B),  unit=STop,   zero=SEmpty
        // SOr(A,SEmpty,B) ==> SOr(A,B),  unit=SEmpty, zero=STop
        if (tds.contains(unit))
          create(tds.filterNot(_ == unit): _*)
        else
          this
      },
      () => {
        // (and A B A C) -> (and A B C)
        // (or A B A C) -> (or A B C)
        create(tds.distinct: _*)
      },
      () => {
        // (and A (and B C) D) --> (and A B C D)
        // (or A (or B C) D) --> (or A B C D)
        if (!tds.exists(sameCombination))
          this
        else {
          create(tds.flatMap {
            case td:SCombination if sameCombination(td) => td.tds
            case x => Seq(x)
          }: _*)
        }
      },
      () => {
        val i2 = create(tds.map((t: SimpleTypeD) => t.canonicalize(nf = nf))
                          .sortWith(cmpTypeDesignators): _*)
          .maybeDnf(nf).maybeCnf(nf)
        if (this == i2)
          this // return the older object, hoping the newer one is more easily GC'ed, also preserves EQ-ness
        else {
          i2
        }
      },
      () => {
        // (or A (not B)) --> STop   if B is subtype of A,    zero=STop
        // (and A (not B)) --> SEmpty   if B is supertype of A,   zero=SEmpty
        tds.find{a => tds.exists{
          case SNot(b) if annihilator(a,b).contains(true) => true
          case _ => false
        }} match {
          case None => this
          case Some(_) => zero
        }
      },
      ))
  }
  // SCombination(tds: SimpleTypeD*)
  override def cmpToSameClassObj(td:SimpleTypeD):Boolean = {
    // this method is only called if td and this have exactly the same class
    if (this == td)
      false // sortWith requires returning false if A not < B, including the case that A == B.
    else td match {
      case sc:SCombination => compareSequence(this.tds,sc.tds)
      case _ => super.cmpToSameClassObj(td) // throws an exception
    }
  }
}