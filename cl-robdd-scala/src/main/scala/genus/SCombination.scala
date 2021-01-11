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

abstract class SCombination(val tds: SimpleTypeD*) extends SimpleTypeD {
  def create(tds: SimpleTypeD*):SimpleTypeD
  val unit:SimpleTypeD
  val zero:SimpleTypeD

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
        val rs = tds.find {td =>
              if (this.getClass == td.getClass)
                true
              else false
        }
        if (rs.isEmpty)
          this
        else {
          create(tds.flatMap {
            case td:SCombination if this.getClass == td.getClass => td.tds
            case x => Seq(x)
          }: _*)
        }
      },
      () => {
        val i2 = create(tds.map((t: SimpleTypeD) => t.canonicalize(nf = nf)).sortWith(cmpTypeDesignators): _*).maybeDnf(nf).maybeCnf(nf)
        if (this == i2)
          this // return the older object, hoping the newer one is more easily GC'ed
        else {
          i2
        }
      }
      ))
  }
}