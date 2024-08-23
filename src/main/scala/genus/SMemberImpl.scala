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

import NormalForm._
import adjuvant.Adjuvant.eql

import scala.annotation.tailrec

abstract class SLiteral(val xs:Vector[(SimpleTypeD,Any)]) extends SimpleTypeD {
  override def toMachineReadable():String = xs.mkString("SMember(", ",", ")")

  override def typep(a: Any): Boolean = xs.exists{case (td,b) => td.typep(a) && a == b}

  override protected def inhabitedDown: Option[Boolean] = Some(xs.nonEmpty) // SMember() is empty

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    if (xs.exists{case (_,b) => t.typep(b)}) Some(false)
    else Some(true)
  }

  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    if (xs.isEmpty)
      SEmpty.subtypep(t)
    else
      Some(xs.forall{case (_,b) => t.typep(b)})
  }

  override def canonicalizeOnce(nf: Option[NormalForm] = None): SimpleTypeD = {
    // Member(true,false) should be the same as SAtomic(java.lang.Boolean)
    if (this == SMember.trueOrFalse)
      SAtomic(classOf[java.lang.Boolean])
    else
      super.canonicalizeOnce(nf)
  }

  // SMember(xs: Any*)
  override def cmpToSameClassObj(t:SimpleTypeD):Boolean = {
    if (this == t)
      false
    else t match {
      case SMember(ys) =>
        @tailrec
        def comp(as:List[Any], bs:List[Any]):Boolean = {
          (as,bs) match {
            case (Nil,Nil) => throw new Exception(s"not expecting equal sequences $xs, $ys")
            case (Nil,_) => true
            case (_,Nil) => false
            case (a::as,b::bs) =>
              if (a == b)
                comp(as,bs)
              else if ( a.toString != b.toString)
                a.toString < b.toString
              else
                throw new Exception(s"cannot compare $this vs $t because two unequal elements print the same: $a{.toString}")
          }
        }
        comp(xs.toList, ys.toList)
      case _ => super.cmpToSameClassObj(t)  // throws an exception
    }
  }

  override def leafTypes(): Set[SimpleTypeD] = {
    xs.map { case (_, a) => SEql(a) }.toSet
  }
}
