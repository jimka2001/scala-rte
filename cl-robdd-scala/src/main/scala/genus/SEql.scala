// Copyright (c) 2020,21 EPITA Research and Development Laboratory
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

import adjuvant.Adjuvant.eql
import genus.Types.cmpTypeDesignators

/** The equal type, a type which is equal to a given object.
 *
 * @param a the object defining the type
 */
case class SEql(a: (SimpleTypeD,Any)) extends SMemberImpl(Vector(a)) with TerminalType {
  override def toMachineReadable():String = toString

  override def toString: String = {
    s"SEql(${a._2} /* " +
      (a._1 match {
        case td@SAtomic(_) => td.shortTypeName()
        case _ => "???"
      }) +
      "*/ )"
  }

  override def toDot(): String = {
    a._2.toString
  }

  override def typep(b: Any): Boolean = {
     a._2 == b && a._1.typep(b)
  }

  override protected def inhabitedDown: Option[Boolean] = Some(true)

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    if (t.typep(a._2)) Some(false)
    else Some(true)
  }

  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    Some(t.typep(a._2))
  }

  // SEql((SAtomic,Any))
  override def cmpToSameClassObj(t:SimpleTypeD):Boolean = {
    if (this == t)
      false
    else t match {
      case SEql(tdb) =>
        tdb match {
          case (td:SimpleTypeD,b:Any) =>
            if (a._1 != td)
              cmpTypeDesignators(a._1,td)
            else
              a._2.toString < b.toString
        }
      case _ => super.cmpToSameClassObj(t)  // throws an exception
    }
  }
}

object SEql {
  def apply(a:Any):SimpleTypeD = new SEql((SAtomic(a.getClass),a))
}
