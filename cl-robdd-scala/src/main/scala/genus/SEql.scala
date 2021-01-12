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

/** The equal type, a type which is equal to a given object.
 *
 * @param a the object defining the type
 */
case class SEql(a: Any) extends SMemberImpl(a) {
  override def toString = s"[= $a]"

  override def typep(b: Any): Boolean = {
    a == b
  }

  override def inhabitedDown: Option[Boolean] = Some(true)

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    if (t.typep(a)) Some(false)
    else Some(true)
  }

  override def subtypep(t: SimpleTypeD): Option[Boolean] = {
    Some(t.typep(a))
  }

  // EqlType(a: Any)
  override def cmpToSameClassObj(t:SimpleTypeD):Boolean = {
    if (this == t)
      false
    else t match {
      case SEql(b: Any) =>
        if( ! (a.getClass eq b.getClass))
          a.getClass.toString <= b.getClass.toString
        else if (a.toString != b.toString)
          a.toString <= b.toString
        else
          throw new Exception(s"cannot compare $this vs $t because they are different yet print the same")
      case _ => super.cmpToSameClassObj(t)  // throws an exception
    }
  }
}
