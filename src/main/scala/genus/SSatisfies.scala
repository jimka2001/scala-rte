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

/** A custom type, build by a function defined by the user.
 *
 * @param f a function taking an object Any and returning a Boolean defining the type
 */
case class SSatisfies(f   : Any => Boolean, printable:String) extends STerminal {
  override def typep(a: Any): Boolean = f(a)

  override def toString: String = printable + "?"
  override def toLaTeX():String = toString
  override def toMachineReadable():String = s"SSatisfies($printable)"
  override def toDot(): String = toString

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = super.disjointDown(t)

  override protected def inhabitedDown: Option[Boolean] = super.inhabitedDown

  override protected def subtypepDown(t: SimpleTypeD): Option[Boolean] = super.subtypepDown(t)

  override def cmpToSameClassObj(t:SimpleTypeD):Boolean = {
    s"$this" < s"$t"
  }
}

object SSatisfies {
  def apply(f:Any=>Boolean):SSatisfies = new SSatisfies(f, f.toString)
}
