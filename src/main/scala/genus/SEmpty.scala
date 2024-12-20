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

/** The empty type, subtype of all types. */
object SEmpty extends STerminal {
  override def toString = "SEmpty"
  override def toDot():String = "SEmpty"
  override def toLaTeX():String = "\\emptyset"
  override def toMachineReadable():String = "SEmpty"

  override def typep(a: Any): Boolean = false

  override protected def inhabitedDown: Some[Boolean] = Some(false)

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = Some(true)

  override def subtypep(t: SimpleTypeD): Option[Boolean] = Some(true)

  override def cmpToSameClassObj(t:SimpleTypeD):Boolean = false

  override lazy val sampleValues:Set[Any] = Set[Any]()
}
