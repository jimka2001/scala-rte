// Copyright (©) 2021 EPITA Research and Development Laboratory
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

import adjuvant.Adjuvant.{findSimplifier, uniquify}
import genus.NormalForm._
import genus.Types.{andp, createMemberFromPairs, orp}

/** A union type, which is the union of zero or more types.
 *
 * @param tds var-arg, zero or more types
 */
object SyntaxSugar {
  def SAndNot(td1: SimpleTypeD, td2: SimpleTypeD): SimpleTypeD = SAnd(td1, SNot(td2))
  def SXor(td1: SimpleTypeD, td2: SimpleTypeD): SimpleTypeD = SOr(SAndNot(td1, td2),
                                                                  SAndNot(td2, td1))
}