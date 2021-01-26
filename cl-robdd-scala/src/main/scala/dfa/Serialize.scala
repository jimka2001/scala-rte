// Copyright (c) 2019 EPITA Research and Development Laboratory
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

package dfa

object Serialize {

  def serialize[L,E](dfa:Dfa[L,E]):Unit = serialize(dfa,print)

  def serialize[L,E](dfa:Dfa[L,E], print:String=>Unit):Unit = {
    print(s"Q=${dfa.Q}\n")
    print(s"q0=${dfa.q0}\n")
    print(s"F=${dfa.F}\n")
    for{
      q <- dfa.Q
      tr <- q.transitions
    } print(s"delta($q,${tr.label}) = ${tr.destination}\n")
  }

  def serializeToString[L,E](dfa:Dfa[L,E]):String = {
    import cl.Accumulators.withOutputToString

    withOutputToString(printer => {
      printer("{\n")
      serialize(dfa,printer)
      printer("}\n")
    })
  }
}
