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

object Simulate {

  def findReachableFinal[Sigma, L, E](dfa: Dfa[L, E], delta: (Sigma, L) => Boolean)(seq: Seq[Sigma]): Option[State[L, E]] = {
    val init: Option[State[L, E]] = Some(dfa.q0)
    seq.foldLeft(init) { (mq: Option[State[L, E]], s: Sigma) =>

      for{
        q <- mq
        Transition(_,_,d:State[L,E]) <- q.transitions.find { case Transition(_, l, _) => delta(s, l) }
      } yield d
    }
  }

  def simulate[Sigma, L, E](dfa: Dfa[L, E], delta: (Sigma, L) => Boolean)(seq: Seq[Sigma]): Option[E] = {

    for{
      d <- findReachableFinal(dfa,delta)(seq)
      if dfa.F.contains(d)
    } yield dfa.exitValue(d)
  }
}

