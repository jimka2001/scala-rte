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

package xymbolyco

class State[Σ,L,E](dfa:Dfa[Σ,L,E], val id:Int) {
  var transitions:Set[Transition[Σ,L,E]] = Set()

  // find the destination state of this state given the label,
  //     this uses a label search, independent of the particular
  //     value of an input sequence.
  def delta(label:L):State[Σ,L,E] = dfa.delta(this,label)

  // find the destination state of this state given an element
  //    of the input sequence.
  def successor(s:Σ):Option[State[Σ,L,E]] = {
    transitions
      .find{case Transition(_,label,_) => dfa.labeler.member(s,label) }
      .flatMap{case Transition(_,_,dest) => Some(dest)
      }
  }

  // determine whether this state is a sink state, which means
  //  1) it is not a final state
  //  2) it has at least one transitions
  //  3) all of its transitions (normally only one) has a universal label
  //        e.g., STop
  //  4) all of its transitions (normally only one) is a self loop.
  lazy val isSinkState:Boolean = {
    !dfa.F.contains(this) &&
      transitions.nonEmpty &&
      transitions.forall { case Transition(src, label, dst) =>
        this == src &&
          this == dst &&
          dfa.labeler.universe == label
      }
  }

  override def toString:String = {
    s"q:$id"
  }
}
