// Copyright (c) 2021,22 EPITA Research and Development Laboratory
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
import genus.{SAnd, SEmpty, SNot, STop, SimpleTypeD}

sealed abstract class IfThenElseTree[E] {
  def apply(a:Any):Option[State[Any,SimpleTypeD,E]]
}

object IfThenElseTree {
  def apply[E](tds:List[SimpleTypeD],
               transitions:Set[Transition[Any, SimpleTypeD, E]]):IfThenElseTree[E] = {
    if (transitions.isEmpty)
      IfThenElseFalse[E]()
    else if (tds.isEmpty) {
      // should have exactly one transition
      assert(transitions.size == 1, s"too many transitions, expecting exactly 1: $transitions")
      val Transition(_,_,dest) = transitions.head
      IfThenElseTrue[E](dest)
    } else
      IfThenElseNode[E](tds,transitions)
  }
}

case class IfThenElseTrue[E](dest:State[Any,SimpleTypeD,E]) extends IfThenElseTree[E] {
  def apply(a:Any):Option[State[Any,SimpleTypeD,E]] = Some(dest)
}

case class IfThenElseFalse[E]() extends IfThenElseTree[E] {
  def apply(a:Any):Option[State[Any,SimpleTypeD,E]] = None
}

case class IfThenElseNode[E](tds:List[SimpleTypeD], transitions:Set[Transition[Any, SimpleTypeD, E]])
  extends IfThenElseTree[E] {
  val tdh::tdt = tds
  def reduceTransitions(search: SimpleTypeD, intersect:SimpleTypeD, replace: SimpleTypeD): Set[Transition[Any, SimpleTypeD, E]] = {
    transitions.flatMap { case Transition(src, td, dest) =>
      val simpler = SAnd(td,intersect)
        .canonicalize()
        .searchReplace(search, replace)
        .canonicalize()
      if (simpler.inhabited.contains(false))
        Set()
      else
        Set(Transition(src, simpler, dest))
    }
  }

  // ifTrue and ifFalse are lazy.
  // This has the effect that only the part of the tree that is actually
  //   walked, gets expanded, and only once.  If it is walked again, the
  //   expansion has already happened.
  lazy val ifTrue:IfThenElseTree[E] = IfThenElseTree[E](tdt, reduceTransitions(tdh, tdh, STop))
  lazy val ifFalse:IfThenElseTree[E] = IfThenElseTree[E](tdt, reduceTransitions(tdh, SNot(tdh), SEmpty))

  def apply(a: Any): Option[State[Any,SimpleTypeD,E]] = {
    if (tdh.typep(a))
      ifTrue(a)
    else
      ifFalse(a)
  }
}

