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
import genus.SyntaxSugar.SAndNot
import genus.{SAnd, SEmpty, SNot, STop, SimpleTypeD}

// an IfThenElseTree is a lazy tree which is used to figure out given
// an object, which transition in a Dfa to select.
// A state in a Dfa has a set of Transition objects which are
// assumed to be labeled by disjoint types (SimpleTypeD)
// Calling the apply(a:Any) method on an IfThenElseTree object will return
// None if the object `a` satisfies none of the transitions.
// Some(dst) if a matches the Transition(_,td,dst).
// However, as explained above, this check is lazy.
// The tree has been initialized, with a call to IfThenElse(...)
// with a list of types which indicate all the leaf-level types
// in the set of transitions.
// The object, `a`, is checked against each SimpleTypeD as td.typep(a)
// to return either true or false. The list of transitions is
// then filtered by intersection td (in the case of true) or SNot(td)
// in the case of false.  Any transition which becomes unsatisfiable
// is eliminated, and some (hopefully most) of the type designators
// become simpler.
// This filtering either continues until there are no transitions remaining,
// in which case None is returned, or a single transition remains whose type
// is STop,  in which case indicated destination state is returned, wrapped
// in Some(dst).
// Usually when this code is used, E=Any, and S=State[Any, SimpleTypeD, E];
// however, S has been parameterized to make the testing easier.  Test cases
// are written with a simpler S, such as Int
sealed abstract class IfThenElseTree[E,S] {
  def apply(a:Any):Option[S]

}

object IfThenElseTree {
  def apply[E,S](tds:List[SimpleTypeD], transitions:Set[(SimpleTypeD, S)]):IfThenElseTree[E,S] = {
    if (transitions.isEmpty)
      IfThenElseMissing[E,S]()
    else if (transitions.head._1 == STop) {
      val (_,dest) = transitions.head
      IfThenElseFound[E,S](dest)
    } else if (tds.isEmpty) {
      // should have exactly one transition
      assert(transitions.size == 1, s"too many transitions, expecting exactly 1: $transitions")
      val (_,dest) = transitions.head
      IfThenElseFound[E,S](dest)
    }
    else
      IfThenElseNode[E,S](tds,transitions)
  }
}

case class IfThenElseFound[E,S](dest:S) extends IfThenElseTree[E,S] {
  def apply(a:Any):Option[S] = Some(dest)
  override def toString():String = dest.toString()
}

case class IfThenElseMissing[E,S]() extends IfThenElseTree[E,S] {
  def apply(a:Any):Option[S] = None
  override def toString():String = "missing"
}

case class IfThenElseNode[E,S](tds:List[SimpleTypeD], transitions:Set[(SimpleTypeD, S)])
  extends IfThenElseTree[E,S] {
  val tdh::tdt = tds.filter(td1 => transitions.exists{case (td2,_) => td2.leafTypes().contains(td1)})
  def reduceTransitions(intersect: SimpleTypeD, search:SimpleTypeD, replace: SimpleTypeD): Set[(SimpleTypeD, S)] = {
    transitions.flatMap { case (td, dest) =>
      val simpler = SAnd(td, intersect)
        .canonicalize()
        .searchReplaceInType(search, replace)
        .canonicalize()
      if (simpler.inhabited.contains(false))
        Set()
      else
        Set((simpler, dest))
    }
  }

  var ifTrueEvaluated:Boolean = false // this is here just for testing
  var ifFalseEvaluated:Boolean = false // it would be nice to eliminate if the test could figure out whether or not a lazy val has been evaluated

  override def toString():String = {
    s"Ite($tdh, $transitions, " +
      (if (ifTrueEvaluated) ifTrue.toString() else "_") +
      ", " +
      (if (ifFalseEvaluated) ifFalse.toString() else "_") +
      ")"
  }

  // ifTrue and ifFalse are lazy.
  // This has the effect that only the part of the tree that is actually
  //   walked, gets expanded, and only once.  If it is walked again, the
  //   expansion has already happened.
  lazy val ifTrue:IfThenElseTree[E,S] = locally{
    ifTrueEvaluated = true
    // at this point, in the tree, we have assured that the object is of type tdh,
    //   we still need to check all of tdt.
    // For example, if we have already determined that the object is an Int,
    //   then we don't need to further check whether it is a Number
    IfThenElseTree[E,S](tdt, reduceTransitions(tdh, tdh, STop))
  }
  lazy val ifFalse:IfThenElseTree[E,S] = locally{
    ifFalseEvaluated = true
    IfThenElseTree[E,S](tdt, reduceTransitions(SNot(tdh), tdh, SEmpty))
  }

  def apply(a: Any): Option[S] = {
    if (tdh.typep(a))
      ifTrue(a)
    else
      ifFalse(a)
  }
}

