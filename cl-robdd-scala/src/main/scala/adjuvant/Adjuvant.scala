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


package adjuvant

import scala.annotation.tailrec

object Adjuvant {
  def conj[T](seq: Seq[T], obj: T): Seq[T] = seq match {
    // inspired by the Clojure conj function
    // See https://clojuredocs.org/clojure.core/conj
    // given an object ane a sequence, add the element to the sequence
    // either at the beginning or end, depending on the type of Seq.
    // It is easier to add to the begging of a list, but to the end of a vector.
    // The caller calls this function when it is not important whether the
    // new element be added to the beginning or the end.
    case Seq() => Seq(obj)
    case l: List[T] => obj :: l
    case _ => seq :+ obj
  }

  def traceGraph[V, W](v0: V, edges: V => Seq[(W, V)]): (Map[Int, V], Map[Int, Seq[(W, Int)]]) = {
    val s0: Seq[(W, Int)] = Seq()

    @tailrec
    def recur(v: V,
              i: Int,
              es: List[(W, V)],
              intToV: Map[Int, V],
              vToInt: Map[V, Int],
              m: Map[Int, Seq[(W, Int)]]): (Map[Int, V], Map[Int, Seq[(W, Int)]]) = {
      es match {
        case (w, v1) :: wvs if !vToInt.contains(v1) =>
          recur(v, i + 1, es, intToV + (i -> v1), vToInt + (v1 -> i), m)
        case (w, v1) :: wvs =>
          val currentEdges: Seq[(W, Int)] = m.getOrElse(vToInt(v), s0)
          recur(v, i, wvs, intToV, vToInt, m + (vToInt(v) -> conj(currentEdges, w -> vToInt(v1))))
        case Nil =>
          intToV.keys.find(k => !m.contains(k)).map(intToV) match {
            case Some(v2) => recur(v2, i, edges(v2).toList, intToV, vToInt, m)
            case None => (intToV, m)
          }
      }
    }

    recur(v0, 1, edges(v0).toList, Map(0 -> v0), Map(v0 -> 0), Map())
  }

  def searchReplace[A](xs: Seq[A], search: A, replace: A): Seq[A] = {
    xs.map(x => if (search == x) replace else x)
  }

  def findAdjacent[A](xs: Seq[A], cmp: (A, A) => Boolean): Boolean =
    xs.toList.tails.exists {
      case Nil => false
      case x :: Nil => false
      case x1 :: x2 :: _ => cmp(x1, x2)
    }

  def removeAdjacent[A](xs: Seq[A], cmp: (A, A) => Boolean): Seq[A] =
    xs.toList.tails.flatMap {
      case Nil => Nil
      case x :: Nil => List(x)
      case x1 :: x2 :: _ if cmp(x1, x2) => Nil
      case x :: _ => List(x)
    }.toSeq

  def fixedPoint[V](value: V, f: V => V, cmp: (V, V) => Boolean): V = {
    @tailrec
    def recur(value: V): V = {
      val newValue = f(value)
      if (cmp(value, newValue))
        value
      else
        recur(newValue)
    }

    recur(value)
  }

  // The memoize method is inspired by
  //  https://clojuredocs.org/clojure.core/memoize
  // Returning a memoized version of a referentially transparent function. The
  // memoized version of the function keeps a cache of the mapping from arguments
  // to results and, when calls with the same arguments are repeated often, has
  // higher performance at the expense of higher memory use.
  def memoize[F, T](f: F => T): F => T = {
    val hash = scala.collection.mutable.Map[F, T]()

    def mem(i: F): T = {
      hash.getOrElse(i, locally {
        val v: T = f(i)
        hash(i) = v
        v
      })
    }

    mem
  }

  def partitionBy[S, T](domain: Set[S], f: S => T): Set[Set[S]] = {
    if (domain.size == 1)
      Set(domain)
    else
      domain.groupBy(f).values.toSet
  }

  @tailrec
  def findSimplifier[T](target: T, simplifiers: List[() => T]): T = {
    // simplifiers is a list of 0-ary functions.   calling such a function
    //   either returns `this` or something else.   we call all the functions
    //   in turn, as long as they return `this`.  As soon as such a function
    //   returns something other than `this`, then that new value is returned
    //   from findSimplifier.  As a last resort, `this` is returned.
    simplifiers match {
      case Nil => target
      case s :: ss =>
        val t2 = s()
        if (target == t2)
          findSimplifier(target, ss)
        else
          t2
    }
  }
}
