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

package bdd

import scala.annotation.tailrec

case class Assignment(trueVariables:Set[Int]) {
  def value(v:Int):Boolean = {
    trueVariables.contains(v)
  }
}

object Assignment {
  def apply(bitMask: Long): Assignment = {
    @tailrec
    def recur(bitMask: Long, bit: Int, set: Set[Int]): Set[Int] = {
      if (0 == bitMask)
        set
      else if (0 == bitMask % 2)
        recur(bitMask / 2, bit + 1, set)
      else
        recur(bitMask / 2, bit + 1, set + bit)
    }

    Assignment(recur(bitMask, 1, Set()))
  }

  //  def toMinTerm(pair:(Assignment,Assignment)):String = {
  //    pair match {
  //      case (assignTrue,assignFalse) => toMinTerm(assignTrue,assignFalse)
  //    }
  //  }
  def toMinTerm(assignTrue: Assignment, assignFalse: Assignment): String = toMinTerm("x", assignTrue, assignFalse)

  def toMinTerm(v: String, assignTrue: Assignment, assignFalse: Assignment): String = {
    val trues = assignTrue.trueVariables.toList.sorted
    val falses = assignFalse.trueVariables.toList.sorted
    val digits = Array("\u2080", "\u2081", "\u2082", "\u2083", "\u2084",
                       "\u2085", "\u2086", "\u2087", "\u2088", "\u2089")

    @tailrec
    def subscript(n: Int, higher: List[String]): String = {
      require(n >= 0)
      if (n < 10)
        (digits(n) :: higher).mkString("")
      else
        subscript(n / 10, digits(n % 10) :: higher)
    }

    @tailrec
    def loop(trues: List[Int], falses: List[Int], literals: List[String]): String =
      (trues, falses) match {
        case (Nil, Nil) => literals.reverse.mkString("")
        case (x :: xs, Nil) => loop(xs, Nil, (v + subscript(x, List())) :: literals)
        case (Nil, y :: ys) => loop(Nil, ys, ("\u00AC" + v + subscript(y, List())) :: literals)
        case (x :: xs, y :: _) if x < y => loop(xs, falses, (v + subscript(x, List())) :: literals)
        case (x :: _, y :: ys) if x > y => loop(trues, ys, ("\u00AC" + v + subscript(y, List())) :: literals)
        case (x :: _, y :: _) if x == y => sys.error(s"trues=$trues and falses=$falses contain the same literal=$x")
      }

    loop(trues, falses, List())
  }

  def main(argv: Array[String]): Unit = {
    println(toMinTerm(Assignment(Set(1)), Assignment(Set[Int]())))
    println(toMinTerm(Assignment(Set[Int]()), Assignment(Set(2))))
    println(toMinTerm(Assignment(Set(1)), Assignment(Set(2))))
    println(toMinTerm(Assignment(Set(1, 3)), Assignment(Set(2))))
    println(toMinTerm(Assignment(Set(1, 3)), Assignment(Set(2, 5, 6))))
    println(toMinTerm(Assignment(Set(10, 312)), Assignment(Set(2, 5, 6))))
    ()
  }
}
