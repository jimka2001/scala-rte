// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package lbdd.benchmarks

import bdd.Bdd
import lbdd.LBdd


object Targets {

  // TODO : def bddSamples(n: Int, f: (Int, Bdd) => Bdd): Bdd (check several methods)

  def bddSamples(n: Int): Any = {
    Bdd.withNewBddHash {
      @scala.annotation.tailrec
      def construct(k: Int, b: Bdd): Bdd = {
        k match {
          case 0 => b
          case i if i > 0 => construct(i - 1, bdd.Or(i, b))
        }
      }
      println("Coucou je suis un BDD")
      construct(n - 1, Bdd(n)).findSatisfyingAssignment()
    }
  }


  // TODO : def lazyBddSamples(n: Int, f: (Int, LBdd) => LBdd): LBdd (check several methods)

  def lazyBddSamples(n: Int): Any = {
    @scala.annotation.tailrec
    def construct(k: Int, b: LBdd): LBdd = {
      k match {
        case 0 => b
        case i if i > 0 => construct(i - 1, lbdd.Or(i, b))
      }
    }
    println("Coucou je suis un Lazy BDD")
    construct(n - 1, LBdd(n)).findSatisfyingAssignment()
  }
}
