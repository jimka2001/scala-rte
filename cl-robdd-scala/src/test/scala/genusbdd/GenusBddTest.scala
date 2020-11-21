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

package genusbdd

import bdd._
import genus._
import genus.Types._
import org.scalatest._

import scala.collection.{Seq, mutable}
import org.scalatest.funsuite.AnyFunSuite

class GenusBddTest extends AnyFunSuite {
  Bdd.withNewBddHash {
    trait Trait1
    trait Trait2
    trait Trait3 extends Trait2
    val tds = Seq(SEmpty,
                  STop,
                  SAtomic(classOf[Trait1]),
                  SNot(classOf[Trait1]),
                  SEql(42),
                  SMember(1, 2, 3),
                  SMember(1, 3, 2),
                  evenType, // SCustom(evenp)
                  SAnd(classOf[Trait1], classOf[Trait2]),
                  SAnd(classOf[Trait1], classOf[Trait2], classOf[Trait3]),
                  SOr(classOf[Trait1], classOf[Trait2]),
                  SOr(classOf[Trait1], classOf[Trait2], classOf[Trait3]))
    val tdToInt: mutable.Map[SimpleTypeD, Int] = mutable.Map[SimpleTypeD, Int]()
    tds.foreach { td => println(GenusBdd(td, tdToInt).bdd) }
    println(tdToInt)
    tds.foreach { td =>
      val dnf = GenusBdd(td, tdToInt).dnf
      val can = dnf.canonicalize()
      println() // blank
      println(s"td = $td")
      println(s"dnf           = $dnf")
      println(s"canonicalized = $can")

    }
  }
}
