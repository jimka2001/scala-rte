// Copyright (©) 2023 EPITA Research and Development Laboratory
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

package psug

import genus._
import rte._
import xymbolyco.GraphViz.{dfaView, multiLineString}


object Psug {
  import rte.RteImplicits._

  import scala.language.implicitConversions

  def isEven(x: Any): Boolean = {
    x match {
      case y: Int => y % 2 == 0
      case _ => false
    }
  }

  val even: SSatisfies = SSatisfies(isEven, "even")

  def example(): Unit = {
    val rt2: Rte = Plus(Cat(Or(classOf[Int], And(classOf[Char],
                                                 Not(SEql('z')))),
                            Star(classOf[String]),
                            even))

    if(false) {
      dfaView(rt2.toDfa(),
              abbrev = true,
              title = "rt2-not-min",
              label = Some("Brz " + multiLineString(s"rt2=${rt2.toDot()}",
                                                    60)),
              showSink = false)
    }
    for{ seq <- Vector(List(13,"hello","world", 14),
                       List(13,14),
                       List(12),
                       List(12,13),
                       List(1, "hello", "world", 14,
                            'x', "hello", 4),
                       List('z'),
                       List('z',16),
                       List('z',17)
                       )} {
      println(seq)
      println("  ---> " + rt2.simulate(true,seq))
    }
  }

  def main(argv: Array[String]): Unit = {
    example()
  }

}
