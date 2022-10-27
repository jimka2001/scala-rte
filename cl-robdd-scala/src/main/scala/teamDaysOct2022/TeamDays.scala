// Copyright (©) 2022 EPITA Research and Development Laboratory
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

package teamDaysOct2022

import rte._
import RteImplicits._
import genus.SMember
import genus.Types.evenType
import xymbolyco.GraphViz.dfaView



object TeamDays {
  def genSimpleSampleDfa(): Unit = {
    val rte: Rte = Cat(classOf[String],
                       Star(classOf[Int]),
                       classOf[String])

    val dfa1 = rte.toDfa()
    dfaView(dfa1, abbrev = true, title = "dfa", showSink = false)
  }

  abstract class Expr
  abstract class Binary extends Expr
  final class Plus extends Binary
  final class Minus extends Binary
  abstract class Const extends Expr
  final class Num extends Const
  final class Str extends Const

  def genExprSampleDfa(): Unit = {
    val rte: Rte = Cat(classOf[Expr],
                       Star(classOf[Binary]),
                       classOf[Str])

    val dfa1 = rte.toDfa()

    dfaView(dfa1, abbrev = true, title = "expr", showSink = false)
  }

  def genExpr2SampleDfa(): Unit = {
    val rte: Rte = Cat(classOf[Minus],
                       //Star(classOf[Minus]),
                       Star(Or(classOf[Str],
                               And(classOf[Expr], Not(classOf[Minus])),
                               classOf[Binary])),
                       classOf[Binary])

    val dfa = rte.toDfa()

    dfaView(dfa, abbrev = true, title = "expr", showSink = false)
  }
  
  def genSampleDfa(): Unit = {
    val rte1: Rte = Cat(classOf[String],
                        Star(classOf[Int]),
                        Or(classOf[Boolean],
                           Star(evenType)))
    val rte2: Rte = Cat(classOf[String],
                        Star(classOf[Int]),
                        // SMember(1,0,-1),
                        classOf[String])

    val dfa1 = rte1.toDfa(exitValue = 42)
    val dfa2 = rte2.toDfa(exitValue = 13)

    dfaView(dfa1, abbrev = true, title = "dfa1")
    dfaView(dfa2, abbrev = true, title = "dfa2")

    val dfa3 = Rte.dfaXor(dfa1, dfa2)
    dfaView(dfa3, abbrev = true, title = "xor")

  }

  def main(argv: Array[String]): Unit = {
    //genSimpleSampleDfa()
    //genExprSampleDfa()
    genExpr2SampleDfa()
    //genSampleDfa()
  }
}
