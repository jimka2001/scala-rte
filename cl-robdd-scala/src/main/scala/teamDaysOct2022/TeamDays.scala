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
import adjuvant.Adjuvant.{copyFile, existingFile}
import genus.SMember
import genus.Types.evenType
import xymbolyco.GraphViz.dfaView
import xymbolyco.Minimize



object TeamDays {
  def genSimpleSampleDfa(): Unit = {
    val rte: Rte = Cat(classOf[String],
                       Star(classOf[Int]),
                       classOf[String])

    val dfa1 = rte.toDfa()
    dfaView(dfa1, abbrev = true, title = "dfa", showSink = false)
  }

  abstract class JExpr

  abstract class JBinary extends JExpr
  final class JPlus extends JBinary
  final class JMinus extends JBinary

  abstract class JConst extends JExpr
  final class JNum extends JConst
  final class JStr extends JConst

  def genExprSampleDfa(): Unit = {
    val rte: Rte = Cat(classOf[JExpr],
                       Star(classOf[JBinary]),
                       classOf[JStr])

    val dfa1 = rte.toDfa()

    dfaView(dfa1, abbrev = true, title = "expr", showSink = false)
  }

  val dotDir = existingFile(Seq("/Users/jnewton/Repos/research/dot/"),
                            "/tmp/")
  def genExpr2SampleDfa(): Unit = {
    val rte: Rte = Cat( classOf[JMinus],
                        //Star(classOf[JMinus]),
                        Star(Or(classOf[JStr],
                                And(classOf[JExpr], Not(classOf[JMinus])),
                                classOf[JBinary])),
                        classOf[JBinary])

    val dfa = rte.toDfa()

    dfaView(dfa, abbrev = true, title = "expr", showSink = false,
            dotFileCB = (fn: String) => {
              copyFile(fn, dotDir + "team-days-oct-2022-expr.dot")
            })
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

    dfaView(Minimize.minimize(dfa3), abbrev = true, title = "minimized")
  }

  def genDistinguishFinalDfa() = {
    val rte1: Rte = Cat(classOf[String],
                        Star(classOf[Int]),
                        classOf[Boolean])
    val rte2: Rte = Cat(classOf[String],
                        Star(classOf[Int]),
                        classOf[String])

    val dfa1 = rte1.toDfa(exitValue = 42)
    val dfa2 = rte2.toDfa(exitValue = 13)

    val dfa3 = Rte.dfaUnion(dfa1, dfa2)
    dfaView(dfa3, abbrev = true, title = "distinguishable-output",
            dotFileCB = (fn: String) => {
              copyFile(fn, dotDir + "team-days-oct-2022-distinguishable-output.dot")
            })
  }

  def main(argv: Array[String]): Unit = {
    // DEMO
    // 1
    //genSimpleSampleDfa()

    // 2
    //genExprSampleDfa()

    // 3
    //genExpr2SampleDfa()

    // 4
    //genSampleDfa()



    //genDistinguishFinalDfa()
  }
}
