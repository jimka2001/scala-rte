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

package padl

import adjuvant.Adjuvant.{existingFile, filterFile}
import genus.{SAnd, SAtomic, SEmpty, SEql, SNot, SOr, SSatisfies, STop, SimpleTypeD}
import genus.Types.{evenType, oddType}
import rte.{And, Cat, Eql, Not, Or, Permute, Plus, Rte, Singleton, Star}
import xymbolyco.{Dfa, GraphViz, Thompson}
import xymbolyco.GraphViz.{dfaView, multiLineString}
import xymbolyco.Minimize.minimize


object Padl {
  import scala.language.implicitConversions
  import rte.RteImplicits._
  val dotDir:String = existingFile(Seq("/Users/jnewton/Repos/research/dot/",
                                       "/Users/jimka/Repos/research/dot/"),
                                   "dot-dir-not-found")

  def cpTo(fromName:String,baseName:String):Unit = {
    filterFile(fromName,
               dotDir + baseName + ".dot",
               keepIf= line=> !line.contains("labelloc=") && !line.contains("  label=") )
  }

  def tyint():SimpleTypeD = SAtomic(classOf[Int])
  def tystr():SimpleTypeD = SAtomic(classOf[String])
  def tynum():SimpleTypeD = SAtomic(classOf[Number])
  def tychar():SimpleTypeD = SAtomic(classOf[Char])
  def str():Rte = Singleton(tystr())
  def num():Rte = Singleton(tynum())
  def odd():Rte = Singleton(oddType())
  def even():Rte = Singleton(evenType())
  def integer():Rte = Singleton(tyint())
  def char():Rte = Singleton(tychar())
  def charx():Rte = classOf[Char]
  def givenLabels() = Seq[SimpleTypeD](SEmpty, // 0
                                     STop, // 1
                                     tyint(), // 2
                                     SNot(tyint()), // 3
                                     evenType(), // 4
                                     SAnd(tystr(), evenType()), // 5
                                     SAnd(SNot(tystr()), evenType()), // 6
                                     SAnd(tystr(), SNot(evenType())), // 7
                                     SOr(SAnd(tyint(), SNot(evenType())), // 8
                                         SAnd(tystr(), SNot(evenType()))),
                                     SAnd(SNot(tyint()), // 9
                                          SNot(tystr()),
                                          evenType()),
                                     SOr(SAnd(tyint(), evenType()), // 10
                                         SAnd(tystr(), evenType())),
                                     SAnd(SNot(tystr()), SNot(evenType())), // 11
                                     SAnd(SNot(tyint()), SNot(tystr()), SNot(evenType())) //12
                                     )

  def exampleCode():Unit = {
    import rte.RteImplicits._
    def isEven(x: Any): Boolean =
      x match {
        case y: Int => y % 2 == 0
        case _ => false
      }

    val even = SSatisfies(isEven, "even")
    val r_0 = Star(Cat(classOf[Integer], classOf[String], even))
    val r_1 = Star(Cat(classOf[Integer], Star(classOf[String]), even))
    val r_2 = Star(Cat(classOf[Integer], Plus(classOf[String]), even))

    val dfa_0: Dfa[Any, SimpleTypeD, Boolean] = r_0.toDfa()
    val dfa_1 = r_1.toDfa()
    val dfa_2 = r_2.toDfa()

    // returns Some(true)
    val v0 = dfa_0.simulate(Seq(1,"hello",2))
    // returns Some(true)
    val v1 = dfa_1.simulate(Seq(1,"hello","world",2,
                                3,4,
                                5,"hello",6))
    // returns None
    val v2 = dfa_2.simulate(Seq(1.1, 1.2, 1.3))
  }

  def example1(): Unit = {

    val rt1: Rte = Star(Cat(integer(), str(), even()))

    println("-----------------------")
    dfaView(rt1.toDfa(),
            abbrev = false,
            title = "rt1",
            label = Some(multiLineString(s"rt1=${rt1.toDot()}", 60)),
            showSink = false,
            dotFileCB = str => cpTo(str, "padl-example-1"),
            givenLabels = givenLabels(),
            printLatex = true)
  }

  def example2(): Unit = {
    val rt2: Rte = Plus(Cat(Or(classOf[Int], And(classOf[Char],
                                                 Not(SEql('z')))),
                            Star(classOf[String]),
                            even()))

    println("-----------------------")
    if(false) {
      dfaView(rt2.toDfa(),
              abbrev = true,
              title = "rt2-not-min",
              label = Some("Brz " + multiLineString(s"rt2=${rt2.toDot()}",
                                                    60)),
              showSink = false,
              dotFileCB = str => cpTo(str, "padl-not-min-example-2"),
              givenLabels = givenLabels(),
              printLatex = false)
    }
    for{ seq <- Vector(List(13,"hello","world", 14),
                       List(13,14),
                       List(12),
                       List(12,13),
                       List(1, "hello", "world", 14,
                            'x', "hello", 4),
                       List('z'),
                       List('z',16)
                       )} {
      println(seq)
      println("  ---> " + rt2.simulate(true,seq))
      println(Singleton(SAtomic(classOf[scala.Char])).simulate(true,Vector('a')))
    }

  }

  def example2min(): Unit = {
    val rt2: Rte = Star(Cat(integer(), Star(str()), even()))

    println("-----------------------")
    dfaView(minimize(rt2.toDfa()),
            abbrev = true,
            title = "rt2-min",
            label = Some("Brz min " + multiLineString(s"rt2=${rt2.toDot()}",
                                                      60)),
            showSink = false,
            dotFileCB = str => cpTo(str, "padl-min-example-2"),
            givenLabels = givenLabels(),
            printLatex = false)

  }

  def example3(): Unit = {
    val a:Rte = Eql("a")
    val b:Rte = Eql("b")
    val s:Rte = a++a | b++b

    dfaView(s.toDfa(), title="positive-s")
    dfaView(Not(s).toDfa(), title="not s")
  }

  def main(argv: Array[String]): Unit = {

    //example1()
    //example2()
    example3()
    //example2min()
  }

}
