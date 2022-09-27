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

import adjuvant.Adjuvant.filterFile
import genus.{SAtomic, SEmpty, STop, SimpleTypeD}
import genus.Types.{evenType, oddType}
import rte.{Cat, Or, Permute, Plus, Rte, Singleton, Star}
import xymbolyco.{GraphViz, Thompson}
import xymbolyco.GraphViz.{dfaView, multiLineString}
import xymbolyco.Minimize.minimize


object Padl {
  val dotDir = "/Users/jnewton/Repos/research/dot/"
  def cpTo(fromName:String,baseName:String):Unit = {
    filterFile(fromName,
               dotDir + baseName + ".dot",
               keepIf= line=> !line.contains("labelloc=") && !line.contains("  label=") )
  }
  def example1():Unit = {
    val str = Singleton(SAtomic(classOf[String]))
    val num = Singleton(SAtomic(classOf[Number]))
    val odd = Singleton(oddType)
    val even = Singleton(evenType)
    val integer = Singleton(SAtomic(classOf[Int]))
    val rt1: Rte = Star(Cat(integer, str, even))
    val rt2: Rte = Star(Cat(integer, Plus(str), even))
    val rt3: Rte = Permute(str,even,integer)

    val givenLabels=Seq[SimpleTypeD](SEmpty,
                                     STop,
                                     SAtomic(classOf[Int]),
                                     evenType,
                                     SAtomic(classOf[String]))
    dfaView(rt1.toDfa(),
            abbrev = true,
            title = "rt1",
            label = Some(multiLineString(s"rt1=${rt1.toDot}",60)),
            showSink = false,
            dotFileCB = str => cpTo(str, "padl-example-1"),
            givenLabels=givenLabels)
    dfaView(minimize(Thompson.constructThompsonDfa(rt2,true)),
            abbrev = true,
            label = Some("Thompson " + multiLineString(s"rt2=${rt2.toDot}",
                                                       60)),
            title = "rt2",
            showSink = false,
            dotFileCB = str => cpTo(str, "padl-thompson-example-2"),
            givenLabels=givenLabels)
    dfaView(minimize(rt2.toDfa()),
            abbrev = true,
            title = "rt2",
            label = Some("Brz min " + multiLineString(s"rt2=${rt2.toDot}",
                                                      60)),
            showSink = false,
            dotFileCB = str => cpTo(str, "padl-example-2"),
            givenLabels = givenLabels)

  }

  def main(argv: Array[String]): Unit = {
    example1()
  }

}
