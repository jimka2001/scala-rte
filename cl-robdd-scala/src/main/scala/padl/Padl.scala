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
import genus.SAtomic
import genus.Types.{evenType, oddType}
import rte.{Cat, Or, Rte, Singleton, Star}
import xymbolyco.GraphViz
import xymbolyco.GraphViz.dfaView


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
    dfaView(rt1.toDfa(), abbrev = false, title = "rt1", showSink=false,
            dotFileCB= str=>cpTo(str,"padl-example-1"))
  }

  def main(argv: Array[String]): Unit = {
    example1()
  }

}
