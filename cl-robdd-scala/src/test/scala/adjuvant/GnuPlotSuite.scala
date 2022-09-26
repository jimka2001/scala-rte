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

package adjuvant

import adjuvant.GnuPlot._

import scala.io.Source

class GnuPlotSuite extends MyFunSuite {
  
  test("test 1"){
    var calledCB = false
    val comment = "this is a test"
    def gnuFileCB(fname:String):Unit = {
      println(s"fname=$fname")
      val lines = Source.fromFile(fname).getLines().toList
      assert(lines.size > 0)
      assert(lines.find(s => s.contains(comment)).nonEmpty)
      calledCB = true
    }
    gnuPlot(dataToPlot = Seq(("curve1",
                               Seq(1.0, 2.0, 3, 4),
                               Seq(1.0, 2.0, 2.5, 2.75)),
                             ("curve2", Seq((1.0, 2.0),
                                            (2, 2.25),
                                            (3, 2.125),
                                            (4, 2.012)))))(
      gnuFileCB = gnuFileCB,
      comment = comment,
      view = false)
    assert(calledCB)
  }
}


