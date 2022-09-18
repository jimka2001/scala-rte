// Copyright (c) 2021 EPITA Research and Development Laboratory
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
import org.scalatest.funsuite.AnyFunSuite
class MyFunSuite extends AnyFunSuite {
  import org.scalactic.source
  import org.scalatest.Tag
  def printTime(time:Long):String = {
    var acc = time
    val ns = acc % 1000
    acc = acc / 1000
    val us = acc % 1000
    acc = acc / 1000
    val ms = acc % 1000
    acc = acc / 1000
    val sec = acc % 60
    acc = acc / 60
    val min = acc % 60
    val hour = acc / 60
    
    if (hour > 0)
      s"$hour hours $min min $sec sec"
    else if (min > 0)
      s"$min min $sec sec"
    else if (sec > 0)
      s"$sec sec $ms ms"
    else if (ms > 0)
      s"$ms ms"
    else if (us > 0)
      s"$us us"
    else
      s"$ns ns"
  }
  override def test(testName: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position):Unit = {
    super.test(testName,testTags : _*)(locally{
      val start = System.nanoTime()
      println("[ starting " + testName)
      var finished = false
      try{
        testFun
        finished = true
      }
      finally{
        val end = System.nanoTime()
        if (finished)
          println("] finished " + testName + " " + printTime(end-start))
        else
          println("] aborted " + testName + " " + printTime(end - start))
      }
    })(pos)
  }
}
