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
import adjuvant.Adjuvant.printTime
import org.scalatest.funsuite.AnyFunSuite


class AdjFunSuite extends AnyFunSuite {
  import org.scalactic.source
  import org.scalatest.Tag

  val num_random_tests:Int = sys.env.get("CI_REGISTRY_IMAGE") match {
    case None => 1000   // if interactive
    case Some(_) => 100 // if in ci/cd pipeline
  }

  override def test(testName: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position):Unit = {
    super.test(testName,testTags : _*)(locally{
      import java.time.{Duration, LocalDateTime}
      val start = LocalDateTime.now()
      println("[ starting " + testName
                + " at : " + start
                + " total time: " + AdjFunSuite.elapsed())
      var finished = false
      try{
        testFun
        finished = true
      }
      finally{
        val endLocalTime = LocalDateTime.now()
        if (finished) {
          println("] finished " + testName
                    + " at : " + endLocalTime
            + " test time: " + Duration.between(start,endLocalTime)
            + " total time: " + AdjFunSuite.elapsed())

          println("current time = " + endLocalTime)
          println("time started = " + AdjFunSuite.testsStartedTime)
        }
        else
          println("] aborted "  + testName
                    + " at : " + endLocalTime
            + " test time: " + Duration.between(start,endLocalTime)
            + " total time: " + AdjFunSuite.elapsed())
      }
    })(pos)
  }
}

object AdjFunSuite {

  import java.time.{Duration, LocalDateTime}
  val testsStartedTime: LocalDateTime = LocalDateTime.now()

  def elapsed():Duration = Duration.between(testsStartedTime, LocalDateTime.now())
}