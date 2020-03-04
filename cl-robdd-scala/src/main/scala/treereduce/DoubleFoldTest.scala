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
//


package treereduce

object DoubleFoldTest {

  def rationalFoldTest():Unit = {

    import treereduce.TreeParallelReduce._
    import treereduce.TreeReduce._
    import treereduce.TreeReducible._

    def term(n:Double):Double = {
      (2*n/(2*n-1)) * (2*n/(2*n+1))
    }

    val interval = (1 to 100).toList
    val p1 = interval.treeMapReduce(1.0)(_.toDouble,(acc:Double,n:Double) => {
      println(s"n=$n  acc=$acc")
      acc * term(n)
    })
    val p2 = pairMapReduce(interval)(1.0, _.toDouble, (acc:Double,n:Double) => acc * term(n))
    val p3 = interval.foldLeft(1.0){ (acc:Double,n:Int) => {
      //println(s"n=$n  acc=$acc")
      acc * term(n.toDouble)}}

    println(List(p1*2,p2*2,p3*2))
  }

  def main(argv:Array[String]):Unit = {
    rationalFoldTest()
  }
}
