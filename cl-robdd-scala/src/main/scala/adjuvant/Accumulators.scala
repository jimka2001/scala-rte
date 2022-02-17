// Copyright (c) 2019,20 EPITA Research and Development Laboratory
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

case class successor[A](init:A){
  var current:A = init

  def apply(f:A=>A):A = {
    current = f(current)
    current
  }
}

object Accumulators {
  def withCollector[A](f: (A => Unit) => Unit): List[A] = {
    var objects: List[A] = Nil

    def collect(obj: A): Unit = {
      objects = obj :: objects
    }

    f(collect)
    objects.reverse
  }

  def withSetCollector[A](f: (A => Unit) => Unit): Set[A] = {
    // Thanks to Claudio Bley for this implementation.
    // https://users.scala-lang.org/t/optional-parens/6938/11?u=jimka
    val objects = collection.mutable.Set[A]()

    f(objects.add)

    objects.toSet
  }

  def withNconc[A](f: (List[A] => Unit) => Unit): List[A] = {
    withCollector[List[A]] { nconc =>
      f(nconc)
    }.flatten
  }


  def withReducer[A](init:A,folder:(A,A)=>A)(f:(A=>Unit)=>Unit):A = {
    var acc = init
    def reduce(obj:A):Unit = {
      acc = folder(acc,obj)
    }
    f(reduce)
    acc
  }

  def withMaximizer[A](init: A)(f: (A => Unit) => Unit)(implicit ev: A => Ordered[A]): A = {
    withReducer(init, (a:A,b:A)=>if (a < b) b else a)(f)
  }

  def withMinimizer[A](init: A)(f: (A => Unit) => Unit)(implicit ev: A => Ordered[A]): A = {
    withReducer(init, (a:A,b:A)=>if (a > b) b else a)(f)
  }

  def withSummer[A](init:A,plus:(A,A)=>A)(f:(A=>Unit)=>Unit): A = {
    withReducer(init,plus)(f)
  }

  def withCounter(f:(()=>Unit)=>Unit):Int = {
    var i = 0
    def reduce():Unit = {
      i = i+1
    }
    f(reduce)
    i
  }

  def withOutputToString(f:(String=>Unit)=>Unit):String = {
    withOutputToString(f, java.nio.charset.StandardCharsets.UTF_8)
  }

  def withOutputToString(f:(String=>Unit)=>Unit,
                         encoding:java.nio.charset.Charset):String = {
    import java.io.{ByteArrayOutputStream, PrintWriter}
    val baos = new ByteArrayOutputStream()
    val pw = new PrintWriter(baos)
    var str = ""

    def printer(str: String): Unit = {
      pw.print(str)
    }

    try {
      f(printer)
      pw.flush()
      str = new String(baos.toByteArray, encoding)
    } finally pw.close()
    str
  }

  def makeCounter(incr:Int= 1):()=>Int = {
    var c = 0
    def count():Int = {
      c = c + incr
      c
    }
    count
  }

  def main(argv: Array[String]): Unit = {

    import java.io.{ByteArrayOutputStream, PrintWriter}
    import java.nio.charset.StandardCharsets

    def printToOutputString(f: PrintWriter => Unit): String = {
      val baos = new ByteArrayOutputStream()
      val pw = new PrintWriter(baos)
      try {
        f(pw)
        pw.flush()
        new String(baos.toByteArray, StandardCharsets.UTF_8)
      } finally pw.close()
    }

    val helloWorld = printToOutputString { pw =>
      pw.print("hello ")
      pw.print("world")
    }
    println(s"helloWorld=$helloWorld")
  }
}