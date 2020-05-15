// Copyright (c) 2020 EPITA Research and Development Laboratory
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

package lbdd.benchmarks.scalameter.benchmark

import java.io.File

import lbdd.benchmarks.Targets._
import org.scalameter.persistence.SerializationPersistor
import org.scalameter.{Bench, Gen}


object PerformanceBenchmark extends Bench.OfflineReport {

  override lazy val persistor = SerializationPersistor(new File("target/scalameter/performance/results"))

  val nbGen: Gen[Int] = Gen.range("nbVariables")(1, 5000,20)

  performance of "inputs" in {
    measure method "bddSamples" in {
      using(nbGen) in {
        n => bddSamples(n)
      }
    }
  }

  performance of "inputs" in {
    measure method "lazyBddSamples" in {
      using(nbGen) in {
        n => lazyBddSamples(n)
      }
    }
  }
}
