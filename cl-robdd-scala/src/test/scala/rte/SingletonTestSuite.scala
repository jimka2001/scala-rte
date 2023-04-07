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

package rte

import genus._
import RandomType.randomType
import adjuvant.MyFunSuite

class SingletonTestSuite extends MyFunSuite {
  test("singleton canonicalizeOnce") {
    val x = SEql("x")
    val y = SEql("y")
    val xy = SMember("x", "y")
    val yz = SMember("y", "z")
    assert(Singleton(STop).canonicalizeOnce == Sigma)
    assert(Singleton(SEmpty).canonicalizeOnce == EmptySet)
    assert(Singleton(SAnd(x, y)).canonicalizeOnce == EmptySet)
    assert(Singleton(SAnd(SDouble,SInt)).canonicalizeOnce == And(Singleton(SDouble),Singleton(SInt)))
    assert(Singleton(SAnd(xy, yz)).canonicalizeOnce == Singleton(y))
    assert(Singleton(SOr(SDouble,SInt)).canonicalizeOnce == Or(Singleton(SDouble),Singleton(SInt)))
    assert(Singleton(SOr(xy, yz)).canonicalizeOnce == Singleton(SMember("x","y","z")))
    assert(Singleton(SNot(x)).canonicalizeOnce == And(Not(Singleton(x)), Sigma))
  }
  test("discovered case 43") {
    // <[SAnd [SAnd [SOr {a,b,c},{false,true},Class2X]],![SOr Trait1,Integer],![SOr {4,5,6},String]]>
    class Class2X
    trait Trait1
    val td = SAnd(SAnd(SOr(SMember("a", false),
                           SAtomic(classOf[Class2X]))),
                  SNot(SOr(SEql(4),
                           SAtomic(classOf[String]))))
    var rte: Rte = Singleton(td.canonicalize())
    for {_ <- 0 to 10} {
      rte = rte.canonicalizeOnce
    }
    rte.canonicalize
  }

  test("discovered case 59") {
    class Class2X
    trait Trait1
      //Or(And(<Class2X$1>,Not(<[SOr String,[= 4]]>)),And(<{false,a}>,Not(<[SOr String,[= 4]]>)))
    val c2x = Singleton(SAtomic(classOf[Class2X]))
    val S =   SAtomic(classOf[String])
    val rte2 = Or(And(c2x,
                      Not(Singleton(SOr(S,SEql(4))))),
                  And(Singleton(SMember(false,"a")),
                      Not(Singleton(SOr(S,SEql(4))))))
    rte2.canonicalizeOnce
  }

  test("discovered case 74"){
    // And: ArraySeq(({false,a}), Not(((SOr String,(= 4)))))
    val S =   SAtomic(classOf[String])
    val rte = And(Singleton(SMember(false,"a")),
                  Not(Singleton(SOr(S,SEql(4)))))
    rte.canonicalize
  }

  test("singleton  canonicalize") {

    for {depth <- 0 to 4
         _ <- 1 to num_random_tests
         td = randomType(depth)
         rt = Singleton(td)
         } {
      rt.canonicalize
    }
  }

}
