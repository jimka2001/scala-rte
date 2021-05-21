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


package genus

import genus.Types._
import genus.NormalForm._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class GenusCanonicalize extends AnyFunSuite {

  trait TraitA

  trait TraitB

  trait TraitC

  trait TraitD
  class Test1 extends TraitA with TraitB with TraitC with TraitD

  val A: Class[TraitA] = classOf[TraitA]
  val B: Class[TraitB] = classOf[TraitB]
  val C: Class[TraitC] = classOf[TraitC]
  val D: Class[TraitD] = classOf[TraitD]
  test("and canonicalize") {
    // (and A EmptyType) -> EmptyType
    assert(SAnd(SAtomic(A), SEmpty, SAtomic(B)).canonicalize()
           == SEmpty)
    assert(SAnd().canonicalize()
           == STop)
    // (and A A B) --> (and A B)
    assert(SAnd(SAtomic(A),
                SAtomic(A),
                SAtomic(B)).canonicalize()
           == SAnd(SAtomic(A),
                   SAtomic(B)))

    // IntersectionType(EqlType(42), AtomicType(Integer)) --> EqlType(42)
    assert(SAnd(SEql(42), SAtomic(Types.Integer)).canonicalize()
           == SEql(42))

    // IntersectionType(MemberType(42,43,"hello"), AtomicType(Integer)) --> MemberType(42,43)
    assert(SAnd(SMember(42, 43, "hello"), SAtomic(Types.Integer)).canonicalize()
           == SMember(42, 43))

    // IntersectionType(A,TopType,B) ==> IntersectionType(A,B)
    assert(SAnd(SAtomic(A), STop, SAtomic(B)).canonicalize()
           == SAnd(SAtomic(A), SAtomic(B)))
  }
  test("and canonicalize 2") {
    assert(SMember("1", "2", "3", "4").typep("2"))
    assert(SAtomic(Types.String).typep("2"))
    assert(SNot(SMember("1", "2", "3", "4")).typep("5"))
    assert(SAnd(SAtomic(Types.String), SMember("1", "2", "3", "4")).typep("2"))
    // (and Int (not (member 1 2)) (not (member 3 4)))
    //  --> (and Int (not (member 1 2 3 4)))
    assert(SAnd(SAtomic(Types.String),
                SNot(SMember("1", "2")),
                SNot(SMember("3", "4"))).canonicalize()
           == SAnd(SAtomic(Types.String),
                   SNot(SMember("1", "2", "3", "4")),
                   )
           )
    assert(SAnd(SAtomic(Types.String),
                SNot(SMember("1", 2)),
                SNot(SMember("3", 4))).canonicalize()
           == SAnd(SAtomic(Types.String),
                   SNot(SMember("1", "3"))
                   )
           )
  }
  test("(and (and A B) (and C D)) -> (and A B C D)") {
    // (and (and A B) (and C D)) -> (and A B C D)
    assert(SAnd(SAnd(SAtomic(A),
                     SAtomic(B)),
                SAnd(SAtomic(C),
                     SAtomic(D))).canonicalize()
           == SAnd(SAtomic(A),
                    SAtomic(B),
                    SAtomic(C),
                    SAtomic(D)))
  }
  test("canonicalize children of and") {
    assert(SAnd(SMember("1")).canonicalize()
           == SEql("1"))
  }
  test("canonicalize member") {
    assert(SMember().canonicalize()
           == SEmpty)
    assert(SMember("hello").canonicalize()
           == SEql("hello"))
    assert(SMember("hello").canonicalize()
           != SEql("world"))
    assert(SMember("hello", "world").canonicalize()
           == SMember("hello", "world"))
    assert(SMember("hello", "world", "world", "hello").canonicalize()
           == SMember("hello", "world"))
  }

  test("canonicalize or") {
    assert(SOr(SAtomic(A),
               SEmpty,
               SAtomic(B)).canonicalize()
           == SOr(SAtomic(A),
                  SAtomic(B)))
    assert(SOr(SAtomic(A),
               SMember(),
               SAtomic(B)).canonicalize()
           == SOr(SAtomic(A), SAtomic(B)))
    assert(SOr().canonicalize() == SEmpty)
    assert(SOr(SAtomic(A)).canonicalize()
           == SAtomic(A))
    assert(SOr(SAtomic(A),
               SAtomic(A),
               SAtomic(B),
               SAtomic(A)).canonicalize()
           == SOr(SAtomic(A), SAtomic(B)))
    assert(SOr(SAtomic(A), STop).canonicalize()
           == STop)
    assert(SOr(SAtomic(A),
               SMember(1, 2, 3),
               SMember(3, 4, 5)).canonicalize()
           == SOr(SAtomic(A),
                  SMember(1, 2, 3, 4, 5)))
    // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))
    assert(SOr(SAtomic(Types.String),
               SMember(1, 2, "hello"),
               SMember(2, 3, 4, "world")).canonicalize()
           == SOr(SAtomic(Types.String),
                  SMember(1, 2, 3, 4)
                  ))
    // (or (or A B) (or C D)) --> (or A B C D)
    assert(SOr(SOr(SAtomic(A), SAtomic(B)),
               SOr(SAtomic(C), SAtomic(D))).canonicalize()
           == SOr(SAtomic(A), SAtomic(B), SAtomic(C), SAtomic(D)))
  }
  test("canonicalize or 156"){
    assert(SOr(SEql(1),SAtomic(classOf[java.lang.Integer])).canonicalize() ==
             SAtomic(classOf[java.lang.Integer]))
    assert(SOr(SEql(1),SAnd(SAtomic(classOf[java.lang.Integer]),SNot(SEql(1)))).canonicalize() ==
             SAtomic(classOf[java.lang.Integer]))
  }
  test("canonicalize and 162"){
    trait Trait1
    assert( SAnd(SAtomic(classOf[java.lang.Number]),SNot(SAtomic(classOf[Trait1]))).canonicalize() ==
              SAtomic(classOf[java.lang.Number]))
  }
  test("canonicalize or 2") {
    assert(SOr(A, SNot(A)).canonicalize()
           == STop)
    // (or A (and (not A) B) ==> (or A B)
    assert(SOr(A, SAnd(!A, B)).canonicalize()
           == SOr(A, B))
    // (or A (and A B)) ==> A
    assert(SOr(A, A && B).canonicalize()
           == SAtomic(A))
    // (or A (and (not A) B C) D) --> (or A (and B C) D)
    assert(SOr(A, SAnd(!A, B, C), D).canonicalize()
           == SOr(B && C, A, D))
    // (or A (and A B C) D) --> (or A D)
    assert(SOr(A, SAnd(A, B, C), D).canonicalize()
           == SOr(A, D))

    // (or X (not Y)) --> Top   if Y is subtype of X
    abstract class X
    class Y extends X
    assert(SOr(classOf[X], SNot(classOf[Y])).canonicalize()
           == STop)
    assert(SOr(classOf[Y], SNot(classOf[X])).canonicalize()
           == SOr(classOf[Y], SNot(classOf[X])))

    // AXBC + !X = ABC + !X
    assert(SOr(SAnd(A, classOf[X], B, C), SNot(classOf[X])).canonicalize()
           == SOr(SAnd(A, B, C), SNot(classOf[X])).canonicalize())


    assert(SEql(1).subtypep(classOf[java.lang.Integer]).contains(true))
    assert(SOr(classOf[java.lang.Integer], SNot(SEql(1))).canonicalize()
           == STop)

    assert(SOr(SEql(1), SEql(2)).canonicalize()
           == SMember(1, 2))
    // [Or [= -1],[= 0],[Not [Member -1,0]]]
    assert(SOr(SEql(-1), SEql(0), SNot(SMember(-1, 0))).canonicalize()
           == STop)
  }
  test("discovered errors") {
    abstract class Abstract1

    trait Trait2
    abstract class Abstract2 extends Trait2
    assert(classOf[Abstract2].subtypep(classOf[Trait2]).contains(true))
    assert(SAnd(classOf[Abstract2], SNot(classOf[Trait2])).canonicalize()
           == SEmpty)
    SNot(SOr(SOr(classOf[Abstract1], SEql(1)))).canonicalize(Some(Dnf))

    // [And [Not java.lang.String],
    //      [Not [= 0]]]
    SOr(SNot(String),
        SNot(SEql(0))).canonicalize(Some(Dnf))
    // [Not [Or [Not
    //           [Or
    //             [Not java.lang.Integer]]],
    //          [And ]]]
    SNot(SOr(SNot(SOr(SNot(classOf[Integer]))),
             SAnd())).canonicalize(Some(Dnf))

    // [Or [Or [Or [Not [Or [= 1]]]],
    //         [Not [Or [Or Abstract1$1,[= 0]]]]]]
    SOr(SOr(SOr(SNot(SOr(SEql(1)))),
            SNot(SOr(SOr(classOf[Abstract1],
                         SEql(1)))))).canonicalize(Some(Dnf))
    locally {
      // union: converted [Or [Not [Not java.lang.Integer]],[Not [Not Abstract1$1]]] to Top
      val t1 = SOr(SNot(SNot(classOf[java.lang.Integer])),
                   SNot(SNot(classOf[Abstract1]))).canonicalize()
      assert(t1.canonicalize() != STop)
    }
    locally {
      val t1 = SAnd(SNot(classOf[Abstract1]),
                    SNot(classOf[Abstract2]))
      val dnf = t1.canonicalize(Some(Dnf))
      assert(t1 - dnf == SEmpty)
      // println(List(t1,dnf,t1 - dnf))
    }
    locally {
      val t1 = SNot(SAnd(SNot(classOf[java.lang.Integer]),
                         SNot(classOf[Abstract1])))
      assert(t1.canonicalize() != STop)
    }
    locally {
      val t1 = SAnd(SNot(classOf[Abstract1]),
                    SNot(classOf[java.lang.Integer]))
      val dnf = t1.canonicalize(Some(Dnf))
      assert(t1 - dnf == SEmpty)
    }
    // Expected :Top td=[And [Not [= -1]],
    //                       [Or java.lang.Number,
    //                           [Member a,b,c]]]
    //          inverse=[Not [Or [And [Not [= -1]],
    //                                java.lang.Number],
    //                           [Member a,b,c]]],
    //          td || inverse=[Or [And [Not [= -1]],
    //                                 java.lang.Number],
    //                            [And [Not [Member a,b,c]],
    //                                 [Not java.lang.Number]],
    //                            [Member -1,a,b,c]],
    //     expecting TopType
    // Actual   :[Or [And [Not [= -1]],
    //                    java.lang.Number],
    //               [And [Not [Member a,b,c]],
    //                    [Not java.lang.Number]],
    //               [Member -1,a,b,c]]
    locally {
      val t1 = SOr(SAnd(SNot(SEql(-1)),
                        classOf[java.lang.Number]),
                   SAnd(SNot(SMember("a", "b", "c")),
                        SNot(classOf[java.lang.Number])),
                   SMember(-1, "a", "b", "c"))
      assert(t1.inhabited.contains(true))
      println("dnf =" + t1.canonicalize(Some(Dnf)))
      println("cnf =" + t1.canonicalize(Some(Cnf)))
      assert(t1.canonicalize(Some(Cnf)) == STop)
    }
  }
  test("dnf vs cnf") {
    trait TraitA
    trait TraitB
    trait TraitC
    trait TraitD
    val A = classOf[TraitA]
    val B = classOf[TraitB]
    val C = classOf[TraitC]
    val D = classOf[TraitD]

    val dnf1 = SOr(SAnd(A, B),
                   SNot(SAnd(C, D)))
    val cnf2 = dnf1.toCnf
    val cnf3 = dnf1.canonicalize(Some(Cnf))

    assert(cnf2 - dnf1 == SEmpty, "test 1")
    assert(dnf1 - cnf2 == SEmpty, "test 2")
    assert(cnf3 - dnf1 == SEmpty, "test 5")
    assert(dnf1 - cnf3 == SEmpty, "test 6")


    val cnf1 = SAnd(SOr(A, B),
                    SNot(SOr(C, D)))
    val dnf2 = cnf1.toDnf
    val dnf3 = cnf1.canonicalize(Some(Dnf))

    assert(dnf2 - cnf1 == SEmpty, "test 3")
    assert(cnf1 - dnf2 == SEmpty, "test 4")
    assert(cnf1 - dnf3 == SEmpty, "test 7")
    assert(dnf3 - cnf1 == SEmpty, "test 8")
  }

  test("randomized testing of canonicalize") {

    for {_ <- 0 to 500
         td = randomType(5)
         can = td.canonicalize(Some(Dnf))
         dnf = td.canonicalize(Some(Dnf))
         td_inhabited = td.inhabited
         can_inhabited = can.inhabited
         dnf_inhabited = dnf.inhabited} {
      assert(can_inhabited.isEmpty
             || td_inhabited.isEmpty
             || td.inhabited == can.inhabited,
             s"td=$td  can=$can, inhabited = $td_inhabited vs $can_inhabited")
      assert(td_inhabited.isEmpty
             || dnf_inhabited.isEmpty
             || td.inhabited == dnf.inhabited,
             s"td=$td  dnf=$dnf inhabited = $td_inhabited vs $dnf_inhabited")
    }
  }

  //    [Or java.lang.Number,Trait1$1] did not equal Empty
  //    td=[And [Not [= 1]],
  //            [Or java.lang.Number,
  //                Trait1$1]]
  //    dnf=[Or Trait1$1,
  //            [And [Not [= 1]],
  //                 java.lang.Number]],
  //    td-dnf=[Or java.lang.Number,
  //               Trait1$1],
  //    expecting EmptyType
  //                                                                                                                                                                                                        ScalaTestFailureLocation: genus.GenusCanonicalize at (GenusCanonicalize.scala:327)
  //  Expected :Empty
  //  td=[And [Not [= 1]],
  //          [Or java.lang.Number,
  //              Trait1$1]]
  //  dnf=[Or Trait1$1,
  //          [And [Not [= 1]],
  //               java.lang.Number]],
  //  td-dnf=[Or java.lang.Number,
  //             Trait1$1],
  //  expecting EmptyType
  //                                                                                                                                                                   Actual   :[Or java.lang.Number,Trait1$1]
  //  org.scalatest.exceptions.TestFailedException: [Or java.lang.Number,Trait1$1]
  //    did not equal Empty td=[And [Not [= 1]],[Or java.lang.Number,Trait1$1]]
  //                        dnf=[Or Trait1$1,[And [Not [= 1]],java.lang.Number]],
  //                        td-dnf=[Or java.lang.Number,Trait1$1],
  //                        expecting EmptyType
  //                                                                                                                                                                                                                                                    at org.scalatest.Assertions.newAssertionFailedException(Assertions.scala:472)
  //  at org.scalatest.Assertions.newAssertionFailedException$(Assertions.scala:471)
  //  at org.scalatest.Assertions$.newAssertionFailedException(Assertions.scala:1231)
  //  at org.scalatest.Assertions$AssertionsHelper.macroAssert(Assertions.scala:1295)
  //  at genus.GenusCanonicalize.$anonfun$new$19(GenusCanonicalize.scala:327)

  def testDnfInverse(td: SimpleTypeD): Assertion = {
    val dnf = td.canonicalize(Some(Dnf))
    val inverse = SNot(dnf)

    assert(td - dnf == SEmpty,
           s"td=$td dnf=$dnf, dnf inverse=$inverse, td-dnf=${td - dnf}, expecting SEmpty")
    assert(((td || inverse) == STop) || (!(td || inverse) == SEmpty),
           s"td=$td inverse=$inverse, td || inverse=${td || inverse}, expecting SEmpty")
  }

  test("issue 5"){
    //  td=[And [Not [= 1]],
    //          [Or java.lang.Number,
    //              Trait1$1]]

    testDnfInverse(SAnd(SNot(SEql(1)),
                        SOr(classOf[java.lang.Number],
                            classOf[Trait1])))
  }
  test("issue 4"){
    //td=[And [Or java.lang.Integer,
    //           [Member a,b,c]],
    //        [Not [Member 4,5,6]]]

    val A = SMember(4,5,6)
    val B = classOf[java.lang.Integer]
    val C = SMember("a","b","c")
    val D = SAnd( SNot(SEmpty), SNot(C), SNot(B), SOr(C,B))
    assert(D.canonicalize(Some(Dnf)) == SEmpty)

    //[And [Not [Member 4,5,6]],[Or [Member a,b,c],java.lang.Integer],[And [Not [Member a,b,c]],[Not java.lang.Integer]]]
    val E = SAnd( SNot(A),SOr(C,B),SAnd(SNot( C),SNot(B)))
    assert(E.canonicalize(Some(Dnf)) == SEmpty)

    testDnfInverse(SAnd(SOr(B, C),
                        SNot(A)))

    // [And [And [Or java.lang.Integer,
    //              [Member a,b,c]],
    //           [Not [Member 4,5,6]]],
    //      [Not [Or [And [Not [Member 4,5,6]],
    //                    java.lang.Integer],
    //               [Member a,b,c]]]]
  }
  test("randomized testing of inversion") {
    for {_ <- 0 to 500
         }
      testDnfInverse(randomType(2))
  }
}
