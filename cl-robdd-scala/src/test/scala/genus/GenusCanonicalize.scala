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
import RandomType.{randomType,interestingValues}

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


    assert(SAnd(SAtomic(Types.String),SNot(SMember("1","2","3","4"))).canonicalizeOnce()
           != SAtomic(Types.String))

    val td1 = SAnd(SAtomic(Types.String),
                   SNot(SMember("1", "2")),
                   SNot(SMember("3", "4")))

    assert(td1.canonicalize()
             == SAnd(SAtomic(Types.String),
                     SNot(SMember("1", "2", "3", "4"))),
           s"\nlhs= " + td1.canonicalize() +
             "\nrhs" + SAnd(SAtomic(Types.String),
                            SNot(SMember("1", "2", "3", "4"))
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
      t1.canonicalize(Some(Dnf))
      t1.canonicalize(Some(Cnf))
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
  test("verify cnf dnf") {
    import NormalForm._
    def baseCase(td:SimpleTypeD):Boolean = {
      td match {
        case _:TerminalType => true
        case SNot(_:TerminalType) => true
        case _ => false
      }
    }
    def isCnf(td:SimpleTypeD):Boolean = {
      td match {
        case td if baseCase(td) => true
        case SOr(tds@ _*) => tds.forall(baseCase)
        case SAnd(tds@ _*) =>
          val ors = tds.collect{case td:SOr => td}
          val others = tds diff ors
          others.forall(td => baseCase(td)) && ors.forall{case SOr(tds@ _*) => tds.forall(baseCase)}
        case _ => false
      }
    }
    def isDnf(td:SimpleTypeD):Boolean = {
      td match {
        case td if baseCase(td) => true
        case SAnd(tds@ _*) => tds.forall(baseCase)
        case SOr(tds@ _*) =>
          val ands = tds.collect{case td:SAnd => td}
          val others = tds diff ands
          others.forall(td => baseCase(td)) && ands.forall{case SAnd(tds@ _*) => tds.forall(baseCase)}
        case _ => false
      }
    }
    for {_ <- 0 to 1000
         n <- 0 to 5
         rt = randomType(n)
         } {
      assert(isDnf(rt.canonicalize(Some(Dnf))), s"\nrt=$rt\ndnf=${rt.canonicalize(Some(Dnf))}")
      assert(isCnf(rt.canonicalize(Some(Cnf))), s"\nrt=$rt\ncnf=${rt.canonicalize(Some(Cnf))}")
    }
  }

  def check_type(td:SimpleTypeD):Unit= {
    for {v <- interestingValues
         cnf = td.canonicalize(Some(Cnf))
         dnf = td.canonicalize(Some(Dnf))
         } {
      assert(td.typep(v) == cnf.typep(v),
             s"\nv = $v" + " type=" + v.getClass() +
               "\nclosedWorldView = " + SAtomic.closedWorldView.value +
               s"\ntd=$td" +
               s"\ncnf=$cnf" +
               "\nlhs = td.typep(v)  = " + td.typep(v) +
               "\nrhs = cnf.typep(v) = " + cnf.typep(v)
             )
      assert(td.typep(v) == dnf.typep(v),
             "\n closedWorldView = " + SAtomic.closedWorldView.value +
               s"\ntd=$td" +
               s"\ndnf=$dnf" +
               "\nlhs=" + td.typep(v) +
               "\nrhs=" + dnf.typep(v)
             )
    }
  }
  test("discovered 388"){
    val one:Integer = 1
    //assert(classOf[Long].isInstance(one))
    //assert(classOf[Integer].isInstance(one))
    assert(SAtomic(classOf[Integer]).typep(one))
    assert(SAtomic(Integer).typep(one))
    check_type(SAtomic(Integer))
    check_type(SOr(SAtomic(classOf[Integer]),SEql(one)))
    check_type(SOr(SAtomic(Integer),SEql(one)))

    assert(SAtomic(classOf[Integer]).typep(1))
    check_type(SOr(Integer,SEql(1)))
    check_type(SOr(SAtomic(Integer),SEql(1)))
  }
  test("discovered 389"){
    assert(SAtomic(classOf[Long]).typep(1L))
    assert(SAtomic(classOf[scala.Long]).typep(1L))
    assert(SAtomic(classOf[java.lang.Long]).typep(1L))

    assert(SAtomic(classOf[Integer]).typep(1))
    assert(SAtomic(classOf[scala.Int]).typep(1))
    assert(SAtomic(classOf[java.lang.Integer]).typep(1))
    assert(SAtomic(classOf[Int]).typep(1))
    assert(SAtomic(classOf[Int]).typep(1L))

    assert(SAtomic(classOf[Short]).typep(1:Short))
    assert(SAtomic(classOf[java.lang.Short]).typep(1:Short))
    assert(SAtomic(classOf[scala.Short]).typep(1:Short))
  }
  test("rand typep") {
    // make sure typep membership of particular values is the same before and after canonicalizing
    def testit() = {
      for {depth <- 0 to 5
           _ <- 0 to 2000
           } check_type(randomType(depth))
    }
    SAtomic.withOpenWorldView(testit())
    SAtomic.withClosedWorldView(testit())
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
  test("combo conversion14"){
    // (or (member 1 2 3) (member 2 3 4 5)) --> (member 1 2 3 4 5)
    // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))
    val S = SAtomic(classOf[String])
    assert(SOr(SMember(1, 2, 3), SMember(2, 3, 4, 5)).conversion14()
             == SMember(1,2,3,4,5))

    assert(SOr(SMember(1, 2, 3), SMember(2, 3, 4, 5)).conversion14()
             == SMember(1,2,3,4,5))

    assert(SOr(SMember(1, 2, 3), SMember(2, 3, 4, 5), S).conversion14()
             == SOr(SMember(1,2,3,4,5),S))
    assert(SOr(SMember(1, 2, 3, "hello"), SMember(2, 3, 4, 5, "world"), S).conversion14()
             == SOr(SMember(1,"hello",2,3,4,5,"world"),S))
    assert(SOr(SAtomic(A),
               SMember(1, 2, 3),
               SMember(3, 4, 5)).conversion14()
             == SOr(SAtomic(A),
                    SMember(1, 2, 3, 4, 5)))
  }
  test("combo conversion13"){
    assert(SAnd(SAtomic(Types.String),
                SNot(SMember("1", "2")),
                SNot(SMember("3", "4"))).conversion13()
             == SAnd(SAtomic(Types.String),
                     SNot(SMember("1", "2", "3", "4"))
                     ))
  }
  test("and conversion20"){
    val S = SAtomic(classOf[String])
    assert(SAnd(S,SMember(1,2,3,4),SNot(SMember(3,4,5,6))).conversion15()
             == SAnd(S,SMember(1,2)))
  }
  test("or conversion20"){
    val S = SAtomic(classOf[String])
    assert(SOr(S,SMember(1,2,3,4),SNot(SMember(3,4,5,6))).conversion15()
             == SOr(S,SNot(SMember(5,6))))
  }
  test("and/or member/not-member") {
    val S = SAtomic(classOf[String])
    val values = List(1, 2, 3, 4, "a", "b", "c", "d", true)

    for {td <- Seq(SOr(S, SMember(1, 2, 3)),
                   SOr(S, SMember("a", "b", "c")),
                   SOr(S, SMember("a", "b", "c", 1, 2, 3)),
                   SAnd(S, SMember("a", "b", "c", 1, 2, 3)),
                   SAnd(S, SMember("a", "b", "c")),
                   SAnd(S, SMember(1, 2, 3)),
                   SAnd(S, SNot(SMember("a", "b", "c", 1, 2, 3))),
                   SAnd(S, SNot(SMember("a", "b", "c"))),
                   SAnd(S, SNot(SMember(1, 2, 3))),
                   SOr(S, SNot(SMember("a", "b", "c", 1, 2, 3))),
                   SOr(S, SNot(SMember("a", "b", "c"))),
                   SOr(S, SNot(SMember(1, 2, 3)))
                   )
         td1 = td.conversion16()
         td2 = td.canonicalizeOnce()
         td3 = td.canonicalize()
         v <- values} {
      assert(td.typep(v) == td1.typep(v))
      assert(td.typep(v) == td2.typep(v),
             s"\nv=$v" +
               "\nrhs =" + td +
               "\nlhs =" + td2)
      assert(td.typep(v) == td3.typep(v),
             s"\nv=$v" +
               "\nrhs =" + td +
               "\nlhs =" + td3)
    }
  }
  test("combo conversion16") {
    val S = SAtomic(classOf[String])
    val values = List("a", "b", "c", "c", 1, 2, 3, 4, true)
    for {td <- Seq(SAnd(S, SMember("a", "b", "c", 1, 2, 3)),
                   SOr(S, SMember("a", "b", "c", 1, 2, 3)),
                   SAnd(S, SNot(SMember("a", "b", "c", 1, 2, 3))),
                   SOr(S, SNot(SMember("a", "b", "c", 1, 2, 3))))
         v <- values}
      assert(td.typep(v) == td.conversion16().typep(v))


    assert(SAnd(S, SMember("a", "b", "c", 1, 2, 3)).conversion16()
             == SAnd(S, SMember("a", "b", "c")))
    assert(SAnd(S, SNot(SMember("a", "b", "c", 1, 2, 3))).conversion16()
             == SAnd(S, SNot(SMember("a", "b", "c"))))
    assert(SOr(S, SMember("a", "b", "c", 1, 2, 3)).conversion16()
             == SOr(S, SMember(1, 2, 3)))
    assert(SOr(S, SNot(SMember("a", "b", "c", 1, 2, 3))).conversion16()
             == SOr(S, SNot(SMember(1, 2, 3))))

    // (and Long (not (member 1 2)) (not (member 2 3 4)))
    //  --> (and Long (not (member 1 2 3 4)))
    assert(SAnd(SInt, SNot(SMember(1, 2, 3, 4))).conversion16()
             == SAnd(SInt, SNot(SMember(1, 2, 3, 4))))

    // (and Double (not (member 1.0 2.0 "a" "b"))) --> (and Double (not (member 1.0 2.0)))
    assert(SAnd(SInt, SNot(SMember(1, 2, 3, 4, "a", "b"))).conversion16()
             == SAnd(SInt, SNot(SMember(1, 2, 3, 4))))
  }
  test("combo conversionC12"){

    trait TraitA
    trait TraitB
    trait TraitC
    trait TraitY
    class ClassX extends TraitA with TraitB with TraitC with TraitY

    val A = SAtomic(classOf[TraitA])
    val B = SAtomic(classOf[TraitB])
    val C = SAtomic(classOf[TraitC])
    val X = SAtomic(classOf[ClassX])

    // AXBC + !X = ABC + !X
    assert(SOr(SAnd(A, X, B, C), SNot(X)).conversion12()
             == SOr(SAnd(A,B,C),SNot(X)))

    // (A+X+B+C)!X = (A+B+C)!X
    assert(SAnd(SOr(A, X, B, C), SNot(X)).conversion12()
             == SAnd(SOr(A,B,C),SNot(X)))
  }
  test("combo conversion11"){
    trait TraitA
    trait TraitB
    trait TraitC
    trait TraitY
    class ClassX extends TraitA with TraitB with TraitC with TraitY

    val A = SAtomic(classOf[TraitA])
    val B = SAtomic(classOf[TraitB])
    val C = SAtomic(classOf[TraitC])
    val X = SAtomic(classOf[ClassX])
    val Y = SAtomic(classOf[TraitY])

    // A + A!B -> A + B
    assert(SOr(A, SAnd(SNot(A), B)).conversion11()
             == SOr(A,B))

    // A + A!BX + Y = (A + Y)
    assert(SOr(A, SAnd(A, SNot(B), X), Y).conversion11()
             == SOr(A,Y))

    //                         A +      A! BX +      Y = (A + BX + Y)
    assert(SOr(A, SAnd(SNot(A), B, X), Y).conversion11()
             == SOr(A,SAnd(B,X),Y))

    // if rule does not apply, the it must return exactly the default
    assert(SOr(A, B, C).conversion11() == SOr(A,B,C))



    // ---------------

    // A(A+!B) -> AB
    assert(SAnd(A, SOr(SNot(A), B)).conversion11()
             == SAnd(A,B))

    // A(A+!B+X)Y = AY
    assert(SAnd(A, SOr(A, SNot(B), X), Y).conversion11()
             == SAnd(A,Y))

    //                         A(!A+B+X)     Y = A(B+X)Y
    assert(SAnd(A, SOr(SNot(A), B, X), Y).conversion11()
             == SAnd(A,SOr(B,X),Y))

    // if rule does not apply, the it must return exactly the default
    assert(SAnd(A, B, C).conversion11() == SAnd(A,B,C))
  }
  test("combo conversion9"){
    trait TraitA
    trait TraitB
    trait TraitC
    trait TraitY
    class ClassX extends TraitA with TraitB with TraitC with TraitY

    val A = SAtomic(classOf[TraitA])
    val B = SAtomic(classOf[TraitB])
    val C = SAtomic(classOf[TraitC])
    val X = SAtomic(classOf[ClassX])

    // ABC + A!BC + X -> ABC + AC + X (later -> AC + X)
    assert(SOr(SAnd(A, B, C), SAnd(A, SNot(B), C), X).conversion9()
             == SOr(SAnd(A,B,C),SAnd(A,C),X), "434")

    // AB!C + A!BC + A!B!C -> AB!C + A!BC + A!C ->
    assert(SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(B), SNot(C))).conversion9()
             == SOr(SAnd(A,B,SNot(C)),SAnd(A,SNot(B),C),SAnd(A,SNot(C))), "438")

    // AB!C + A!BC + A!B!C -> does not reduce to AB!C + A!BC + A
    assert(SOr(SAnd(A, B, SNot(C)), SAnd(A, SNot(B), C), SAnd(A, SNot(B), SNot(C))).conversion9()
             == SOr(SAnd(A,B,SNot(C)),SAnd(A,SNot(B),C),SAnd(A,SNot(C))))

    // no change sequence
    // !ABC + A!BC + X -> no change
    assert(SOr(SAnd(SNot(A), B, C), SAnd(A, SNot(B), C), X).conversion9()
             == SOr(SAnd(SNot(A),B,C),SAnd(A,SNot(B),C),X))


    // -----------------

    assert(SAnd(SOr(A, B, C), SOr(A, SNot(B), C), X).conversion9()
             == SAnd(SOr(A,B,C),SOr(A,C),X), "434")

    assert(SAnd(SOr(A, B, SNot(C)), SOr(A, SNot(B), C), SOr(A, SNot(B), SNot(C))).conversion9()
             == SAnd(SOr(A,B,SNot(C)),SOr(A,SNot(B),C),SOr(A,SNot(C))), "438")

    assert(SAnd(SOr(A, B, SNot(C)), SOr(A, SNot(B), C), SOr(A, SNot(B), SNot(C))).conversion9()
             == SAnd(SOr(A,B,SNot(C)),SOr(A,SNot(B),C),SOr(A,SNot(C))))


    assert(SAnd(SOr(SNot(A), B, C), SOr(A, SNot(B), C), X).conversion9()
             == SAnd(SOr(SNot(A),B,C),SOr(A,SNot(B),C),X))

  }
  test("and conversionD1"){
    // SAnd(SMember(42,43,44), A, B, C)
    //  ==> SMember(42,44)
    assert(SAnd(SMember(1, 2, 3, "hello", "world"), SInt).conversionD1()
           == SMember(1,2,3))

    // SOr(SNot(SMember(42,43,44,"a","b")), String)
    //  ==> SNot(SMember(42,43,44))
    assert(SOr(SNot(SMember(1, 2, 3, "hello", "world")), SAtomic(classOf[String])).conversionD1()
             == SNot(SMember(1,2,3)))
  }

  test("combo conversionD3") {
    // discover a disjoint pair
    assert(SAnd(SMember(1, 2, 3), SMember(4, 5, 6)).conversionD3()
             == SEmpty)
    assert(SAnd(SMember(1, 2, 3), SMember(3, 4, 5), SMember(4, 5, 6)).conversionD3()
             == SEmpty)
    // else return the default
    assert(SAnd(SMember(1, 2, 3), SMember(3, 4, 5)).conversionD3()
             != SEmpty)


    assert(SOr(SNot(SMember(1, 2, 3)), SNot(SMember(4, 5, 6))).conversionD3()
             == STop)
    assert(SOr(SNot(SMember(1, 2, 3)), SNot(SMember(3, 4, 5)), SNot(SMember(4, 5, 6))).conversionD3()
             == STop)
    // else return the default
    assert(SOr(SNot(SMember(1, 2, 3)), SNot(SMember(3, 4, 5))).conversionD3()
             != STop)

  }
  test("and conversion9"){
    trait TraitA
    trait TraitB
    trait TraitC
    trait TraitY
    class ClassX extends TraitA with TraitB with TraitC with TraitY

    val A = SAtomic(classOf[TraitA])
    val B = SAtomic(classOf[TraitB])
    val C = SAtomic(classOf[TraitC])
    val X = SAtomic(classOf[ClassX])

    // ABC + A!BC + X -> ABC + AC + X (later -> AC + X)
    assert(SAnd(SOr(A, B, C), SOr(A, SNot(B), C), X).conversion9()
             == SAnd(SOr(A,B,C),SOr(A,C),X), "434")

    // AB!C + A!BC + A!B!C -> AB!C + A!BC + A!C ->
    assert(SAnd(SOr(A, B, SNot(C)), SOr(A, SNot(B), C), SOr(A, SNot(B), SNot(C))).conversion9()
             == SAnd(SOr(A,B,SNot(C)),SOr(A,SNot(B),C),SOr(A,SNot(C))), "438")

    // AB!C + A!BC + A!B!C -> does not reduce to AB!C + A!BC + A
    assert(SAnd(SOr(A, B, SNot(C)), SOr(A, SNot(B), C), SOr(A, SNot(B), SNot(C))).conversion9()
             == SAnd(SOr(A,B,SNot(C)),SOr(A,SNot(B),C),SOr(A,SNot(C))))

    // no change sequence
    // !ABC + A!BC + X -> no change
    assert(SAnd(SOr(SNot(A), B, C), SOr(A, SNot(B), C), X).conversion9()
             == SAnd(SOr(SNot(A),B,C),SOr(A,SNot(B),C),X))
  }
  test("combo conversion10"){
    // (and A B) --> (and A) if  A is subtype of B
    assert(SAnd(SMember(1, 2, 3), SMember(1, 2)).conversion10()
             == SMember(1,2))
    assert(SAnd(SMember(1, 2), SMember(1, 2, 3)).conversion10()
             == SMember(1,2))
    // (and A B C) --> (and A C) if  A is subtype of B
    assert(SAnd(SMember(1, 2), SMember(1, 2, 3), SDouble).conversion10()
             == SAnd(SMember(1,2),SDouble))
    assert(SAnd(SDouble, SMember(1, 2), SMember(1, 2, 3)).conversion10()
             == SAnd(SDouble,SMember(1,2)))

    // (or A B) --> (and A) if  A is supertype of B
    assert(SOr(SMember(1, 2, 3), SMember(1, 2)).conversion10()
             == SMember(1,2,3))
    assert(SOr(SMember(1, 2), SMember(1, 2, 3)).conversion10()
             == SMember(1,2,3))
    // (or A B C) --> (or A C) if  A is supertype of B
    assert(SOr(SMember(1, 2), SMember(1, 2, 3), SDouble).conversion10()
             == SOr(SMember(1,2,3),SDouble))
    assert(SOr(SDouble, SMember(1, 2), SMember(1, 2, 3)).conversion10()
             == SOr(SDouble,SMember(1,2,3)))

    locally{
        abstract class ClassBsup

        trait TraitX
        trait TraitY
        trait TraitZ
        class XYZ extends TraitX with TraitY with TraitZ

        val Asub = SAtomic(classOf[ClassAsub])
        val Bsup = SAtomic(classOf[ClassBsup])
        val X = SAtomic(classOf[TraitX])
        val Y = SAtomic(classOf[TraitY])
        val Z = SAtomic(classOf[TraitZ])

        class ClassAsub extends ClassBsup

        // if Asub is subtype of Bsup then
        // SOr(X,Asub,Y,Bsup,Z) --> SOr(X,Y,Bsup,Z)
        assert(SOr(X, Asub, Y, Bsup, Z).conversion10()
                 == SOr(X,Y,Bsup,Z))
        assert(SOr(X, Bsup, Y, Asub, Z).conversion10()
                 == SOr(X,Bsup,Y,Z))

        // but be careful, if A < B and B < A we DO NOT want to remove both.
        assert((SOr(Z, SOr(X, Y), SOr(Y, X)).conversion10()
          == SOr(Z,SOr(Y,X)))
                 || (SOr(Z, SOr(X, Y), SOr(Y, X)).conversion10()
          == SOr(Z,SOr(X,Y))))
      }
  }
  test("combo conversion1"){
    // (and) -> STop,  unit=STop,   zero=SEmpty
    // (or) -> SEmpty, unit=SEmpty,   zero=STop
    assert(SAnd().conversion1() == STop)
    assert(SOr().conversion1() == SEmpty)
    // (and A) -> A
    // (or A) -> A
    assert(SAnd(SEql(1)).conversion1() == SEql(1))
    assert(SOr(SEql(1)).conversion1() == SEql(1))

    assert(SAnd(SEql(1),SEql(2)).conversion1() == SAnd(SEql(1),SEql(2)))
    assert(SOr(SEql(1),SEql(2)).conversion1() == SOr(SEql(1),SEql(2)))
  }
  test("combo conversion2"){
    // (and A B SEmpty C D) -> SEmpty,  unit=STop,   zero=SEmpty
    // (or A B STop C D) -> STop,     unit=SEmpty,   zero=STop
    assert(SAnd(SEql(1),SEql(2),SEmpty,SEql(3),SEql(4)).conversion2()
             == SEmpty)
    assert(SOr(SEql(1),SEql(2),STop,SEql(3),SEql(4)).conversion2()
             == STop)
  }
  test("combo conversion3"){
    // (and A (not A)) --> SEmpty,  unit=STop,   zero=SEmpty
    // (or A (not A)) --> STop,     unit=SEmpty, zero=STop
    assert(SAnd(SEql(1),SNot(SEql(1))).conversion3()
             == SEmpty)
    assert(SOr(SEql(1),SNot(SEql(1))).conversion3()
             == STop)
  }
  test("combo conversion4"){
    // SAnd(A,STop,B) ==> SAnd(A,B),  unit=STop,   zero=SEmpty
    // SOr(A,SEmpty,B) ==> SOr(A,B),  unit=SEmpty, zero=STop
    assert(SAnd(SEql(1),STop,SEql(2)).conversion4()
             == SAnd(SEql(1),SEql(2)))
    assert(SOr(SEql(1),SEmpty,SEql(2)).conversion4()
             == SOr(SEql(1),SEql(2)))
  }
  test("combo conversion5"){
    // (and A B A C) -> (and A B C)
    // (or A B A C) -> (or A B C)
    assert(SAnd(SEql(1),SEql(2),SEql(1),SEql(3)).conversion5()
             == SAnd(SEql(2),SEql(1),SEql(3)))
    assert(SOr(SEql(1),SEql(2),SEql(1),SEql(3)).conversion5()
             == SOr(SEql(2),SEql(1),SEql(3)))
  }
  test("combo conversion6"){
    // (and A (and B C) D) --> (and A B C D)
    // (or A (or B C) D) --> (or A B C D)
    val a = SEql(1)
    val b = SEql(2)
    val c = SEql(3)
    val d = SEql(4)
    assert(SAnd(a,SAnd(b,c),d).conversion6()
             == SAnd(a,b,c,d))
    assert(SOr(a,SOr(b,c),d).conversion6()
             == SOr(a,b,c,d))
  }

  test("combo conversion8"){
    abstract class ClassA
    val A = SAtomic(classOf[ClassA])
    class ClassB extends ClassA
    val B = SAtomic(classOf[ClassB])
    // (or A (not B)) --> STop   if B is subtype of A,    zero=STop
    assert(SOr(A,SNot(B)).conversion8()
             == STop)
    assert(SOr(B,SNot(A)).conversion8()
             == SOr(B,SNot(A)))

    // (and A (not B)) --> SEmpty   if B is supertype of A,   zero=SEmpty
    assert(SAnd(A,SNot(B)).conversion8()
             == SAnd(A,SNot(B)))
    assert(SAnd(B,SNot(A)).conversion8()
             == SEmpty)
  }
}
