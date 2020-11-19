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


package typesystem

import org.scalatest._
import Types._

class TypeSystemCanonicalize extends FunSuite {

  trait TraitA
  trait TraitB
  trait TraitC
  trait TraitD
  val A = classOf[TraitA]
  val B = classOf[TraitB]
  val C = classOf[TraitC]
  val D = classOf[TraitD]
  test("and canonicalize") {
    // (and A EmptyType) -> EmptyType
    assert(IntersectionType(AtomicType(A), EmptyType, AtomicType(B)).canonicalize()
           == EmptyType)
    assert(IntersectionType().canonicalize()
           == TopType)
    // (and A A B) --> (and A B)
    assert(IntersectionType(AtomicType(A),
                            AtomicType(A),
                            AtomicType(B)).canonicalize()
           == IntersectionType(AtomicType(A),
                               AtomicType(B)))

    // IntersectionType(EqlType(42), AtomicType(Integer)) --> EqlType(42)
    assert(IntersectionType(EqlType(42), AtomicType(Types.Integer)).canonicalize()
           == EqlType(42))

    // IntersectionType(MemberType(42,43,"hello"), AtomicType(Integer)) --> MemberType(42,43)
    assert(IntersectionType(MemberType(42, 43, "hello"), AtomicType(Types.Integer)).canonicalize()
           == MemberType(42, 43))

    // IntersectionType(A,TopType,B) ==> IntersectionType(A,B)
    assert(IntersectionType(AtomicType(A), TopType, AtomicType(B)).canonicalize()
           == IntersectionType(AtomicType(A), AtomicType(B)))
  }
  test("and canonicalize 2") {
    assert(MemberType("1","2","3","4").typep("2"))
    assert(AtomicType(Types.String).typep("2"))
    assert(NotType(MemberType("1","2","3","4")).typep("5"))
    assert(IntersectionType(AtomicType(Types.String),MemberType("1","2","3","4")).typep("2"))
    // (and Int (not (member 1 2)) (not (member 3 4)))
    //  --> (and Int (not (member 1 2 3 4)))
    assert(IntersectionType(AtomicType(Types.String),
                            NotType(MemberType("1", "2")),
                            NotType(MemberType("3", "4"))).canonicalize()
           == IntersectionType(NotType(MemberType("1", "2", "3", "4")),
                               AtomicType(Types.String)
                               )
           )
    assert(IntersectionType(AtomicType(Types.String),
                            NotType(MemberType("1", 2)),
                            NotType(MemberType("3", 4))).canonicalize()
           == IntersectionType(NotType(MemberType("1", "3")),
                               AtomicType(Types.String)
                               )
           )
  }
  test("(and (and A B) (and C D)) -> (and A B C D)"){
    // (and (and A B) (and C D)) -> (and A B C D)
    assert(IntersectionType(IntersectionType(AtomicType(A),
                                             AtomicType(B)),
                            IntersectionType(AtomicType(C),
                                             AtomicType(D))).canonicalize()
           == (IntersectionType(AtomicType(A),
                                AtomicType(B),
                                AtomicType(C),
                                AtomicType(D))))
  }
  test("canonicalize children of and"){
    assert(IntersectionType(MemberType("1")).canonicalize()
      == EqlType("1"))
  }
  test("canonicalize member"){
    assert(MemberType().canonicalize()
           == EmptyType)
    assert(MemberType("hello").canonicalize()
           == EqlType("hello"))
    assert(MemberType("hello").canonicalize()
           != EqlType("world"))
    assert(MemberType("hello","world").canonicalize()
           == MemberType("hello","world"))
    assert(MemberType("hello","world","world","hello").canonicalize()
           == MemberType("hello","world"))
  }

  test("canonicalize or") {
    assert(UnionType(AtomicType(A),
                     EmptyType,
                     AtomicType(B)).canonicalize()
           == UnionType(AtomicType(A),
                        AtomicType(B)))
    assert(UnionType(AtomicType(A),
                     MemberType(),
                     AtomicType(B)).canonicalize()
           == UnionType(AtomicType(A), AtomicType(B)))
    assert(UnionType().canonicalize() == EmptyType)
    assert(UnionType(AtomicType(A)).canonicalize()
           == AtomicType(A))
    assert(UnionType(AtomicType(A),
                     AtomicType(A),
                     AtomicType(B),
                     AtomicType(A)).canonicalize()
           == UnionType(AtomicType(A), AtomicType(B)))
    assert(UnionType(AtomicType(A), TopType).canonicalize()
           == TopType)
    assert(UnionType(AtomicType(A),
                     MemberType(1, 2, 3),
                     MemberType(3, 4, 5)).canonicalize()
           == UnionType(AtomicType(A),
                        MemberType(1, 2, 3, 4, 5)))
    // (or String (member 1 2 "3") (member 2 3 4 "5")) --> (or String (member 1 2 4))
    assert(UnionType(AtomicType(Types.String),
                     MemberType(1, 2, "hello"),
                     MemberType(2, 3, 4, "world")).canonicalize()
           == UnionType(MemberType(1, 2, 3, 4),
                        AtomicType(Types.String)
                        ))
    // (or (or A B) (or C D)) --> (or A B C D)
    assert(UnionType(UnionType(AtomicType(A), AtomicType(B)),
                     UnionType(AtomicType(C), AtomicType(D))).canonicalize()
           == UnionType(AtomicType(A), AtomicType(B), AtomicType(C), AtomicType(D)))
  }
  test("canonicalize or 2"){
    assert(UnionType(A,NotType(A)).canonicalize()
           == TopType)
    // (or A (and (not A) B) ==> (or A B)
    assert(UnionType(A,IntersectionType(!A,B)).canonicalize()
           == UnionType(A,B))
    // (or A (and A B)) ==> A
    assert(UnionType(A,A&&B).canonicalize()
           == AtomicType(A))
    // (or A (and (not A) B C) D) --> (or A (and B C) D)
     assert(UnionType(A, IntersectionType(!A, B, C), D).canonicalize()
           == UnionType(A, D, B&&C))
    // (or A (and A B C) D) --> (or A D)
    assert(UnionType(A, IntersectionType(A,B,C), D).canonicalize()
           == UnionType(A,D))

    // (or X (not Y)) --> Top   if Y is subtype of X
    abstract class X
    class Y extends X
    assert(UnionType(classOf[X],NotType(classOf[Y])).canonicalize()
           == TopType)
    assert(UnionType(classOf[Y],NotType(classOf[X])).canonicalize()
           == UnionType(classOf[Y],NotType(classOf[X])))

    // AXBC + !X = ABC + !X
    assert(UnionType(IntersectionType(A,classOf[X],B,C), NotType(classOf[X])).canonicalize()
           == UnionType(IntersectionType(A,B,C), NotType(classOf[X])))
  }
  test("discovered errors"){
    abstract class Abstract1

    trait Trait2
    abstract class Abstract2 extends Trait2
    assert(classOf[Abstract2].subtypep(classOf[Trait2]).contains(true))
    assert(IntersectionType(classOf[Abstract2],NotType(classOf[Trait2])).canonicalize()
           == EmptyType)
    NotType(UnionType(UnionType(classOf[Abstract1],EqlType(1)))).canonicalize(dnf=true)

    // [And [Not java.lang.String],
    //      [Not [= 0]]]
    UnionType(NotType(String),
              NotType(EqlType(0))).canonicalize(dnf=true)
    // [Not [Or [Not
    //           [Or
    //             [Not java.lang.Integer]]],
    //          [And ]]]
    NotType(UnionType(NotType(UnionType(NotType(classOf[Integer]))),
                      IntersectionType())).canonicalize(dnf=true)

    // [Or [Or [Or [Not [Or [= 1]]]],
    //         [Not [Or [Or Abstract1$1,[= 0]]]]]]
    UnionType( UnionType( UnionType( NotType( UnionType( EqlType(1)))),
                          NotType( UnionType(UnionType(classOf[Abstract1],
                                                       EqlType(1)))))).canonicalize(dnf=true)
    locally{
      // union: converted [Or [Not [Not java.lang.Integer]],[Not [Not Abstract1$1]]] to Top
      val t1 = UnionType(NotType(NotType(classOf[java.lang.Integer])),
                         NotType(NotType(classOf[Abstract1]))).canonicalize()
      assert(t1.canonicalize() != TopType)
    }
    locally {
      val t1 = IntersectionType(NotType(classOf[Abstract1]),
                                NotType(classOf[Abstract2]))
      val dnf = t1.canonicalize(dnf = true)
      assert(t1 - dnf == EmptyType)
      // println(List(t1,dnf,t1 - dnf))
    }
    locally{
      val t1 = NotType(IntersectionType(NotType(classOf[java.lang.Integer]),
                                        NotType(classOf[Abstract1])))
      assert(t1.canonicalize() != TopType)
    }
    locally {
      val t1 = IntersectionType(NotType(classOf[Abstract1]),
                                NotType(classOf[java.lang.Integer]))
      val dnf = t1.canonicalize(dnf = true)
      assert(t1 - dnf == EmptyType)
    }

  }
  test("randomized testing of canonicalize"){

    for{r <- 0 to 100
        td = randomType(5)
        can = td.canonicalize(dnf=false)
        dnf = td.canonicalize(dnf=true)
        td_inhabited = td.inhabited
        can_inhabited = can.inhabited
        dnf_inhabited = dnf.inhabited}{
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
  test("randomized testing of inversion"){
    for{r <- 0 to 100
        td = randomType(2)
        dnf = td.canonicalize(dnf=true)
        inverse= NotType(dnf)}{

      assert(td - dnf == EmptyType,
             s"td=$td dnf=$dnf, td-dnf=${td-dnf}, expecting EmptyType")
      assert((td || inverse) == TopType,
             s"td=$td inverse=$inverse, td || inverse=${td || inverse}, expecting TopType")
    }
  }
}
