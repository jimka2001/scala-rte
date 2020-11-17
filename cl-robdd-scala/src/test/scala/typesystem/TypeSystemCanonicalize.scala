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
    assert(IntersectionType(AtomicType(A), EmptyType, AtomicType(B)).canonicalize
           == EmptyType)

    // (and A A B) --> (and A B)
    assert(IntersectionType(AtomicType(A),
                            AtomicType(A),
                            AtomicType(B)).canonicalize
           == IntersectionType(AtomicType(A),
                               AtomicType(B)))

    // IntersectionType(EqlType(42), AtomicType(Integer)) --> EqlType(42)
    assert(IntersectionType(EqlType(42), AtomicType(Types.Integer)).canonicalize
           == EqlType(42))

    // IntersectionType(MemberType(42,43,"hello"), AtomicType(Integer)) --> MemberType(42,43)
    assert(IntersectionType(MemberType(42, 43, "hello"), AtomicType(Types.Integer)).canonicalize
           == MemberType(42, 43))

    // IntersectionType(A,TopType,B) ==> IntersectionType(A,B)
    assert(IntersectionType(AtomicType(A), TopType, AtomicType(B)).canonicalize
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
                            NotType(MemberType("3", "4"))).canonicalize
           == IntersectionType(AtomicType(Types.String),
                               NotType(MemberType("1", "2", "3", "4")))
           )
    assert(IntersectionType(AtomicType(Types.String),
                            NotType(MemberType("1", 2)),
                            NotType(MemberType("3", 4))).canonicalize
           == IntersectionType(AtomicType(Types.String),
                               NotType(MemberType("1", "3")))
           )
  }
  test("(and (and A B) (and C D)) -> (and A B C D)"){
    // (and (and A B) (and C D)) -> (and A B C D)
    assert(IntersectionType(IntersectionType(AtomicType(A),
                                             AtomicType(B)),
                            IntersectionType(AtomicType(C),
                                             AtomicType(D))).canonicalize
           == (IntersectionType(AtomicType(A),
                                AtomicType(B),
                                AtomicType(C),
                                AtomicType(D))))
  }

}
