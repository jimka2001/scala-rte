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

import RandomType._
import java.lang
import RandomType.randomType
import Types._

class Test1

class Test2 extends Test1

trait Trait1

trait Trait2 extends Trait1

class Test3 extends Test2 with Trait2

import adjuvant.MyFunSuite
class GenusSubtypep extends MyFunSuite {
  val Long: Class[lang.Long] = classOf[java.lang.Long]
  val Integer: Class[Integer] = classOf[java.lang.Integer]
  val Number: Class[Number] = classOf[java.lang.Number]

  test("subtypep Java types") {
    // superclass.isAssignableFrom(subclass)
    assert(Number.isAssignableFrom(Integer))
    assert(Number.isAssignableFrom(Long))
    assert(!Integer.isAssignableFrom(Number))
    assert(!Long.isAssignableFrom(Integer))
    assert(!Integer.isAssignableFrom(Long))

    assert(SAtomic(Integer).subtypep(SAtomic(Number)).contains(true))
    assert(SAtomic(Long).subtypep(SAtomic(Long)).contains(true))
    assert(SAtomic(Number).subtypep(SAtomic(Integer)).contains(false))
    assert(SAtomic(Integer).subtypep(SAtomic(Long)).contains(false))
    assert(SAtomic(Long).subtypep(SAtomic(Integer)).contains(false))
  }

  test("subtypep Scala types") {
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test2])).contains(true),
           "Test2 < Test2")
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test1])).contains(true),
           "Test1 < Test1")
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test1])).contains(true),
           "Test2 < Test1")
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test2])).contains(false),
           "not Test1 < Test2")
  }

  test("subtypep Scala traits") {
    // every type is a subtype of itself
    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Trait1])).contains(true))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Trait2])).contains(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test3])).contains(true))


    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Trait1])).contains(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Trait2])).contains(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test2])).contains(true))
    assert(SAtomic(classOf[Test3]).subtypep(SAtomic(classOf[Test1])).contains(true))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Trait1])).contains(true))

    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Test3])).contains(false))
    assert(SAtomic(classOf[Trait2]).subtypep(SAtomic(classOf[Test3])).contains(false))
    assert(SAtomic(classOf[Test2]).subtypep(SAtomic(classOf[Test3])).contains(false))
    assert(SAtomic(classOf[Test1]).subtypep(SAtomic(classOf[Test3])).contains(false))
    assert(SAtomic(classOf[Trait1]).subtypep(SAtomic(classOf[Trait2])).contains(false))
  }

  test("AtomicType subtype of union") {
    assert(SAtomic(classOf[Test3]).subtypep(SOr(SAtomic(classOf[Trait1]),
                                                SAtomic(classOf[Trait2]),
                                                SAtomic(classOf[Test1]))).contains(true))

    assert(SAtomic(classOf[Trait2]).subtypep(SOr(SAtomic(classOf[Test2]),
                                                 SAtomic(classOf[Trait1]))).contains(true))

    assert(!SAtomic(classOf[Test1]).subtypep(SOr(SAtomic(classOf[Test2]),
                                                 SAtomic(classOf[Trait1]))).contains(true))

    // Test1 is disjoint from Integer and also from Long, so it is not a subtype of their union.
    assert(SAtomic(classOf[Test1]).subtypep(SOr(SAtomic(Integer),
                                                SAtomic(Long))).contains(false))
  }
  test("AtomicType subtype of intersection") {

    //  Test2 < Test1
    // Trait2 < Trait1
    //  Test3 < Test2
    //  Test3 < Trait2
    class Test3 extends Test2 with Trait2
    assert(SAtomic(classOf[Test3]).subtypep(SAnd(SAtomic(classOf[Trait1]),
                                                 SAtomic(classOf[Trait2]),
                                                 SAtomic(classOf[Test1]))).contains(true))

    assert(!SAtomic(classOf[Trait2]).subtypep(SAnd(SAtomic(classOf[Test2]),
                                                   SAtomic(classOf[Trait1]))).contains(true))

    assert(!SAtomic(classOf[Test1]).subtypep(SAnd(SAtomic(classOf[Test2]),
                                                  SAtomic(classOf[Trait1]))).contains(true))

    // Test1 is disjoint from Integer and also from Long, so it is not a subtype of their intersection.
    assert(SAtomic(classOf[Test1]).subtypep(SAnd(SAtomic(Integer),
                                                 SAtomic(Number),
                                                 SAtomic(Long))).contains(false))
  }
  test("AtomicType subtype of intersection 2") {
    trait Trait1
    trait Trait2
    assert(SAtomic(classOf[Trait1]).inhabited.contains(false))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2])).inhabited.contains(false))
    assert(SAtomic(classOf[Trait1]).disjoint(SAtomic(classOf[Trait2])).contains(true))
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2])).canonicalize() == SEmpty)
    // true because empty set is a subtype of every set
    assert(SAnd(SAtomic(classOf[Trait1]),
                SAtomic(classOf[Trait2])).subtypep(SEql(1)).contains(true))
  }
  test("AtomicType subtype of member") {
    assert(SAtomic(classOf[Test1]).subtypep(SMember(1, 2, 3)).contains(false))
  }
  test("AtomicType subtype of eql") {
    assert(SAtomic(classOf[Test1]).subtypep(SEql(1)).contains(false))
  }
  test("AtomicType subtype of Not") {
    trait Abstract1

    assert(classOf[java.lang.Integer].disjoint(classOf[Abstract1]).contains(true))
    assert(classOf[Abstract1].subtypep(SNot(classOf[java.lang.Integer])).contains(true))
    assert(classOf[java.lang.Integer].subtypep(SNot(classOf[Abstract1])).contains(true))
    assert(!SNot(classOf[java.lang.Integer]).subtypep(classOf[Abstract1]).contains(true))
    assert(!SNot(classOf[Abstract1]).subtypep(classOf[java.lang.Integer]).contains(true))

    trait Abstract2 extends Abstract1

    assert(classOf[Abstract2].subtypep(classOf[Abstract1]).contains(true))
    assert(SNot(classOf[Abstract1]).subtypep(SNot(classOf[Abstract2])).contains(true))

    assert(SNot(classOf[Abstract2]).subtypep(classOf[Abstract1]).contains(false))
  }

  test("Union subtype of Union") {
    val union1 = SOr(SAtomic(Integer),
                     SAtomic(Long))
    // A subtype of A
    assert(union1.subtypep(union1).contains(true))
  }

  test("Union subtype of AtomicType") {
    val union1 = SOr(SAtomic(Integer),
                     SAtomic(Long))
    assert(SAtomic(Integer).subtypep(SAtomic(Number)).contains(true))
    assert(SAtomic(Integer).subtypep(SAtomic(Number)).contains(true))
    assert(union1.subtypep(SAtomic(Number)).contains(true))

    assert(SAtomic(Integer).subtypep(SAtomic(classOf[Test1])).contains(false))
    assert(union1.subtypep(SAtomic(classOf[Test1])).contains(false))
  }

  test("subtypep 174") {
    assert(SAnd().subtypep(STop).contains(true))
    assert(SAnd().subtypep(SMember(1, 2, 3)).contains(false))
    assert(SOr().subtypep(STop).contains(true))
    assert(SOr().subtypep(SMember(1, 2, 3)).contains(true))
    assert(SMember(1.2, 3).subtypep(SOr()).contains(false))
    assert(SAnd(SEql(0), SEql(1)).subtypep(SEmpty).contains(true))
    assert(SAnd().subtypep(SAnd()).contains(true))
    assert(SNot(SEql(2)).subtypep(STop).contains(true))
    assert(SOr(SEql(1), SNot(SEql(2))).subtypep(STop).contains(true))
    // Some(false) contained false canonicalize: rt1=[Or [And Trait3$1],[And java.lang.String,[= 1]]] rt2=Trait3$1
    locally {
      trait Trait3
      val s = SAtomic(classOf[String])
      val t = SAtomic(classOf[Trait3])

      assert(!SAnd(s, SEql(1)).subtypep(t).contains(false))
      assert(!SOr(t,
                  SAnd(s,
                       SEql(1))).subtypep(t).contains(false))
      assert(!SOr(SAnd(t),
                  SAnd(s,
                       SEql(1))).subtypep(t).contains(false))
    }
  }

  test("intersection and union subtypep") {
    def checkSubtype(rt1: SimpleTypeD, rt2: SimpleTypeD, comment: String): Unit = {
      assert(!rt1.subtypep(rt2).contains(false), s"$comment: rt1=$rt1 rt2=$rt2")
    }

    for {_ <- 0 to 200
         n <- 0 to 5
         rt1 = randomType(n)
         rt2 = randomType(n)
         union = SOr(rt1, rt2)
         intersect = SAnd(rt1, rt2)
         } {
      checkSubtype(rt1, union, "x <: x || y")
      checkSubtype(rt2, union, "y <: y || x")
      checkSubtype(intersect, rt1, "x&y <: x")
      checkSubtype(intersect, rt2, "x&y <: y")
    }
  }

  def checkSubtype(rt1: SimpleTypeD, rt2: SimpleTypeD, comment: String): Unit = {
    assert(!rt1.subtypep(rt2).contains(false),
           s": $comment: \n   rt1= $rt1 \n   rt2= $rt2" +
             "\n  failed rt1 < rt2" +
             s"\n   rt1.canonicalize= ${rt1.canonicalize()}"+
             s"\n   rt2.canonicalize= ${rt2.canonicalize()}")
    assert(!rt2.subtypep(rt1).contains(false),
           s": $comment: \n   rt1= $rt1 \n   rt2= $rt2" +
             "\n  failed rt1 > rt2 => " + rt2.subtypep(rt1) +
             s"\n   rt1.canonicalize= ${rt1.canonicalize()}"+
             s"\n   rt2.canonicalize= ${rt2.canonicalize()}")
  }

  test("discovered case 242"){
    // Some(false) contained false : toDnf:
    // rt1= ![SAnd ![SOr [SAnd Trait3X,Trait2],!Abstract2],!![SOr Trait3X]]
    // rt2= [SOr !![SOr [SAnd Trait3X,Trait2],!Abstract2],!!![SOr Trait3X]]
    // failed rt1 > rt2
    //   rt1.canonicalize= ![SAnd Trait3X,![SOr [SAnd Trait2,Trait3X],!Abstract2]]
    //   rt2.canonicalize= STop

    val Trait3X = SAtomic(classOf[RandomType.Trait3X])
    val Trait2 = SAtomic(classOf[RandomType.Trait2])
    val Abstract2 = SAtomic(classOf[RandomType.Abstract2])

    val rt1 = SNot(SAnd( SNot(SOr( SAnd( Trait3X,Trait2),SNot(Abstract2))),
                         SNot(SNot(SOr( Trait3X)))))
    val rt2 = rt1.toDnf
    assert(!rt1.subtypep(rt2).contains(false))
    assert(!rt2.subtypep(rt1).contains(false))
  }
  test("discovered 257"){
    assert(SAtomic(classOf[java.lang.Boolean]).subtypep(SMember(true,false)).contains(true))
  }
  test("discovered 258"){
    assert(SAtomic(classOf[Boolean]).subtypep(SAtomic(classOf[Boolean])).contains(true))
    assert(SAtomic(classOf[java.lang.Boolean]).subtypep(SAtomic(classOf[java.lang.Boolean])).contains(true))

    checkSubtype(SMember(true,false), SAtomic(classOf[java.lang.Boolean]),"test 257")
    checkSubtype(SMember(true,false), SAtomic(classOf[Boolean]),"test 258")

  }
  test("discovered 269"){
    val rt = SOr(SOr(SMember(false, true), SMember(4, 5, 6)),
                 SOr(SEql(true), SAtomic(classOf[Boolean])))
    val rt2 = SOr(SMember(false, true,4,5,6),
                  SAtomic(classOf[Boolean]))
    //val rt_can = rt.canonicalize() // SMember(false,true,4,5,6)
    val rt_can = SMember(false,true,4,5,6)
    assert(SAtomic(classOf[Boolean]).subtypep(SMember(false,true,4,5,6)) != Some(false), "270")
    assert(rt2.subtypep(SMember(false, true,4,5,6)) != Some(false), "271")
    assert(rt.subtypep(SMember(false, true,4,5,6)) != Some(false), "272")
    assert(rt.subtypep(rt_can) != Some(false), "273")
    assert(rt_can.subtypep(rt) != Some(false), "274")
    checkSubtype(rt, rt_can, "discovered 269")
  }

  test("randomized testing of subtypep with normalization") {
    import NormalForm._

    for {_ <- 0 to 500
         n <- 0 to 5
         rt = randomType(n)
         } {
      checkSubtype(rt, rt.canonicalize(), "canonicalize")
      checkSubtype(rt, rt.canonicalize(Some(Dnf)), "canonicalize(Dnf)")
      checkSubtype(rt, rt.canonicalize(Some(Cnf)), "canonicalize(Cnf)")
      checkSubtype(rt, rt.toDnf, "toDnf")
      checkSubtype(rt, rt.toCnf, "toCnf")
      checkSubtype(rt, rt.toCnf.toDnf, ".toCnf.toDnf")
      checkSubtype(rt, rt.toDnf.toCnf, ".toDnf.toCnf")
      checkSubtype(rt.toCnf, rt.toDnf, "toCnf vs toDnf")
    }
  }
  test("discovered subtypep 259"){
    locally{
      // ![SAnd {4,5,6},Number,Integer]
      val rt= SNot(SAnd(SMember(4,5,6),Number,Integer))
      checkSubtype(rt, rt.canonicalize(), "canonicalize 262")
    }
    locally{
      //[SOr ![SAnd ![= 0],[SOr Number],[SOr [= -1],Integer]]]
      val rt= SOr( SNot(SAnd( SNot(SEql(0)),SOr( Number),SOr( SEql(-1),Integer))))
      //println(s"rt=$rt")
      //println(s"rt.canonicalize() = ${rt.canonicalize()}")
      checkSubtype(rt, rt.canonicalize(), "canonicalize 272")
    }
    locally{
      trait Trait1
      trait Trait2
      trait Trait3 extends Trait2
      abstract class Abstract1
      abstract class Abstract2 extends Trait3
      // [SOr Trait3$2,[= 1],Abstract2$2]
      assert(SAtomic(classOf[Trait3]).subtypep(SEql(1)).contains(true))
      assert(SAtomic(classOf[Abstract2]).subtypep(SEql(1)).contains(true))
      assert(SEql(1).subtypep(SEql(1)).contains(true))
      val rt= SOr( classOf[Trait3],SEql(1),classOf[Abstract2])
      //println(s"rt=$rt")
      //println(s"rt.canonicalize() = ${rt.canonicalize()}")
      checkSubtype(rt, rt.canonicalize(), "canonicalize 270")
    }
    locally{
      trait Trait1
      // [SOr Trait1$1,Integer]
      val rt = SOr(classOf[Trait1],classOf[java.lang.Integer])
      //println(s"rt=$rt")
      //println(s"rt.canonicalize() = ${rt.canonicalize()}")
      assert(classOf[Trait1].subtypep(SAtomic(classOf[java.lang.Integer])).contains(true))
      assert(classOf[java.lang.Integer].subtypep(SAtomic(classOf[java.lang.Integer])).contains(true))
      assert(SOr(classOf[Trait1],classOf[java.lang.Integer]).subtypep(SAtomic(classOf[java.lang.Integer])).contains(true))
      assert(SAtomic(classOf[java.lang.Integer]).subtypep(SOr(classOf[Trait1],classOf[java.lang.Integer])).contains(true))
      checkSubtype(rt, rt.canonicalize(), "canonicalize 259")
    }
    locally{
      abstract class Abstract1
      // [SOr [SOr Abstract1$1,Integer],[SAnd [= -1],[= 1]]]
      val rt = SOr(SOr(classOf[Abstract1],classOf[java.lang.Integer]),
                   SAnd(SEql(-1),SEql(1)))
      //println(s"rt=$rt")
      //println(s"rt.canonicalize() = ${rt.canonicalize()}")
      checkSubtype(rt, rt.canonicalize(), "canonicalize 274")
    }
    locally{
      abstract class Abstract1
      // ![SOr [SOr Abstract1$1,Integer],[SAnd [= -1],[= 1]]]
      val rt = SNot(SOr(SOr(classOf[Abstract1],classOf[java.lang.Integer]),
                        SAnd(SEql(-1),SEql(1))))
      //println(s"rt=$rt")
      //println(s"rt.canonicalize() = ${rt.canonicalize()}")
      checkSubtype(rt, rt.canonicalize(), "canonicalize 299")
    }
    locally{
      val rt = SOr(SNot(classOf[java.lang.Long]),
                  // SEql("hello"),
                   SNot(SEmpty))
      assert(SAtomic(classOf[java.lang.Long]).canonicalize() != SEmpty)
      assert(SNot(classOf[java.lang.Long]).canonicalize() != STop)
      assert(STop.subtypep(SNot(classOf[java.lang.Long])).contains(false))
      assert(SNot(classOf[java.lang.Long]).subtypep(STop).contains(true))
      assert(SNot(classOf[java.lang.Long]).subtypep(SNot(SEmpty)).contains(true))
      assert(SNot(SEmpty).subtypep(SNot(classOf[java.lang.Long])).contains(false))
      assert(rt.canonicalize() == STop)
    }
    assert(SOr(SNot(SNot(STop)),
               SNot(SOr(SEql(1)))).canonicalize() == STop)
    assert(SOr(SNot(classOf[java.lang.Number]),
               SNot(SEmpty)).canonicalize() == STop)

    locally{
      val rt = SOr(SNot(classOf[java.lang.Number]),
                   SOr(classOf[java.lang.Integer], SEql(0), SMember(4,5,6)),
                   SNot(SEmpty))
      assert(rt.canonicalize() == STop)
      checkSubtype(rt, rt.canonicalize(), "canonicalize 256")
    }
    locally{
      assert(SAtomic.existsInstantiatableSubclass(classOf[java.lang.String]))
      val rt = SOr(SAtomic(classOf[java.lang.String]),SEql(-1))
      checkSubtype(rt, rt.canonicalize(), "canonicalize 243")
    }
    locally{
      abstract class Abstract1
      val rt = SAtomic(classOf[Abstract1])
      assert(rt.subtypep(SEmpty).contains(true))
      checkSubtype(rt, rt.canonicalize(), "canonicalize 249")
    }
  }
  test("discovered cases 275") {
    assert(SAtomic(Long).disjoint(SAtomic(classOf[Double])).contains(true), "#1")
    assert(SAtomic(Long).subtypep(SNot(SAtomic(classOf[Double]))).contains(true), "#2 Long <: not(Double)")
    assert(SNot(SAtomic(classOf[Double])).subtypep(SAtomic(Long)).contains(false), "#3 not(Double) !<: Long")
  }
  test("discovered cases SMember/SEql") {
    assert(      SNot(SMember(1,2)).subtypep(SOr(SEql(3),SNot(SMember(1,2)))).contains(true), "#4" )
    assert(      SNot(SEql(0)).subtypep( SOr(SEql(3),SNot(SEql(0))) ).contains(true), "#5" )
  }
  test("boolean subtype"){
    val b = SAtomic(classOf[Boolean])
    assert(SEql(true).subtypep(b).contains(true), "# 380")
    assert(SEql(false).subtypep(b).contains(true), "# 381")
    assert(SMember(false,true).subtypep(b).contains(true), "# 382")
    assert(SMember(true,false).subtypep(b).contains(true), "# 383")
    assert(b.typep(true), "# 384")
    assert(b.typep(false), "# 385")
  }
}