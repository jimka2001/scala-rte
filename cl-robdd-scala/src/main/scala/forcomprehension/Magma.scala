package forcomprehension

abstract class Magma[T] {

  import TrueOrFalseBecause._

  def gen(): LazyList[T]

  def op(a: T, b: T): T

  def member(a: T): TrueOrFalseBecause

  def equiv(a: T, b: T): TrueOrFalseBecause = {
    if (a == b)
      True(s"$a == $b")
    else
      False(s"$a != $b")
  }

  def isClosed(): TrueOrFalseBecause = {
    forallM[T](gen(), { a =>
      forallM[T](gen(), { b =>
        member(op(a, b)) ++ s"$this not closed because non-member op($a,$b),"
      })
    })
  }

  def isAssociative(): TrueOrFalseBecause = {
    forallM[T](gen(), { a =>
      forallM[T](gen(), { b =>
        forallM[T](gen(), { c =>
          equiv(op(op(a, b), c),
                op(a, op(b, c))) ++ s"not associative: E.g, $a, $b, $c:"
        })
      })
    })
  }

  def isAbelian(): TrueOrFalseBecause = {
    forallM[T](gen(), { a =>
      forallM[T](gen(), { b =>
        equiv(op(a, b), op(b, a)) ++ s"not Abelian, e.g., $a, $b,"
      })
    })
  }

  def isIdentity(z: T): TrueOrFalseBecause = {
    forallM[T](gen(), { a =>
      equiv(op(a, z),
            op(z, a)).map( str => s"$z is not an the identity for $this, $str")
    })
  }

  def findIdentity(): Option[T] = {
    gen().find(z => isIdentity(z) match {
      case True(_) => true
      case _ => false
    })
  }

  def findInverse(z: T, a: T): Option[T] = {
    gen().find(b => (equiv(z, op(a, b)) && equiv(z, op(b, a))).toBoolean)
  }

  def findInverse(a: T): Option[T] = {
    findIdentity() match {
      case None => None
      case Some(z) => findInverse(z, a)
    }
  }

  def isInverter(z: T, invert: T => Option[T]): TrueOrFalseBecause = {
    forallM[T](gen(), { a =>
      invert(a) match {
        case None => False(s"$a has no inverse")
        case Some(b) =>
          member(b) &&
            equiv(z, op(a, b)) &&
            equiv(z, op(b, a))
      }
    })
  }

  def isSemiGroup(): TrueOrFalseBecause = {
    (isClosed() && isAssociative()).map(str =>
                                          s"$this is not a semigroup because $str")
  }

  def isMonoid(z: T): TrueOrFalseBecause = {
    (isSemiGroup() && isIdentity(z)).map(str =>
                                           s"$this not a monoid because $str")
  }

  def isGroup(z: T, invert: T => Option[T]): TrueOrFalseBecause = {
    (isMonoid(z) && isInverter(z, invert)).map(str =>
                                                 s"$this not a group because $str")
  }
}

object Magma {

  import TrueOrFalseBecause._

  def genFinite(n: Int): LazyList[Int] = {
    def loop(m: Int): LazyList[Int] = {
      if (m > n)
        LazyList.empty
      else
        m #:: loop(m + 1)
    }

    loop(0)
  }

  def cayleyTable[T](elements: LazyList[T], dyn_op: (T, T) => T): String = {
    val header: String = "*|" ++ elements.map(x => s"$x").mkString(" ")
    val divider: String = "-+" ++ elements.map(x => "-").mkString("-")
    "\n" ++ header ++ "\n" ++ divider ++ "\n" ++ elements
      .map(x => elements.map(y => s"${dyn_op(x, y)}")
        .mkString(s"$x|", " ", ""))
      .mkString("\n")
  }

  def testModP() = {
    for {p <- 2 to 10
         add = new AdditionModP(p)
         mult = new MultiplicationModP(p)
         } {
      assertM(add.isMonoid(0))

      add.isGroup(0, (a: Int) => Some((p - a) % p)) match {
        case False(str) => println(str)
        case _ => println(s"$add is a group")
      }
      mult.isGroup(1, a => (1 to p).find(b => (a * b) % p == 1)) match {
        case False(str) => println(str)
        case _ => println(s"$mult is a group")
      }
    }
  }

  def findGroups() = {
    val elements = genFinite(2)
    var groups = 0
    var monoids = 0
    var tries = 0
    // find 3 element monoids
    for {a <- elements
         b <- elements
         c <- elements
         d <- elements
         dyn_op = ((x: Int, y: Int) => (x, y) match {
           case (_, 0) => x
           case (0, _) => y
           case (1, 1) => a
           case (1, 2) => b
           case (2, 1) => c
           case (2, 2) => d
         })
         dm = DynMagma(() => elements,
                       op1 = dyn_op,
                       member1 = (a: Int) => elements.contains(a)
                       )
         } {
      tries += 1

      (dm.isGroup(0, x => dm.findInverse(x)) match {
        case tf: False => tf // println(str)
        case tf: True =>
          groups += 1
          monoids += 1
          tf ++ (s"found a group " + cayleyTable(elements, dyn_op))
      }) ||
        (dm.isMonoid(0) match {
          case tf: False => tf //println(str)
          case tf: True =>
            monoids += 1
            tf ++ (s"found a monoid " + cayleyTable(elements, dyn_op))
        }) ||
        (dm.isSemiGroup() match {
          case tf: False => tf //println(str)
          case tf: True =>
            tf ++ (s"found a semigroup " + cayleyTable(elements, dyn_op))
        })
    }
    println(s"monoids: $monoids/$tries")
    println(s"groups:  $groups/$tries")
  }

  def findGroupsM() = {
    val elements = genFinite(2)
    var groups = 0
    var tries = 0
    // find 3 element monoids
    for {a <- elements
         b <- elements
         c <- elements
         d <- elements
         dyn_op = ((x: Int, y: Int) => (x, y) match {
           case (_, 0) => x
           case (0, _) => y
           case (1, 1) => a
           case (1, 2) => b
           case (2, 1) => c
           case (2, 2) => d
         })
         dm = DynMagma(() => elements,
                      op1 = dyn_op,
                      member1 = (a: Int) => elements.contains(a)
                      )
         ig <- dm.isGroup(0, x => dm.findInverse(x))
         //table = caleyTable(elements, dyn_op)
         }  {
      val table = cayleyTable(elements, dyn_op)
      tries += 1
      groups += 1
      println(s"found a group " + table + ": ig")
    }
    println(s"groups:  $groups/$tries")
  }

  def testExists() = {
    def f(p: Int): TrueOrFalseBecause = {
      val g = new MultiplicationModP(p)

      def inv(a: Int): Option[Int] = {

        (1 until p).find(b => (a * b) % p == 1)
      }
      g.isGroup(1, inv)
    }
    println(existsM[Int](LazyList.from(3 to 3), f))
    println(existsM[Int](LazyList.from(2 to 10), p => !f(p)))
    //println(existsM[Int](LazyList.from(2 to 10), f))

  }

  def testLogic():Unit = {
    assert((True("x") && True("y")) == True("y"))
    assert((True("x") || True("y")) == True("x"))
    assert((False("x") && False("y")) == False("x"))
    assert((False("x") || False("y")) == False("y"))
  }

  def moreTests():Unit = {
    for {p <- List(2, 3, 5, 7, 11)
         g = new MultiplicationModP(p)
         } {
      def inv(a: Int): Option[Int] = {
        (1 until p).find(b => (a * b) % p == 1)
      }

      assertM(g.isGroup(1, inv))
      assert(g.isGroup(1, inv).toBoolean)
    }
  }

  def main(argv: Array[String]): Unit = {
    testLogic()
    moreTests()
    testModP()
    //findGroups()
    findGroupsM()
    testExists()
  }
}

