package forcomprehension

abstract class Magma[T] {

  import BoolBecause._

  def gen(): LazyList[T]

  def op(a: T, b: T): T

  def member(a: T): BoolBecause

  def equiv(a: T, b: T): BoolBecause = {
    if (a == b)
      True(s"$a == $b")
    else
      False(s"$a != $b")
  }

  def isClosed(): BoolBecause = {
    forallM[T](gen(), { a:T =>
      forallM[T](gen(), { b:T =>
        member(op(a,b)) ++ s"$this not closed because non-member op($a,$b),"
      })
    })
  }

  def isAssociative(): BoolBecause = {
    forallM[T](gen(), { a =>
      forallM[T](gen(), { b =>
        forallM[T](gen(), { c =>
          equiv(op(op(a, b), c),
                op(a, op(b, c))) ++ s"not associative: E.g, $a, $b, $c:"
        })
      })
    })
  }

  def isAbelian(): BoolBecause = {
    forallM[T](gen(), { a =>
      forallM[T](gen(), { b =>
        equiv(op(a, b), op(b, a)) ++ s"not Abelian, e.g., $a, $b,"
      })
    })
  }

  def isIdentity(z: T): BoolBecause = {
    forallM[T](gen(), { a =>
      equiv(op(a, z), a).map( str => s"$z is not the identity because op($a, z)=${op(a,z)}") &&
        equiv(op(z, a), a).map( str => s"$z is not the identity because op(z, $a)=${op(z,a)}")
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

  def isInverter(z: T, invert: T => Option[T]): BoolBecause = {
    forallM[T](gen(), { a =>
      invert(a) match {
        case None => False(s"because $a has no inverse")
        case Some(b) =>
          member(b) &&
            equiv(z, op(a, b)) &&
            equiv(z, op(b, a))
      }
    })
  }

  def isSemiGroup(): BoolBecause = {
    (isClosed() && isAssociative()).map(str =>
                                          s"$this is not a semigroup because $str")
  }

  def isMonoid(z: T): BoolBecause = {
    (isSemiGroup() && isIdentity(z)).map(str =>
                                           s"$this not a monoid because $str")
  }

  def isGroup(z: T, invert: T => Option[T]): BoolBecause = {
    (isMonoid(z) && isInverter(z, invert)).map(str =>
                                                 s"$this not a group because $str")
  }
}

object Magma {

  import BoolBecause._

  // generate a lazy list from 0 to n inclusive
  def genFinite(n: Int): LazyList[Int] = {
    LazyList.from(0 to n)
  }

  def cayleyTable[T](elements: LazyList[T], op: (T, T) => T): String = {
    val header: String = "*|" ++ elements.map(x => s"$x").mkString(" ")
    val divider: String = "-+" ++ elements.map(x => "-").mkString("-")
    "\n" ++ header ++ "\n" ++ divider ++ "\n" ++ elements
      .map(x => elements.map(y => s"${op(x, y)}")
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
      mult.isGroup(1, a => (1 until p).find(b => (a * b) % p == 1)) match {
        case False(str) => println(str)
        case _ => println(s"$mult is a group")
      }
    }
  }

  def randomCayleyTable(n:Int):(Int,Int)=>Int = {
    import scala.util.Random
    // generate a function representing one possible addition table of a
    // magma of n elements, numbers 0 to n-1 with the restriction
    // that 0+z= 0 and z+0 = z
    val tbl = (for{row <- 1 to n-1
                   col <- 1 to n-1 }
    yield (row,col) -> Random.between(0,n)).toMap

    def add(a: Int, b: Int): Int = {
        if (a == 0)
          b
        else if (b == 0)
          a
        else tbl((a,b))
    }

    add
  }

  def randomCayleyTables(n:Int):LazyList[(Int,Int)=>Int] = {
    randomCayleyTable(n) #:: randomCayleyTables(n)
  }

  def allUnitalCayleyTables(n: Int): LazyList[(Int, Int) => Int] = {
    import scala.math.pow
    // generate a lazy sequence of length n^n * (n-1)^2 of functions
    // each representing one possible addition table of a
    // magma of n elements, numbers 0 to n-1 with the restriction
    // that 0+z= 0 and z+0 = z
    def recur(i: Int): LazyList[(Int, Int) => Int] = {
      def add(a: Int, b: Int): Int = {
        if (a == 0)
          b
        else if (b == 0)
          a
        else {
          // interpret i as a (n-1)x(n-1) digit number in base n, i.e., composed
          // of digits 0 ... (n-1)
          // arrange these digits into an (n-1) x (n-1) matrix, and return
          // the element at row a, column b.
          // i.e. the digit in position (1 based) (a-1)*(n-1)+(b-1)
          // we know that a>0 and b>0 because of the first two clauses
          // of the if/else-if.
          val pos = (a - 1) * (n - 1) + (b - 1)
          i / pow(n, pos).toInt % n
        }
      }

      if (i < 0)
        LazyList()
      else
        add _ #:: recur(i - 1)
    }
    // n ^ ((n-1)^2)
    // e.g., if n=8, we want to know how many 49 digit base 8 numbers exist?
    // i.e., 8^((8-1)*(8-1))
    // if n=2, how many 1 digit base 2 numbers = 1^2 = 2
    // if n=3, how many 4 digit base 3 numbers = 3^4 = 81
    // if n=4, how many 9 digit base 4 numbers = 4^9 = 262144
    recur(pow(n, (n - 1) * (n - 1)).toInt - 1)
  }

  def allCayleyTables(n: Int): LazyList[(Int, Int) => Int] = {
    import scala.math.pow

    def recur(i: Int): LazyList[(Int, Int) => Int] = {
      def add(a: Int, b: Int): Int = {
          // interpret i as a n x n  digit number in base n, i.e., composed
          // of digits 0 ... (n-1)
          // arrange these digits into an n x n matrix, and return
          // the element at row a, column b.
          // i.e. the digit in position (0 based) a*n+b
          val pos = a * n + b
          i / pow(n, pos).toInt % n
      }

      if (i < 0)
        LazyList()
      else
        add _ #:: recur(i - 1)
    }

    recur(pow(n, n*n).toInt - 1)
  }

  def findGroups(n:Int) = {
    val elements = genFinite(n-1)
    var groups = 0
    var abeliangroups = 0
    var monoids = 0
    var abelianmonoids = 0
    var semigroups = 0
    var abeliansemigroups = 0
    var tries = 0
    var abelians = 0
    // find n element monoids
    for {dyn_op <- allCayleyTables(n)
         dm = DynMagma(() => elements,
                       op1 = dyn_op,
                       member1 = (a: Int) => elements.contains(a)
                       )
         ab = dm.isAbelian()
         } {
      tries += 1
      if (ab.toBoolean)
        abelians += 1
      (dm.isGroup(0, x => dm.findInverse(x)) match {
        case tf: False => tf // println(str)
        case tf: True =>
          groups += 1
          monoids += 1
          semigroups += 1
          if (ab.toBoolean) {
            abeliangroups += 1
            abelianmonoids += 1
            abeliansemigroups += 1
          }
          print("found"
                  + (if (ab.toBoolean) " Abelian" else "")
                  + s" group ${cayleyTable(elements, dyn_op)} ")
          tf ++ (s"found a group " + cayleyTable(elements, dyn_op))
      }) ||
        (dm.isMonoid(0) match {
          case tf: False => tf //println(str)
          case tf: True =>
            monoids += 1
            semigroups += 1
            if (ab.toBoolean) {
              abelianmonoids += 1
              abeliansemigroups += 1
            }
            tf ++ (s"found a monoid " + cayleyTable(elements, dyn_op))
        }) ||
        (dm.isSemiGroup() match {
          case tf: False => tf //println(str)
          case tf: True =>
            semigroups += 1
            if (ab.toBoolean) {
              abeliansemigroups += 1
            }
            tf ++ (s"found a semigroup " + cayleyTable(elements, dyn_op))
        })
    }
    println(s"magmas: $tries   abelian=$abelians")
    println(s"semigroups: $semigroups   abelian=$abeliansemigroups")
    println(s"monoids: $monoids   abelian=$abelianmonoids")
    println(s"groups:  $groups   abelian=$abeliangroups")
  }

  def findGroupsM(n:Int) = {
    val elements = genFinite(n-1) // [0, 1, 2] size 3
    var groups = 0
    var tries = 0

    for { add <- allUnitalCayleyTables(n)
          _ = (tries += 1)
          dm = DynMagma(() => elements,
                        op1 = add,
                        member1 = (a: Int) => elements.contains(a)
                        )
          ig <- dm.isGroup(0, x => dm.findInverse(x))
          }  {
      val table = cayleyTable(elements, add)
      groups += 1
      println(s"found a group " + table + s": $ig")
    }
    println(s"groups:  $groups/$tries")
  }

  def testExists() = {
    def f(p: Int): BoolBecause = {
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

  def testCayleyTables(n:Int):Unit = {
    for {add <- allUnitalCayleyTables(n)
         str = cayleyTable(genFinite(n-1), add)
         } println(str)
  }

  def isRing[T](gen: () => LazyList[T],
                member: T => TrueOrFalseBecause,
                add: (T, T) => T, mult: (T, T) => T,
                invert: T => Option[T],
                one: T, zero: T): TrueOrFalseBecause = {
    val ma = DynMagma[T](gen, add, x => member(x).toBoolean)
    val mb = DynMagma[T](gen, mult, x => member(x).toBoolean)
    ma.isGroup(zero, invert) &&
      ma.isAbelian() &&
      mb.isMonoid(one) &&
      forallM[T](gen(), { a =>
        forallM[T](gen(), { b =>
          forallM[T](gen(), { c =>
            // left distribute
            ma.equiv(mult(a, add(b, c)),
                     add(mult(a, b), mult(a, c))).ifFalse(s"$a does not left-distribute across ($b+$c)") &&
            // right distribute
              ma.equiv(mult(add(b,c),a),
                       add(mult(b,a),mult(c,a))).ifFalse(s"$a does not right-distribute across ($b+$c)")
          })
        })
      })
  }

  def isField[T](gen: () => LazyList[T],
                 member: T => TrueOrFalseBecause,
                 add: (T, T) => T, mult: (T, T) => T,
                 add_invert: T => Option[T], mult_invert: T => Option[T],
                 one: T, zero: T): TrueOrFalseBecause = {
    val ma = DynMagma[T](gen, add, x => member(x).toBoolean)
    lazy val mb = DynMagma[T](gen, mult, x => member(x).toBoolean)
    def non_zero_gen():LazyList[T] = {
      gen().filter(_ != zero)
    }
    !ma.equiv(one,zero) &&
      mb.isAbelian().ifFalse("not Abelian") &&
      isRing(gen, member,
             add, mult, add_invert,
             one, zero).ifFalse(s"not a ring") &&
      mb.isInverter(non_zero_gen, one, mult_invert).ifFalse("invalid inversion")
  }

  def main(argv: Array[String]): Unit = {
    //testCayleyTables(2)
    //testCayleyTables(3)
    //testLogic()
    //moreTests()
    //testModP()
    findGroups(2)
    //findGroupsM(2)
    //findGroupsM(3)
    //findGroupsM(4)
    //testExists()
  }
}

