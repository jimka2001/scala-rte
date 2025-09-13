// Copyright (c) 2019 EPITA Research and Development Laboratory
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

package bdd

import scala.util.Try

//////////////////////////////
// The logical operations Not, And, Or, AndNot, Xor, Xnor
//////////////////////////////
object Not {
  def apply(b1: Bdd): Bdd = {
    AndNot(BddTrue, b1)
  }
}

sealed abstract class BinaryOperation() {
  def apply():Bdd // 0-args to define in subclass
  def apply(bdd:Bdd):Bdd // 1-arg to define in subclass
  def apply(b1: Bdd, b2: Bdd): Bdd // 2-args to define in subclass
  val commutative:Boolean

  import scala.util.DynamicVariable

  type OP_HASH = scala.collection.mutable.Map[(Bdd, Bdd), Bdd]
  def newHashMap():OP_HASH = {
    new scala.collection.mutable.HashMap
  }
  val maybeOpHash = new DynamicVariable[Option[OP_HASH]](None)

  def memoize(thunk: =>Bdd):Bdd = {
    maybeOpHash.value match {
      case None =>
        maybeOpHash.withValue(Some(newHashMap())) {
          memoize(thunk)
        }
      case Some(_) =>
        thunk
    }
  }
  def memoize(b1:Bdd,b2:Bdd,thunk: => Bdd):Bdd = {
    maybeOpHash.value match {
      case None =>
        maybeOpHash.withValue(Some(newHashMap())) {
          memoize(b1, b2, thunk)
        }
      case Some(hash) =>
        if (commutative) {
          (hash.get((b1, b2)), hash.get((b2, b1))) match {
            case (Some(bdd), _) => bdd
            case (_, Some(bdd)) => bdd
            case (None, None) =>
              hash((b1, b2)) = thunk
              hash((b1, b2))
          }
        }
        else {
          if (!hash.contains((b1, b2)))
            hash((b1, b2)) = thunk
          hash((b1, b2))
        }
    }
  }

  def apply(args: Bdd*): Bdd = apply(args.toList)
  def apply(args:List[Bdd]):Bdd = {
    args match {
      case Nil => apply()
      case bdd::Nil => apply(bdd)
      case _ => memoize(args.reduceLeft{(acc,bdd) => apply(acc,bdd)})
    }
  }
  def bddOp(b1: BddNode, b2: BddNode): Bdd = {
    // println(s"bddOp this=$this  b1=$b1  b2=$b2")

    if (b1.label == b2.label)
      Bdd(b1.label, apply(b1.positive, b2.positive), apply(b1.negative, b2.negative))
    else if (b1.label < b2.label)
      Bdd(b1.label, apply(b1.positive, b2), apply(b1.negative, b2))
    else
      Bdd(b2.label, apply(b1, b2.positive), apply(b1, b2.negative))
  }
}

object And extends BinaryOperation {
  val commutative = true
  def apply():Bdd = BddTrue
  def apply(bdd:Bdd):Bdd = bdd
  def apply(b1: Bdd, b2: Bdd): Bdd = {
    (b1, b2) match {
      case (_, _) if b1 eq b2 => b1
      case (BddTrue, _) => b2
      case (BddFalse, _) => BddFalse
      case (_, BddTrue) => b1
      case (_, BddFalse) => BddFalse
      case (b1: BddNode, b2: BddNode) => memoize(b1,b2,bddOp(b1, b2))
    }
  }
}

object Or extends BinaryOperation {
  val commutative = true
  def apply():Bdd = BddFalse
  def apply(bdd:Bdd):Bdd = bdd
  def apply(b1:Bdd,b2:Bdd):Bdd = {
    //println(s"calculating Or($b1,$b2)")
    (b1,b2) match {
      case (_,_) if b1 eq b2 => b1
      case (BddTrue,_) => BddTrue
      case (BddFalse,_) => b2
      case (_,BddTrue) => BddTrue
      case (_,BddFalse) => b1
      case (b1:BddNode,b2:BddNode) => memoize(b1,b2,bddOp(b1,b2))
    }
  }
}

object AndNot extends BinaryOperation {
  val commutative = false
  def apply():Bdd = sys.error("0-ary AndNot not implemented")
  def apply(bdd:Bdd):Bdd = sys.error("unary AndNot not implemented")
  override def apply(args:List[Bdd]):Bdd = {
    args match {
        // not sure which is better And(head,Not(Or(tail))) or And(head,And(tail.map{b:Bdd => Not(b)}))
      case head::tail if tail.nonEmpty => And(head,And(tail.map{b => Not(b)}))
      case _ => sys.error(s"${args.size}-ary AndNot not implemented")
    }
  }
  def apply(b1:Bdd,b2:Bdd):Bdd = {
    (b1, b2) match {
      case (_, _) if b1 eq b2 => BddFalse
      case (BddTrue, BddFalse) => BddTrue
      case (BddFalse, BddTrue) => BddFalse
      case (BddFalse, _) => BddFalse
      case (_, BddTrue) => BddFalse
      case (b1: BddNode, BddFalse) => b1
      case (BddTrue, b2: BddNode) =>
        memoize(BddTrue, b2,
                Bdd(b2.label, AndNot(BddTrue, b2.positive), AndNot(BddTrue, b2.negative)))
      case (b1: BddNode, b2: BddNode) =>
        memoize(b1, b2,
                bddOp(b1, b2))
    }
  }
}

object Xor extends BinaryOperation {
  val commutative = true
  def apply():Bdd = BddFalse
  def apply(bdd:Bdd):Bdd = bdd
  def apply(b1:Bdd,b2:Bdd):Bdd = {
    (b1,b2) match {
      case (_,_) if b1 eq b2 => BddFalse
      case (BddTrue,_) => Not(b2)
      case (BddFalse,_) => b2
      case (_,BddTrue) => Not(b1)
      case (_,BddFalse) => b1
      case (b1:BddNode,b2:BddNode) => memoize(b1,b2,bddOp(b1,b2))
    }
  }
}

object Xnor extends BinaryOperation {
  val commutative = true
  def apply():Bdd = BddTrue
  def apply(bdd:Bdd):Bdd = bdd
  def apply(b1:Bdd,b2:Bdd):Bdd = {
    (b1,b2) match {
      case (_,_) if b1 eq b2 => BddTrue
      case (BddTrue,_) => b2
      case (BddFalse,_) => Not(b2)
      case (_,BddTrue) => b1
      case (_,BddFalse) => Not(b1)
      case (b1:BddNode,b2:BddNode) => memoize(b1,b2,bddOp(b1,b2))
    }
  }
}

object BinaryOperation {

  def main(argv: Array[String]): Unit = {
    Bdd.withNewBddHash{
      (1 to 10).foreach { _ =>
        assert(Or(BddTrue, BddFalse) == BddTrue)
        assert(Or(BddFalse, BddTrue) == BddTrue)
        assert(Or(BddTrue, BddFalse) == BddTrue)
        assert(Or(BddFalse, BddTrue) == BddTrue)
        assert(Or(BddTrue, BddTrue) == BddTrue)
        assert(Or(BddFalse, BddFalse) == BddFalse)
      }
      AndNot(1,2)
      AndNot(1,2,3)
      AndNot(1,2,3,4)
      AndNot(1,2,3,4,5)
      assert(Try(AndNot(1)).isFailure)
      assert(Try(AndNot()).isFailure)
    }
    Bdd.withNewBddHash {
      println(And(1, 2, 3))
      println(And(1, Or(2, 3, Xor(1, 2, 4), AndNot(3, 4))))
    }
    Bdd.withNewBddHash {
      val bdd3 = Bdd(3)
      val bdd2 = Bdd(2)
      Bdd(1, bdd2, bdd3)
      Bdd(1, bdd2, bdd3)
      Or(1, 2, -3, And(-1, 4), And(2, Not(Or(1, 3))))

      Not(Or(Xor(1, And(-2, -3)),
             AndNot(2, 3)))
      Not(Or(-2,
             3,
             Xor(1, 2, And(-2, -3, 4)),
             AndNot(2, 3)))
    }
  }
}