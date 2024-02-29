package heavybool
import HeavyBool._

object Relations {
  def isReflexive[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    forallM[T](gen, (x:T) =>
      forallM[T]( gen, (y:T) => HeavyBool(rel(x,y))))
      .conjTrue(Map("reason" -> "reflexive"))
      .conjFalse(Map("reason" -> "not reflexive"))
  }

  def isSymmetric[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    def hrel(a:T, b:T) = HeavyBool(rel(a,b))
    forallM[T](gen, (x:T) =>
      forallM[T]( gen, (y:T) => hrel(x,y) ==> hrel(y,x)))
      .conjTrue(Map("reason" -> "symmetric"))
      .conjFalse(Map("reason" -> "not symmetric"))
  }

  def isTransitive[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    def hrel(a:T, b:T) = HeavyBool(rel(a,b))
    forallM[T](gen, (x:T) =>
               forallM[T](gen, (y:T) => {
                 if (rel(x,y))
                   forallM[T](gen, (z:T) =>
                     (hrel(y,z) ==> hrel(x,z)))
                 else
                   HTrue
               }))
      .conjTrue(Map("reason" -> "transitive"))
      .conjFalse(Map("reason" -> "not transitive"))
  }

  def isEquivalence[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    (isSymmetric(gen,rel) && isReflexive(gen,rel) && isTransitive(gen,rel))
      .conjTrue(Map("reason" -> "equivalence"))
      .conjFalse(Map("reason" -> "not equivalence"))
  }

  def main(argv:Array[String]):Unit = {
    println(forallM(LazyList.range(1,20), (a:Int) => HeavyBool(a < 12)))
    println(isReflexive(LazyList.range(1,20), (a:Int, b:Int) => a != b))
    println(isEquivalence(LazyList.range(1,20), (a:Int, b:Int) => a != b))
  }
}
