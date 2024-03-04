package heavybool
import HeavyBool._

object Relations {
  def isReflexive[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    // every element relates to itself
    forallM("x", gen){(x:T) => HeavyBool(rel(x,x))}
      .annotate("reflexive")
  }

  def isSymmetric[T](gen:LazyList[T], rel:(T,T)=>Boolean) = {
    def hrel(a: T, b: T) = HeavyBool(rel(a, b))

    forallM("x", gen) { (x: T) =>
      forallM("y", gen) { (y: T) => hrel(x, y) ==> hrel(y, x) }
    }.annotate("symmetric")
  }

  def isTransitive[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    def hrel(a:T, b:T) = HeavyBool(rel(a,b))
    forallM("x", gen){ (x:T) =>
      forallM("y", gen){ (y:T) => {
        if (rel(x, y))
          forallM("z", gen) { (z: T) =>
            hrel(y, z) ==> hrel(x, z)
          }
        else
          HTrue
      }}}.annotate("transitive")
  }

  def isEquivalence[T](gen:LazyList[T], rel:(T,T)=>Boolean):HeavyBool = {
    isSymmetric(gen,rel) && isReflexive(gen,rel) && isTransitive(gen,rel)
  }.annotate("equivalence")

  def main(argv:Array[String]):Unit = {
    println(existsM("a < 12", LazyList.range(1,20)){ (a:Int) => HeavyBool(a < 12)})
    println(isSymmetric(LazyList.range(1,20), (a:Int, b:Int) => a < b))
    println(isEquivalence(LazyList.range(1,20), (a:Int, b:Int) => a < b))
  }
}
