package demos.scalaio2025

object Mod {
  def mul_mod(a:Int, b:Int, p:Int):Int = {
    (a * b) % p
  }

  def frequencies[A](population:Seq[A]):(Map[A,Int],Map[Int,Set[A]]) = {
    val fr = for{
      (a,as) <- population.groupBy(identity)
    } yield (a, as.length)

    val revmap = fr.foldLeft(Map[Int,Set[A]]())((acc,item) => {
      val (a:A,n:Int) = item
      acc.get(n) match {
        case None => acc + (n -> Set(a))
        case Some(s) => acc + (n -> (s ++ Set(a)))
        }
    })

    (fr, revmap)
  }

}

object ModDemo {

  def main(argv:Array[String]):Unit = {
    val p = 15
    def mul(a:Int, b:Int):Int = Mod.mul_mod(a,b,p)

    val products = for{a <- 0 to p-1
                       b <- 0 to p-1} yield mul(a,b)
    println(products)
    val (fr, revmap) = Mod.frequencies(products)

    println(revmap)
    println(fr)

  }
}