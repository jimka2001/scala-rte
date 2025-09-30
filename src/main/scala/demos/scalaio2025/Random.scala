package demos.scalaio2025

import genus.RandomType.interestingTypes
import adjuvant.Adjuvant.{biasedGaussian, randCase, randElement}
import rte.Rte
import rte.{And, Cat, EmptySeq, EmptySet, Not, Or, Sigma, Singleton, Star}
import genus.{SOr, SimpleTypeD}

object Random {
  import scala.util.Random
  val random = new Random
  // compute vector of types which are either inhabited or indeterminate, excluding provably empty
  val inhabitedTypes:Vector[SimpleTypeD]  = interestingTypes().filter(td => td.inhabited != Some(false))
  // compute approx n^2 / 2 union types, all possible non-trivial unions of inhabitedTypes
  val unionTypes:Vector[SimpleTypeD] = for{i <- inhabitedTypes.indices.toVector
                      td1 = inhabitedTypes(i)
                      j <- inhabitedTypes.indices
                      if i < j
                      td2 = inhabitedTypes(j)
                      } yield SOr(td1, td2)
  val inhabitedLeaves:Vector[Rte] = Vector(Sigma, EmptySeq) ++ (inhabitedTypes ++ unionTypes).map(td => Singleton(td))

  def randomNaiveRteBySizeEdge(leaves:Int):Rte = {
    randomNaiveRteBySize(leaves, (n) => biasedGaussian(1, n))
  }

  def randomNaiveRteBySizeMid(leaves:Int):Rte = {
    randomNaiveRteBySize(leaves, (n)=> {
      if (n == 2 || n == 3)
        1
      else
        random.between(1, n / 2) + random.between(1, n / 2)
    })
  }

  private def randomNaiveRteBySize(leaves:Int, pivot:Int => Int):Rte = {
    if (leaves == 1) {
      randElement(inhabitedLeaves)
    } else {
      val leftSize = pivot(leaves)
      assert( leftSize < leaves)
      lazy val left = randomNaiveRteBySize(leftSize, pivot)
      lazy val right = randomNaiveRteBySize(leaves - leftSize, pivot)
      lazy val mid = randomNaiveRteBySize(leaves, pivot)
      randCase(() => EmptySet,
               List(
                 (0.3, () => Cat(left, right)),
                 (0.3, () => And(left,right)),
                 (0.3, () => Or(left,right)),
                 (0.05, () => Star(mid)),
                 (0.05, () => Not(mid))))
    }
  }


  def randomTotallyBalancedRteBySize(leaves:Int):Rte = {
    import adjuvant.Adjuvant.{Node, InternalNode, LeafNode, balancedRandTreeBySize}
    val probability_binary:Double = 0.9
    def nodeToRte(node:Node):Rte = {
      if (random.between(0.0, 1.0) > probability_binary)
        // Star, Not
        randElement(Seq((t) => Star(t),
                        (t) => Not(t)))(nodeToRte(node))
      else
        node match {
          case InternalNode(_,left,right) =>
            randElement(Seq((a:Rte,b:Rte)=>Cat(a,b),
                            (a:Rte,b:Rte)=>And(a,b),
                            (a:Rte,b:Rte)=>Or(a,b)))(nodeToRte(left), nodeToRte(right))
          case LeafNode() =>
            randElement(inhabitedLeaves)
        }
    }
    // number of internal nodes is 1 fewer than the number of leaves
    nodeToRte(balancedRandTreeBySize(leaves - 1))
  }

}
