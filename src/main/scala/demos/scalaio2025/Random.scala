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

  def randomNaiveRteBySize(leaves:Int):Rte = {
    if (leaves == 1) {
      randElement(inhabitedLeaves)
    } else {
      val leftSize = biasedGaussian(1, leaves)
      lazy val left = randomNaiveRteBySize(leftSize)
      lazy val right = randomNaiveRteBySize(leaves - leftSize)
      lazy val mid = randomNaiveRteBySize(leaves)
      randCase(() => EmptySet,
               List(
                 (0.2, () => Cat(left, right)),
                 (0.2, () => And(left,right)),
                 (0.2, () => Or(left,right)),
                 (0.2, () => Star(mid)),
                 (0.2, () => Not(mid))))
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
