package demos.scalaio2025

import genus.RandomType.interestingTypes
import adjuvant.Adjuvant.{biasedGaussian, randCase, randElement}
import rte.Rte
import rte.{EmptySet,Sigma,EmptySeq,Singleton,Cat,Or,And,Star,Not}

object Random {
  import scala.util.Random
  val random = new Random
  val inhabitedTypes = interestingTypes().filter(td => td.inhabited != Some(false))

  def randomNaiveRteBySize(leaves:Int):Rte = {
    if (leaves == 1)
      randCase(() => EmptySet,
               List((0.1, () => randElement(Seq(Sigma, EmptySeq
                                                // , EmptySet
                                                ))),
                    (0.9, () => Singleton(randElement(inhabitedTypes)))
                    ))
    else {
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


  def randomTotallyBalancedRteBySize(probability_binary:Float,
                                     leaves:Int):Rte = {
    import adjuvant.Adjuvant.{Node, InternalNode, LeafNode, balancedRandTreeBySize}

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
            randCase(() => EmptySet,
                     List((0.1, () => randElement(Seq(Sigma, EmptySeq// , EmptySet
                                                      ))),
                          (0.9, () => Singleton(randElement(inhabitedTypes)))
                          ))
        }
    }
    // number of internal nodes is 1 fewer than the number of leaves
    nodeToRte(balancedRandTreeBySize(leaves - 1))
  }

}
