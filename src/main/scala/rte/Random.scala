package rte

import adjuvant.Adjuvant.{randCase, randElement}
import genus.RandomType
import rte.Rte.{notEmptySeq, notSigma, sigmaStar}

object Random {
  import scala.util.Random
  val random = new Random

  // Here we are passing along an avoidEmpty boolean that will be true when we do not wish there to be
  // any ANDs or NOTs in the RTE, and that any of the SimpleTypeDs will not be empty either
  // this way we are also excluding the EmptySeq, EmptySet, and notSigma explicitly, while also not allowing
  // the recursive call for the randomTypeD to create any EmptyTypes
  def randomRteByDepth(depth: Int, avoidEmpty: Boolean = true): Rte = {

    val rteVector = Vector(notEmptySeq,
                           Sigma,
                           sigmaStar,
                           notSigma,
                           EmptySeq,
                           EmptySet)
    val generators: Seq[() => Rte] = Vector(
      () => rteVector(random.nextInt(rteVector.length - (if (avoidEmpty) 3 else 0))),
      () => Or(randomSeq(depth - 1, random.nextInt(3) + 2, avoidEmpty)),
      () => Star(randomRteByDepth(depth - 1, avoidEmpty)),
      () => Cat(randomSeq(depth - 1, random.nextInt(2) + 2, avoidEmpty)),
      () => Singleton(RandomType.randomType(0, Some(!avoidEmpty))),
      () => And(randomSeq(depth - 1, 2, avoidEmpty)),
      () => Not(randomRteByDepth(depth - 1, avoidEmpty)))

    if (depth <= 0)
      Singleton(RandomType.randomType(0, Some(!avoidEmpty)))
    else {
      val g = generators(random.nextInt(generators.length - (if (avoidEmpty) 2 else 0)))
      g()
    }
  }

  def randomNaiveRteBySize(leaves:Int):Rte = {
    if (leaves == 1)
      randCase(() => EmptySet,
               List((0.1, () => randElement(Seq(Sigma, EmptySeq, EmptySet))),
                    (0.9, () => Singleton(RandomType.randomType(0, false))),
                    ))
    else {
      val leftSize = random.between(1, leaves)
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

  def randomNaiveRteByDepth(depth: Int):Rte = {
    // a naive random Rte of depth <= n is either a leaf or a node whose
    //  children are naive random Rtes of depth <= n-1
    if (depth == 0)
      randCase(() => EmptySet,
               List((0.1, () => randElement(Seq(Sigma, EmptySeq, EmptySet))),
                    (0.9, () => Singleton(RandomType.randomType(0, false))),
                    ))
    else
      randCase(() => EmptySet,
               List((0.1, () => randElement(Seq(Sigma, EmptySeq, EmptySet))),
                    (0.1, () => Singleton(RandomType.randomType(0, false))),
                    (0.2, () => Cat(randomNaiveRteByDepth(depth-1), randomNaiveRteByDepth(depth-1))),
                    (0.2, () => And(randomNaiveRteByDepth(depth-1), randomNaiveRteByDepth(depth-1))),
                    (0.2, () => Or(randomNaiveRteByDepth(depth-1), randomNaiveRteByDepth(depth-1))),
                    (0.1, () => Star(randomNaiveRteByDepth(depth-1))),
                    (0.1, () => Not(randomNaiveRteByDepth(depth-1)))))
  }

  def randomSeq(depth: Int, length: Int, option: Boolean = true): Seq[Rte] = {
    (0 until length).map { _ => randomRteByDepth(depth, option) }
  }

  private val leafTypes = locally{
    import genus.RandomType.interestingTypes
    interestingTypes().filter(td => td.inhabited != Some(false)).to(Vector)
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
                     List((0.1, () => randElement(Seq(Sigma, EmptySeq, EmptySet))),
                          (0.9, () => Singleton(RandomType.randomType(0, false))),
                          ))
        }
    }
    // number of internal nodes is 1 fewer than the number of leaves
    nodeToRte(balancedRandTreeBySize(leaves - 1))
  }

  // Generate an RTE which corresponds (on average) in shape closely to a balanced
  // binary tree.  The goal is to sample languages uniformly rather than sampling
  // syntactic structure of the RTE uniformly.
  //
  // probability_binary: float strictly between 0.0 and 1.0
  def randomTotallyBalancedRteByDepth(probability_binary:Float,
                                      depth:Int) = {
    import adjuvant.{Tree012, Tree012Binary, Tree012Leaf, Tree012Unary}

    def tree012ToRte(tree:Tree012):Rte = {
      tree match {
        case Tree012Leaf() => randCase(() => EmptySet,
                                       List((0.90, () => Singleton(randElement(leafTypes))),
                                            (0.05, () => Sigma),
                                            (0.05, () => EmptySeq)))
        case Tree012Unary(_, child) =>
          // Star, Not
          randElement(Seq((t) => Star(t),
                          (t) => Not(t)))(tree012ToRte(child))

        case Tree012Binary(_, left, right) =>
          // And, Or, Cat
          randCase(() => EmptySet,
                   List((0.70, () => Cat(tree012ToRte(left), tree012ToRte(right))),
                        (0.15, () => Or(tree012ToRte(left), tree012ToRte(right))),
                        (0.15, () => And(tree012ToRte(left), tree012ToRte(right)))))
      }
    }
    // a binary tree of depth=n has 2^n <= m < 2^(n+1) leaves
    // so generate a random number 2^n <= rand < 2^(n+1)
    // i.e 2^n + (rand-int 2^(n+1) - 2^n)
    //    2^n + (rand-int 2^n)
    // since the Tree012 always has empty leaves, we must generate a tree of
    // depth-1, so that plus the leaves it has depth=depth.
    val tree = Tree012.rand(probability_binary, depth-1)
    val rte = tree012ToRte(tree)

    rte
  }
}
