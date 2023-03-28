package genus

import org.scalacheck.Gen;

object GenusSpecifications {
  def naiveGenGenus: Gen[SimpleTypeD] = {
    def genInternalNode(left: SimpleTypeD, right: SimpleTypeD) = Gen.frequency(
      (1, SNot(right)), // Arbitrary choice of right
      (1, SAnd(left, right)),
      (1, SOr(left, right))
    )

    // TODO: continue generation of leaves
    def genLeaf() = Gen.frequency(
      (1, SAtomic),
      (1, SEql),
      (1, SMember),
      (1, SSatisfies),
      (1, STop),
      (1, SEmpty),
    )

    for {
      left <- naiveGenGenus
      right <- naiveGenGenus
      v <- Gen.oneOf(genInternalNode(left, right), genLeaf())
    } yield v
  }
}
