import bdd.{And, Bdd, BddFalse, BddTrue, Or}
import bdd.GraphViz._

dotProgram

Bdd.withNewBddHash {
  Bdd(1)
  println(BddTrue)
  println(BddFalse)
  println(Or(Bdd(1),Bdd(2)).toDnf())
  println(Or(Bdd(1),Bdd(2)).toDnf())
  println(And(Or(Bdd(1),Bdd(2)),Or(Bdd(1),Bdd(-2))).toDnf())
  println(And(Or(1,2),Or(147,-23)).toDnf("A"))

  And(Or(Bdd(1),Bdd(2)),Or(3,-4)).bddView(true,"sample")
}