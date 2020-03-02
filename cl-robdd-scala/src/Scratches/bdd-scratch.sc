import bdd._
import bdd.Bdd._
import bdd.GraphViz._

dotProgram

withNewBddHash{
  Bdd(3).bddView(false,"sample 3")
  Xor(3,-2).bddView(true,"sample or")
  Xor(1,And(2,-1),Or(1,2,3)).bddView(false,"var args")

  println(And(Or(1,2),Or(-2,3)).toDnf("X"))

}
