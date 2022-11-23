package xymbolyco

// FIXME, I get javaws not defined.  Do I really need to add an external library
//    to throw an exception?
import com.sun.javaws.exceptions.InvalidArgumentException
import genus.{RandomType, SimpleTypeD}

// FIXME, this code does not conform to the indentation and spacing recommendations
//    of IntelliJ.   Reformat the file using the IntelliJ reformat-file feature.

object RandomDFAGautier {
  // FIXME, is there a problem with these type parameters?
  //    it looks like Σ, and L are never used.  Why are they defined?
  def RandomDFA[Σ, L, E](num: Int, finals: Int, transitions: Int, transrange: Int,
                         exitValue: E, typedepth: Int, restrictions: Option[Boolean]): Dfa[Any, SimpleTypeD, E] = {
    val r = scala.util.Random
    if(finals+1>num || transitions+transrange>num-1)
    {
      // FIXME I get this exception class not defined,
      //   perhaps remove the if and throw and replace by a call to assert(...)?
      throw new InvalidArgumentException(Array("cannot have more transitions per state than state, or more final states than states"))
    }
    val qids: Set[Int] = Range(0, num).toSet
    // FIXME eliminate this var by creating a library function in adjuvant
    //   which takes a target size, and a call-by-name argument.
    //   the library function should eval the argument enough times to
    //   generate a set of the target size.   That function probably needs
    //   a var, but the code here does not.
    var fids: Set[Int] = Set(num - 1)
    while (fids.size < finals +1) {
      fids += r.nextInt(num)
    }
    if(transrange==0){
      throw new IllegalArgumentException("transrange must be positive")
    }
    val protoDelta = (for {i <- Range(0, num)
                           mylist = randomTransition(transitions,transrange, i, num).toList
                           j <- mylist.indices
                           label = randomTypeD(restrictions, r.nextInt(typedepth))
                           mytuple = (i, label, mylist(j))
                           }
      yield mytuple).toSet
    Dfa.apply(qids, 0, fids, protoDelta, xymbolyco.GenusLabeler(), fids.map { i => i -> exitValue }.toMap)
  }

  // FIXME why is this function here and not in the SimpleTypeD package somewhere?
  def randomTypeD(rest: Option[Boolean], depth: Int): SimpleTypeD = {
    // FIXME find a readable way to eliminate these var's
    var myType = RandomType.randomType(depth)
    var myBool = myType.inhabited
    while (!rest.contains(true) && !myBool.contains(true) && ((rest.isEmpty && (myBool.contains(false) || myBool.isEmpty))
      || (rest.contains(false) && myBool.contains(false))))
    {
      myType = RandomType.randomType(depth)
      myBool = myType.inhabited
    }
    myType
  }

  def randomTransition(t: Int, trrange:Int, i: Int, max: Int): Set[Int] = {

    // FIXME eliminate this var
    var nt = t
    if(trrange!=0){
      nt+= scala.util.Random.nextInt(trrange*2)}

    // FIXME eliminate this var, perhaps use the library function described above
    var mySet: Set[Int] = Set()
    while (mySet.size < nt) {
      mySet += randomVertex(i, max)
    }
    mySet
  }

  def randomVertex(current: Int, max: Int): Int = {
    val r = scala.util.Random

    val temp = r.nextInt(100)
    if (temp < 60) {
      val res = r.nextInt(3) + current+1
      if (res >= max) {
        max - 1
      }
      else res
    }
    else if(temp < 65) {
      current
    }
    else if (temp < 90) {
      r.nextInt(max  - current) + current
    }
    else if(current!=0){
      current - r.nextInt(current)
    }
    else current
  }
}
