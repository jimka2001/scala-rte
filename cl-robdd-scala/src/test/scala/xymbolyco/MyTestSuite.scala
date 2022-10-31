// Copyright (©) 2021 EPITA Research and Development Laboratory
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction,
// including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NON-INFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package xymbolyco


import com.sun.javaws.exceptions.InvalidArgumentException
import genus._
import org.scalatest.funsuite.AnyFunSuite
import xymbolyco.GraphViz._

class MyTestSuite extends AnyFunSuite {
  /*
    test("statistics tests") {

        SAtomic.worldView.withValue(OpenWorldView){
          val transitions= Array.fill(4)(Array.fill(6)(0))

          val num_random_tests = 1000
        for {depth <- 3 until 4
             r <- 0 until num_random_tests
             pattern = Rte.randomRte(depth)} {
          println(/*(depth - 1) * num_random_tests + */r)
          println(pattern.toString)

          val data = check(pattern, 1, 1)
          //val dataCanonicalize = check(canonicalizedPattern, 1, 1)

          var mylist = data("dfa_thompson").protoDelta.toList

          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(0)(0) += 1
            }
            else if (temp.contains(false))
            {
              transitions(0)(1)+=1
            }
            else if (temp.contains(true)) {
              transitions(0)(2) += 1
            }
          }
          mylist = data("min_thompson").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(0)(3) += 1
            }
            else if (temp.contains(false))
            {
              transitions(0)(4)+=1
            }
            else if (temp.contains(true)) {
              transitions(0)(5) += 1
            }
          }

          mylist = data("dfa_brzozowski").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(1)(0) += 1
            }
            else if (temp.contains(false))
            {
              transitions(1)(1)+=1
            }
            else if (temp.contains(true)) {
              transitions(1)(2) += 1
            }
          }
          mylist = data("min_brzozowski").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(1)(3) += 1
            }
            else if (temp.contains(false))
            {
              transitions(1)(4)+=1
            }
            else if (temp.contains(true)) {
              transitions(1)(5) += 1
            }
          }
         /* mylist = dataCanonicalize("dfa_thompson").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(2)(0) += 1
            }
            else if (temp.contains(false))
            {
              transitions(2)(1)+=1
            }
            else if (temp.contains(true)) {
              transitions(2)(2) += 1
            }
          }
          mylist = dataCanonicalize("min_thompson").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(2)(3) += 1
            }
            else if (temp.contains(false))
            {
              transitions(2)(4)+=1
            }
            else if (temp.contains(true)) {
              transitions(2)(5) += 1
            }
          }
          mylist = dataCanonicalize("dfa_brzozowski").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(3)(0) += 1
            }
            else if (temp.contains(false))
            {
              transitions(3)(1)+=1
            }
            else if (temp.contains(true)) {
              transitions(3)(2) += 1
            }
          }
          mylist = dataCanonicalize("min_brzozowski").protoDelta.toList
          for (i <- mylist.indices) {
            val temp = mylist(i)._2.inhabited
            if (temp.isEmpty){
              transitions(3)(3) += 1
            }
            else if (temp.contains(false))
            {
              transitions(3)(4)+=1
            }
            else if (temp.contains(true)) {
              transitions(3)(5) += 1
            }
          }*/
        }
          for(i<-Range(0,2))
          {
            for(j<-Range(0,6))
            {
              println(transitions(i)(j))
            }
          }
      }
    }
    */
  /*
  test("statistics tests2") {
      val transitions= Array.fill(6)(0)

      val num_random_tests = 10000
      for {depth <- 4 until 5
           r <- 0 until num_random_tests
           pattern = Rte.randomRte(depth)} {
        println(/*(depth - 1) * num_random_tests + */r)
        println(pattern.toString)
        val data = check(pattern, 1, 1)
        val mylist = data("dfa_thompson").protoDelta.toList
        for(i<-mylist.indices)
        {
          if(mylist(i)._2.inhabited.contains(false))
          {
            if(data("dfa_thompson").Q.size > data("dfa_brzozowski").Q.size)
            {
              transitions(0)+=1
            }
            else if(data("dfa_thompson").Q.size==data("dfa_brzozowski").Q.size)
            {
              transitions(1)+=1
            }
            else
            {
              transitions(2)+=1
            }
            if(data("min_thompson").Q.size > data("min_brzozowski").Q.size)
            {
              transitions(3)+=1
              for(i<-Range(0,6))
              {
                println(transitions(i))
              }
            }
            else if(data("min_thompson").Q.size==data("min_brzozowski").Q.size)
            {
              transitions(4)+=1
              for(i<-Range(0,6))
              {
                println(transitions(i))
              }
            }
            else
              {
              transitions(5)+=1
              for(i<-Range(0,6))
              {
                println(transitions(i))
              }
            }
          }
        }
      }
    for(i<-Range(0,6))
    {
      println(transitions(i))
    }
  }

   */
  /*
  test("b")
  {
    val myarray = Array.fill(2)(Array.fill(3)(0))
    val num_random_tests = 30000
    for {depth <- 1 until 6
         r <- 0 until num_random_tests
         pattern = Rte.randomRte(depth)
         canonicalizedpattern = pattern.canonicalize
         }
    {
      println((depth-1)*num_random_tests+r)
      val data = Thompson.constructThompsonDfa(pattern,42)
      val data2 = Thompson.constructThompsonDfa(canonicalizedpattern,42)
      if(data.Q.size<10){
      if(data.Q.size == data2.Q.size) {
        myarray(0)(1) += 1
      }
      else if(data.Q.size > data2.Q.size)
      {
        myarray(0)(0)+=1
      }
      else
      {
        myarray(0)(2)+=1
      }}
      else
      {
        if(data.Q.size == data2.Q.size) {
          myarray(1)(1) += 1
        }
        else if(data.Q.size > data2.Q.size)
        {
          myarray(1)(0)+=1
        }
        else
        {
          myarray(1)(2)+=1
        }
      }
      if(r%100==0)
      {
       for(j<-Range(0,2))
       {
         for(i<-Range(0,3))
         {
           println(myarray(j)(i))
         }
       }
      }
    }
  }*/
  test("c") {
    /*
   val rte = Cat(Sigma,Or(And(Singleton(SEql(true)),Singleton(SEql(1))),
                          Star(Singleton(SAtomic(classOf[ADT_abstr]))),
                          Or(Cat(Sigma,Sigma,Star(Sigma)),EmptyWord),EmptySet),
                 Or(Or(Singleton(SAtomic(classOf[Abstract1])),Singleton(SAtomic(classOf[Trait1X])),
                       Singleton(SSatisfies(evenp,"evenp")),Singleton(SAtomic(classOf[Class1X]))),
                    Not(Singleton(SAtomic(classOf[Abstract2X]))),
                    Cat(Singleton(SAtomic(classOf[Trait3])), Singleton(SAtomic(classOf[Trait3])),
                        Singleton(SOr(SAtomic(classOf[ADT1]),SAtomic(classOf[ADT2]),SAtomic(classOf[ADT3])))),
                    Star(Singleton(SEmpty))))
    val rte2= And(Or(EmptySet,Singleton(SMember(false,true)),
                     Or(Singleton(SSatisfies(oddp,"oddp")),Singleton(SMember(1,2,3,4)),
                        Singleton(SSatisfies(evenp,"evenp")),
                        And(Singleton(STop), Singleton(SEql(1)))),
                     Or(Or(Singleton(SAtomic(classOf[Trait2X])),Singleton(SMember(true,false)),
                        Singleton(SAtomic(classOf[ADT1])),Singleton(SEql(true))),
                        Singleton(SOr(SAtomic(classOf[ADT1]),SAtomic(classOf[ADT2]),SAtomic(classOf[ADT3]))),
                                  Star(Singleton(SAtomic(classOf[ADT_abstr]))))))
*/
/*    val rte3 = Not(Or(Or(Singleton(SEql(-1)), Singleton(SSatisfies(oddp, "oddp"))),
                      Or(Singleton(SAtomic(classOf[Number])), Singleton(SSatisfies(evenp, "evenp")),
                         Or(Singleton(SSatisfies(oddp, "oddp")), Singleton(SAtomic(classOf[Trait2X])),
                            Singleton(SAtomic(classOf[Integer])), Singleton(SOr(SAtomic(classOf[ADT1]),
                                                                                SAtomic(classOf[ADT2]), SAtomic(classOf[ADT3]))), EmptySet))))
    */
    //statisticSizePerDepth(100, 4, brz)

    //val data = Profiling.check(rte3,1,1)
  }

  def RandomDFA[Σ, L, E](num: Int, finals: Int, transitions: Int, transrange: Int,
                   exitValue: E, typedepth: Int, restrictions: Option[Boolean]): Dfa[Any, SimpleTypeD, E] = {
    val r = scala.util.Random
    if(finals+1>num || transitions+transrange>num-1)
    {
      throw new InvalidArgumentException(Array("cannot have more transitions per state than state, or more final states than states"))
    }
    val qids: Set[Int] = Range(0, num).toSet
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
                           label = randomTypeD(restrictions, r.nextInt(typedepth - 1) + 1)
                           mytuple = (i, label, mylist(j))
                           }
      yield mytuple).toSet
    new Dfa(qids, 0, fids, protoDelta, xymbolyco.GenusLabeler(), fids.map { i => i -> exitValue }.toMap)
  }

  def randomTypeD(rest: Option[Boolean], depth: Int): SimpleTypeD = {
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
    var nt = t
    if(trrange!=0){
      nt+= scala.util.Random.nextInt(trrange*2)}

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
    else if(temp < 70) {
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
  test("d")
  {
    val dfa = RandomDFA(10,4,4,2,42,4,None)
    dfaView(dfa,"",abbrev = true,Some(""))
    //val rte = Star(Singleton(SAtomic(classOf[Integer]))) //dfa.extract
    //Profiling.check(rte,1,1)
  }
}