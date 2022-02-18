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
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package xymbolyco

import genus._
import org.scalatest.funsuite.AnyFunSuite
import rte._
import xymbolyco.Thompson._


class ThompsonTestSuite  extends AnyFunSuite {
  val num_random_tests:Int = 1000

  test("remove epsilon transitions 1"){
    val t1:SimpleTypeD = SAtomic(classOf[String])
    assert(removeEpsilonTransitions(0, 1,
                                    Seq(makeETransition(0,1),
                                        makeTTransition(0,t1,1)))
           == (0,Seq(0,1),Seq((0,t1,1))))
  }
  test("remove epsilon transitions 2"){
    val a:SimpleTypeD = SAtomic(classOf[String])
    val b:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(1,2),
                                                            makeTTransition(0,a,1),
                                                            makeTTransition(1,b,2)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(1,2),Set((0,a,1),
                           (1,b,2)
                           )))
  }
  test("remove epsilon transitions 3"){
    val a:SimpleTypeD = SAtomic(classOf[String])
    val b:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(0,1),
                                                            makeTTransition(0,a,1),
                                                            makeTTransition(1,b,2)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(2),Set((0,b,2),
                         (0,a,1),
                         (1,b,2))))
  }
  test("remove epsilon transitions 4"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(0,1),
                                                            makeTTransition(0,ts,1),
                                                            makeTTransition(1,ti,2),
                                                            makeTTransition(1,ti,1)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(2),Set((0,ts,1),
                         (0,ti,1),
                         (0,ti,2),
                         (1,ti,1),
                         (1,ti,2))))
  }
  test("remove epsilon transitions 5"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(0,1),
                                                            makeTTransition(0,ts,1),
                                                            makeTTransition(1,ti,2),
                                                            makeTTransition(1,ts,1)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(2),Set((0,ts,1),
                         (0,ti,2),
                         (1,ts,1),
                         (1,ti,2))))
  }
  test("remove epsilon transitions 6"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(1,2),
                                                            makeTTransition(0,ts,1),
                                                            makeTTransition(1,ti,2),
                                                            makeTTransition(1,ti,1)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(1,2),Set((0,ts,1),
                           (1,ti,1),
                           (1,ti,2)
                           )))
  }
  test("remove epsilon transitions 7"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeETransition(1,2),
                                                            makeTTransition(0,ts,1),
                                                            makeTTransition(1,ti,2),
                                                            makeTTransition(1,ts,1)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(1,2),Set((0,ts,1),
                           (1,ts,1),
                           (1,ti,2)
                           )))
  }

  test("complete 1"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val completed = complete(0,
                             Seq(2),
                             Seq((0,ti,1),
                                 (0,ts,2)))
    assert(completed.toSet == Set((0,ti,1),
                                  (0,ts,2),
                                  (0,SNot(SOr(ti,ts)),3),
                                  (1,STop,3),
                                  (2,STop,3),
                                  (3,STop,3)))
  }

  test("determinize 1"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,outs,determinized) = determinize(0,
                                             Seq(1),
                                             Seq((0,ti,1),
                                                 (0,ti,2)))
    // TODO not sure how to test this
    assert(outs.nonEmpty)
    assert(determinized.nonEmpty)
  }
  test("determinize 2"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val (in, outs, determinized) = determinize(0,
                                               Seq(1,3),
                                               Seq((0,ti,1),
                                                   (0,SOr(ti,ts),2),
                                                   (2,ts,3)))
    // TODO not sure how to test this
    assert(outs.nonEmpty)
    assert(determinized.nonEmpty)
  }
  test("determinize 3"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val (in, outs, determinized) = determinize(0,
                                               Seq(1,3),
                                               Seq((0,ti,1),
                                                   (0,SOr(ti,ts),2),
                                                   (2,ts,3),
                                                   (2,ti,5),
                                                   (1,SOr(ti,ts),4)))
    // TODO not sure how to test this
    assert(outs.nonEmpty)
    assert(determinized.nonEmpty)
  }
  test("determinize 4"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val (in, outs, determinized) = determinize(0,
                                               Seq(1,3,4),
                                               Seq((0,ti,1),
                                                   (0,SOr(ti,ts),2),
                                                   (2,ts,3),
                                                   (2,ti,5),
                                                   (1,SOr(ti,ts),4)))
    // TODO not sure how to test this
    assert(outs.nonEmpty)
    assert(determinized.nonEmpty)
  }
  test("determinize 5"){
    determinize(1,
                List(2),
                Seq((1,STop,2),
                    (2,STop,3),
                    (3,STop,3)))
  }
  test("Epsilon"){
    val dfa = constructThompsonDfa(EmptyWord, 42)
    assert(dfa.simulate(Seq()) == Some(42))
    assert(dfa.simulate(Seq(1, 2, 3)) == None)
  }
  test("Sigma"){
    val dfa = constructThompsonDfa(Sigma, 42)
    assert(dfa.simulate(Seq(1)).contains(42))
    assert(dfa.simulate(Seq("hello")).contains(42))
    assert(dfa.simulate(Seq(1, 2, 3)) == None)
  }
  test("SigmaStar"){
    val dfa = constructThompsonDfa(Star(Sigma), 42)

    assert(dfa.simulate(Seq(1)) == Some(42))
    assert(dfa.simulate(Seq("hello")) == Some(42))
    assert(dfa.simulate(Seq(1, 2, 3)) == Some(42))
  }
  test("EmptySet"){
    val dfa = constructThompsonDfa(EmptySet, 42)

    assert(dfa.vacuous() == Some(true))
  }
  test("EmptySetStar"){
    val dfa = constructThompsonDfa(Star(EmptySet), 42)

    assert(dfa.simulate(Seq()) == Some(42))
    assert(dfa.simulate(Seq(1, 2, 3)) == None)
  }
  test("Singleton"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val dfa = constructThompsonDfa(Singleton(ti), 42)

    assert( dfa.simulate(Seq(1)) == Some(42))
    assert(dfa.simulate(Seq(1, 2, 3)) == None)
  }
  test("Star"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])

    val dfa = constructThompsonDfa(Star(Singleton(ti)), 42)

    assert( dfa.simulate(Seq(1)) == Some(42))
    assert( dfa.simulate(Seq(1, 2, 3)) == Some(42))
    assert(dfa.simulate(Seq(1, 2.0, 3)) == None)
  }
  test("Cat") {
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val dfa = constructThompsonDfa(Cat(Singleton(ti),
                                       Singleton(ts)),
                               42)

    assert(dfa.simulate(Seq(1, "hello")) == Some(42))
    assert(dfa.simulate(Seq()) == None)
    assert(dfa.simulate(Seq(1, "hello", "hello")) == None)
    assert(dfa.simulate(Seq(1, 2.0, 3)) == None)
  }
  test("Or"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val dfa = constructThompsonDfa(Or(Singleton(ti),
                                      Singleton(ts)),
                                   42)

    assert(dfa.simulate(Seq(1)) == Some(42))
    assert(dfa.simulate(Seq("hello")) == Some(42))
    assert(dfa.simulate(Seq()) == None)
    assert(dfa.simulate(Seq(1, "hello", "hello")) == None)
    assert(dfa.simulate(Seq(1, 2.0, 3)) == None)
  }
  test("Not"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val dfa = constructThompsonDfa(Not(Or(Singleton(ti),
                                          Singleton(ts))),
                               42)

    assert(dfa.simulate(Seq(1)) == None)
    assert(dfa.simulate(Seq("hello")) == None)
    assert(dfa.simulate(Seq()) == Some(42))
    assert(dfa.simulate(Seq(1, "hello", "hello")) == Some(42))
    assert(dfa.simulate(Seq(1, 2.0, 3)) == Some(42))
  }
  test("And"){
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val ts:SimpleTypeD = SAtomic(classOf[String])
    // begins with int and ends with str
    val dfa = constructThompsonDfa(And(Cat(Singleton(ti), Star(Sigma)),
                                       Cat(Star(Sigma), Singleton(ts))),
                                   42)

    assert(dfa.simulate(Seq(1, "hello", "hello")) == Some(42))
    assert(dfa.simulate(Seq(1, 2.2, 2.2, "hello", "hello")) == Some(42))
    assert(dfa.simulate(Seq(1, 2.2, 2.2)) == None)
    assert(dfa.simulate(Seq(2.2, 2.2, "hello", "hello"))== None)
    assert(dfa.simulate(Seq()) == None)
  }
  test("randomCreate"){
    import rte.Rte.dfaEquivalent
    for {depth <- 0 until 4
         r <- 0 until num_random_tests
         pattern = Rte.randomRte(depth)
         dfa_thompson = constructThompsonDfa(pattern, 42)
         dfa_brzozowski = pattern.toDfa(42)
         }
      // equivalent might return None or Some(true), but need to fail if returns Some(false)
      assert(dfaEquivalent(dfa_brzozowski,dfa_thompson) != Some(false))
  }
}