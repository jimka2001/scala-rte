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
    val completed = complete(Seq((0,ti,1),
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
}