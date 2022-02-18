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
                                    Seq(makeTransition(0,EmptyWord,1),
                                        makeTransition(0,t1,1)))
           == (0,Seq(0,1),Seq((0,t1,1))))
  }
  test("remove epsilon transitions 2"){
    val a:SimpleTypeD = SAtomic(classOf[String])
    val b:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeTransition(1,EmptyWord,2),
                                                            makeTransition(0,a,1),
                                                            makeTransition(1,b,2)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(1,2),Set((0,a,1),
                           (1,b,2)
                           )))
  }
  test("remove epsilon transitions 3"){
    val a:SimpleTypeD = SAtomic(classOf[String])
    val b:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeTransition(0,EmptyWord,1),
                                                            makeTransition(0,a,1),
                                                            makeTransition(1,b,2)))
    assert((in, out.toSet, transitions.toSet) ==
           (0,Set(2),Set((0,b,2),
                         (0,a,1),
                         (1,b,2))))
  }
  test("remove epsilon transitions 4"){
    val ts:SimpleTypeD = SAtomic(classOf[String])
    val ti:SimpleTypeD = SAtomic(classOf[Int])
    val (in,out,transitions) = removeEpsilonTransitions(0, 2,
                                                        Seq(makeTransition(0,EmptyWord,1),
                                                            makeTransition(0,ts,1),
                                                            makeTransition(1,ti,2),
                                                            makeTransition(1,ti,1)))
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
                                                        Seq(makeTransition(0,EmptyWord,1),
                                                            makeTransition(0,ts,1),
                                                            makeTransition(1,ti,2),
                                                            makeTransition(1,ts,1)))
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
                                                        Seq(makeTransition(1,EmptyWord,2),
                                                            makeTransition(0,ts,1),
                                                            makeTransition(1,ti,2),
                                                            makeTransition(1,ti,1)))
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
                                                        Seq(makeTransition(1,EmptyWord,2),
                                                            makeTransition(0,ts,1),
                                                            makeTransition(1,ti,2),
                                                            makeTransition(1,ts,1)))
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

}