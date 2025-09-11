// Copyright (c) 2021 EPITA Research and Development Laboratory
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

package genus

import scala.language.implicitConversions
import java.lang
import scala.annotation.tailrec

import genus.Types.{evenType, oddType}

object RandomType {

  // the following classes have no instantiatable subclass
  trait Trait1

  trait Trait2

  trait Trait3 extends Trait2

  abstract class Abstract1

  abstract class Abstract2 extends Trait3

  sealed abstract class ADT_abstr

  class ADT1 extends ADT_abstr

  class ADT2 extends ADT_abstr

  class ADT3 extends ADT_abstr

  trait Trait1X

  trait Trait2X // has subclass Trait3X which has subclass Abstract2X

  trait Trait3X extends Trait2X // has subclass Abstract2X

  abstract class Abstract1X // has subclass Class1X

  abstract class Abstract2X extends Trait3X

  class Class1X extends Abstract1X

  class Class2X extends Abstract2X

  // allow implicit conversions from c:Class[_] to AtomicType(c)
  //    thus allowing Types such as classOf[java.lang.Integer] && !SEql(0)
  //    classOf[A] && classOf[B]
  implicit def class2type(c: Class[_]): SimpleTypeD = SAtomic(c)


  // The following definitions are defined as functions rather than
  //   simply as values, because the SAtomic constructor caches
  //   objects depending on the world view.

  def mystery(a:Any):Boolean = {
    // This function returns false, but the purpose of the function is to
    //  model a predicate for which we do not know whether it is satisfiable.
    false
  }
  def interestingTypes(): Vector[SimpleTypeD] = Vector(
    STop,
    SEmpty,
    SMember(1, 2, 3, 4),
    SMember(4, 5, 6),
    SEql(0),
    SEql(1),
    SEql(-1),
    SEql(true),
    SEql(false),
    SMember(false, true),
    evenType(),
    oddType(),
    SSatisfies(mystery, "mystery"),
    SMember("a", "b", "c"),
    SAtomic(classOf[lang.Number]),
    SAtomic(classOf[String]),
    SAtomic(classOf[Boolean]),
    SAtomic(classOf[Integer]),
    SAtomic(classOf[Trait1]),
    SAtomic(classOf[Trait2]),
    SAtomic(classOf[Trait3]),
    SAtomic(classOf[Abstract1]),
    SAtomic(classOf[Abstract2]),
    SAtomic(classOf[Trait1X]),
    SAtomic(classOf[Trait2X]),
    SAtomic(classOf[Trait3X]),
    SAtomic(classOf[Abstract1X]),
    SAtomic(classOf[Abstract2X]),
    SAtomic(classOf[Class1X]),
    SAtomic(classOf[Class2X]),
    SAtomic(classOf[ADT1]),
    SAtomic(classOf[ADT2]),
    SOr(SAtomic(classOf[ADT1]), SAtomic(classOf[ADT2]), SAtomic(classOf[ADT3])),
    SAtomic(classOf[ADT_abstr])
    )

  // The idea of this variable is (if true) the certain sample values
  //   can be used to aid in logical reasoning.  For example, we don't know in
  //   general if SSatisfies(f) and SSatisfies(g) are disjoint.  However, if a
  //   witness value, w, is provided such that f(w) = g(w) = true, then we know
  //   that both types are inhabited and moreover that they are NOT disjoint.
  //   Disadvantage of this approach is that the set of sample values is necessarily
  //   small, and run-time checks take more time.
  //   Whether such a check is beneficial is a matter of ongoing research.
  //   The purpose of the interesting-values-implication branch is to
  //   pursue this investigation.
  val interestingValuesAsImplications:Boolean = true

  def getInterestingValues(asImplication:Boolean = interestingValuesAsImplications):Set[Any] = {
    if (asImplication)
      interestingValues.toSet
    else
      Set[Any]()
  }

  val interestingValues: Vector[Any] = Vector(
    -1, -2, 0, 1, 2, 3, 4, 5, 6,
    1L, 0L, -1L, 1000L, 1000000L, // these values cause problems reported in issue #7
    3.14, 2.17, -math.sqrt(2),
    3.14d, 2.17d,
    3.14f, 2.17f, -3.14f, -2.17f,
    'a', 'b', 'c',
    true, false,
    "a", "b", "c", "d", "",
    new Class1X,
    new Class2X,
    new ADT1,
    new ADT2,
    new ADT3
    )

  // here we have 3 functions called randomType, however all three take different arguments in addition to
  // the depth of the random type that needs to be generated.


  // the first function takes as an argument depth ( to be used recursively) and a "filter"
  // the filter is an option [Boolean] which can take Some(true),Some( false), and None
  // when the filter is set to Some(true) every type will be accepted and there are no restrictions
  // when the filter is set to some (false ) the only types that are accepted are those that are not proven to be uninhabited
  // when the filter is set to None, the on ly types that are accepted are those that are proven to be inhabited ( which means
  // that the types that are indeterminately inhabited will be rejected, and those that are proven to be uninhabited will also be rejected)
  //this function then calls the 2nd function if the filter is set to be different than Some(true) with a filter function


  def randomType(depth: Int, filter: Option[Boolean]): SimpleTypeD = {
    if (filter.contains(false)) {
      RandomType.randomType(depth, a => !a.inhabited.contains(false), true)
    }
    else if (filter.isEmpty) {
      RandomType.randomType(depth, a => a.inhabited.contains(true), true)
    }
    else {
      RandomType.randomType(depth)
    }
  }

  // this function still takes a depth for the simpleTypeD, an "avoid" boolean, and a filter function that will be given as an argument
  // the function takes a simpleTypeD as an argument and return a boolean according to some restrictions. While the boolean is not true
  // the second function will call itself.
  def randomType(depth: Int, filter: SimpleTypeD => Boolean, avoid: Boolean): SimpleTypeD = {
    @tailrec
    def recur(): SimpleTypeD = {
      val td = randomType(depth, avoid)
      if (filter(td))
        td
      else {
        recur()
      }
    }
    recur()
  }

  // the final function that is in charge of generating a random SimpleTypeD.
  // the `avoid` boolean, when true, will avoid using nots and ands to compound types, and will also ensure that the
  // selected type/compound type will not be uninhabited.
  // when creating compound types, we will randomly select one of the "and or not" keyword to combine types, when the
  // `avoid` boolean is set to true, it will only select the Or keyword
  def randomType(depth: Int, avoid: Boolean = false): SimpleTypeD = {
    val random = new scala.util.Random
    val maxCompoundSize = 2
    val generators: Seq[() => SimpleTypeD] = Vector(
      () => SOr(0 until 1 + random.nextInt(maxCompoundSize) + random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1, avoid) }: _*),
      // always at least one argument of SAnd and SOr
      () => SAnd(0 until 1 + random.nextInt(maxCompoundSize) + random.nextInt(maxCompoundSize) map { _ => randomType(depth - 1, avoid) }: _*),
      () => SNot(randomType(depth - 1, avoid))
      )
    if (depth <= 0) {
      interestingTypes()(random.nextInt(interestingTypes().length))
    }
    else {
      val g = generators(random.nextInt(generators.length - (if (!avoid) {
        2
      } else 0)))
      g()
    }
  }
}
