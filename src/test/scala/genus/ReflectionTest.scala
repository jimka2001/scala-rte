// Copyright (c) 2024
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

import adjuvant.AdjFunSuite
import adjuvant.Adjuvant.eql
import genus.RandomType._
import genus.Types._

trait TraitR1
trait TraitR2 extends TraitR1

trait RAnimal
trait RMammal extends RAnimal
class RDog extends RMammal
class RCat extends RAnimal

import scala.language.implicitConversions

class ReflectionTest extends AdjFunSuite {
  import SAtomic.computeSubclassesOf

  test("Number"){
    println(computeSubclassesOf(classOf[Number]))
    assert(computeSubclassesOf(classOf[Number]).nonEmpty)
  }

  test("trait3") {
    println(computeSubclassesOf(classOf[RMammal]))
    assert(computeSubclassesOf(classOf[RMammal]) == Seq(classOf[RDog]))
  }

  test("trait2") {
    println(computeSubclassesOf(classOf[RAnimal]))
    assert(computeSubclassesOf(classOf[RAnimal]).toSet ==
             Set(classOf[RCat],
                 classOf[RDog],
                 classOf[RMammal]))
  }
  test("trait1"){
    print(computeSubclassesOf(classOf[TraitR2]))
    // nothing inherits from TraitR2
    assert(computeSubclassesOf(classOf[TraitR2]) == Seq())
  }

  test("computeSubclassesOf List"){
    assert(computeSubclassesOf(classOf[List[Any]]).nonEmpty,
      s"Failed to compute subclasses of classOf[List[Any]]")
    assert(computeSubclassesOf(classOf[List[Any]]).contains(List(1,2,3).getClass),
      s"List(1,2,3).getClass not in the subClasses of classOf[List[Any]]")
    assert(computeSubclassesOf(classOf[List[Any]]).contains(List.empty.getClass),
      s"List.empty.getClass not in subclasses of classOf[List[Any]]")
  }

  test("reflect.getSubTypesOf Number"){
    assert(computeSubclassesOf(classOf[Number]).toArray.nonEmpty)
  }

  test("Number has instantiatable subclass"){

    import genus.SAtomic.existsInstantiatableSubclass
    assert(computeSubclassesOf(classOf[Number]).nonEmpty)
    assert(existsInstantiatableSubclass(classOf[Number]))
  }
}

