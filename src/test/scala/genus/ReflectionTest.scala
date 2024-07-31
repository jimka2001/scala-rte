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

import scala.language.implicitConversions

class ReflectionTest extends AdjFunSuite {


  test("reflect.getSubTypesOf List"){
    val reflect = new org.reflections.Reflections(classOf[List[_]])
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.nonEmpty)
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List(1,2,3).getClass))
    assert(reflect.getSubTypesOf(classOf[List[Any]]).toArray.contains(List.empty.getClass))
  }

  test("reflect.getSubTypesOf Number"){
    val reflect = new org.reflections.Reflections()
    assert(reflect.getSubTypesOf(classOf[Number]).toArray.nonEmpty)
  }

  test("Number has instantiatable subclass"){

    import genus.SAtomic.{existsInstantiatableSubclass, reflections}
    assert(reflections.getSubTypesOf(classOf[Number]).toArray.toList.nonEmpty)
    assert(existsInstantiatableSubclass(classOf[Number]))
  }

  test("Number not empty"){
    //println(classOf[Number])
    assert(SAtomic(classOf[Number]) != SEmpty)
  }

  test("reflection Number"){
    // this tests that reflection has properly installed the sublcass relationships to Number
    assert(SEql(1).subtypep(SAtomic(classOf[Number])) == Some(true))
    assert(SAtomic(classOf[Number]).inhabited == Some(true))
    assert(SMember(1,2,3).subtypep( SAtomic(classOf[Number])) == Some(true))
    assert(SAtomic(classOf[java.lang.Double]).subtypep(SAtomic(classOf[Number])) == Some(true))
  }
}

