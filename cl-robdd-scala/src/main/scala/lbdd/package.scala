// Copyright (c) 2020 EPITA Research and Development Laboratory
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


package object lbdd {
  // We use a package object in order to define a type alias for lazy nodes and to
  //  define lazification function. An Option is used in order to make possible an
  //  empty middle node. If the middle node is not empty, then it contains a function
  //  returning a lazy binary decision diagram.


  case class lazyNode(f: Option[() => LBdd], negation: Boolean = false) {
    def apply(): LBdd = {
      f.get()
    }

    def isEmpty: Boolean = {
      f.isEmpty
    }

    def nonEmpty: Boolean = {
      f.nonEmpty
    }

    def get: () => LBdd = {
      f.get
    }
  }

  object None extends lazyNode(scala.None)

  def Some(f: () => LBdd): lazyNode = {
    lazyNode(scala.Some(f))
  }


  /** lazification function
    * @param    b a lazy binary decision diagram
    * @return   A lazy node constructed from b
    */
  def lazify(b: => LBdd): lazyNode = {
    Some(() => b)
  }

  // Lazy leaves, functional true and false
  val f_false: lazyNode = lazify(LBddFalse)
  val f_true: lazyNode = lazify(LBddTrue)


  def unlazify(b: lazyNode): LBdd = {
    if (b.isEmpty)
      LBddFalse
    else
      b.get()
  }
}
