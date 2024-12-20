// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package adjuvant

import scala.annotation.{tailrec, unused}


object CLcompat {

  def prog1[A, B](val1: A, code2: => B): A = {
    code2 // eval for side effect
    val1
  }

  def prog2[A, B, C](@unused _unused: A, val2: B, code2: => C): B = {
    code2 // eval for side effect
    val2
  }

  @tailrec
  def every[A, B](L1: List[A], L2: List[B])(f: (A, B) => Boolean): Boolean = {
    (L1, L2) match {
      case (a :: as, b :: bs) => f(a, b) && every(as, bs)(f)
      case (_, _) => true
    }
  }

  def merge[A](clauses1: List[A], clauses2: List[A], clauseLess: (A, A) => Boolean): List[A] = {
    // Merge two lists which are already in sorted order, according to clauseLess
    //   into a new list which is likewise in sorted order.
    @tailrec
    def loop(clauses1: List[A], clauses2: List[A], acc: List[A]): List[A] = {
      (clauses1, clauses2) match {
        case (Nil, Nil) => acc.reverse
        case (a :: as, Nil) => loop(as, Nil, a :: acc)
        case (Nil, b :: bs) => loop(Nil, bs, b :: acc)
        case (a :: as, b :: bs) if clauseLess(a, b) => loop(as, b :: bs, a :: acc)
        case (a :: as, b :: bs) => loop(a :: as, bs, b :: acc)
      }
    }

    loop(clauses1, clauses2, Nil)
  }

  def mapcan[A1, A2, B](f: (A1, A2) => List[B], L1: List[A1], L2: List[A2]): List[B] = {
    L1.lazyZip(L2).flatMap(f)
  }

  def block[A](body: (A => Nothing) => A): A = {
    // CL like block/return, the name of the return() function is provided
    //  by the caller.
    //  Usage:  block{ ret =>  ... ret(someValue) ...}

    // extending Exception with NoStackTrace prevents throwing the
    // exception from computing the stacktrace and storing the information.
    // We don't need a stacktrace because the purpose of this exception
    // is simply to perform a non-local exit.
    import scala.util.control.NoStackTrace

    class NonLocalExit(val data: A, val ident: (A => Nothing) => A) extends Exception with NoStackTrace {}

    def ret(data: A): Nothing = {
      throw new NonLocalExit(data, body)
    }

    try {
      body(ret)
    }
    catch {
      case nonLocalExit: NonLocalExit if nonLocalExit.ident eq body => nonLocalExit.data
    }
  }

  def main(argv: Array[String]): Unit = {

  }
}
