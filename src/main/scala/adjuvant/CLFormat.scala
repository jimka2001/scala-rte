// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package adjuvant

import clojure.java.api.Clojure
import clojure.lang.{IFn, RT}


object ClojureEval {

  private val loadString: IFn =
    Clojure.`var`("clojure.core", "load-string")

  def apply(code: String): AnyRef = {
    loadString.invoke(code)
  }
}


object CLFormat {

  private val require: IFn =
    Clojure.`var`("clojure.core", "require")

  require.invoke(Clojure.read("clojure.pprint"))

  private val clFormatFn: IFn =
    Clojure.`var`("clojure.pprint", "cl-format")

  def apply(fmt: String, args: Any*): String = {
    val allArgs = (null +: fmt +: args).map(_.asInstanceOf[AnyRef])
    val seq = RT.seq(allArgs.toArray)
    clFormatFn.applyTo(seq).asInstanceOf[String]
  }

  ClojureEval(
    """(defmethod print-method Object [v ^java.io.Writer w]
           (.write w (.toString v)))"""
    )

  def main(argv:Array[String]):Unit = {
    println(CLFormat("Hello ~A!", "world"))

    println(CLFormat("""~&~
                      expecting t1 <: (or t1 t2)~@
                      t1=~A~@
                      t2=~A~@
                      depth=~A~%~@
                      reps=~S~@
                      reos=~A""",
                     100, 200, 300, List(1,2,3,400), List(1,2,3,500)))
  }
}