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

package genus

// genus
import NormalForm._

/** The atoms of our type system: a simple type built from a native Scala/Java type.
 *
 * @param ct the class of a Scala or Java type this class will wrap (call it with `classOf[native_type]`)
 */
case class SAtomic(ct: Class[_]) extends SimpleTypeD with TerminalType {
  override def toString:String = {
    val fullName = ct.getName

    val shortName = fullName.dropWhile(_ != '$')
    if (shortName == "")
      fullName
    else
      shortName.drop(1)
  }

  override def typep(a: Any): Boolean = {
    ct.isInstance(a)
  }

  override def inhabited: Some[Boolean] = {
    if (ct.isAssignableFrom(classOf[Nothing]))
      Some(false)
    else
      Some(true)
  }

  // AtomicType(ct: Class[_])
  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    import java.lang.reflect.Modifier
    def isFinal(cl: Class[_]): Boolean = {
      Modifier.isFinal(cl.getModifiers)
    }

    def isInterface(cl: Class[_]): Boolean = {
      Modifier.isInterface(cl.getModifiers)
    }

    t match {
      case SEmpty => Some(true)
      case STop => Some(false) // STop is only disjoint with SEmpty, but this != SEmpty
      case SAtomic(tp) =>
        if (tp == ct)
          Some(false)
        else if (ct.isAssignableFrom(tp) || tp.isAssignableFrom(ct))
          Some(false)
        else if (isFinal(ct) || isFinal(tp)) // if either is final
          Some(true)
        else if (isInterface(ct) || isInterface(tp)) // if either is an interface
          Some(false) // TODO justify this choice.
        else // neither is final, and neither is an interface, and neither is a subclass of the other, so disjoint.
          Some(true)
      case _ => super.disjointDown(t)
    }
  }

  // AtomicType(ct: Class[_])
  override def subtypep(s: SimpleTypeD): Option[Boolean] = {
    s match {
      case SEmpty => Some(false)
      case STop => Some(true)
      // super.isAssignableFrom(sub) means sub is subtype of super
      case SAtomic(tp) =>
        Some(tp.isAssignableFrom(ct))

      case SMember(_@_*) =>
        Some(false) // no member type exhausts all the values of an Atomic Type

      case SEql(_) =>
        Some(false)

      case SNot(_) =>
        super.subtypep(s)

      case SOr(tp@_*) =>
        if (tp.exists(x => subtypep(x).contains(true)))
        // A < A union X,
        // and A < B => A < B union X
        // if this happens to be a subtype of one of the components of the union type, then it is
        //   a subtype of the union type, but if this fails to be a subtype of every component
        //   we can not reach any conclusion
          Some(true)
        else if (tp.forall(x => disjoint(x).contains(true)))
          Some(false)
        else
          super.subtypep(s)

      case SAnd(tp@_*) =>
        if (tp.forall(x => subtypep(x).contains(true)))
          Some(true)
        else if (tp.forall(x => disjoint(x).contains(true)))
          Some(false)
        else
          super.subtypep(s)

      case SCustom(_,_) =>
        super.subtypep(s)
    }
  }

  // AtomicType(ct: Class[_])
  override def canonicalizeOnce(nf:Option[NormalForm]=None): SimpleTypeD = {
    SAtomic(ct)
  }

  // AtomicType(ct: Class[_])
  override def cmp(td:SimpleTypeD):Boolean = {
    if (this == td)
      false
    else td match {
      // this <= td ?
      case SAtomic(cl) => s"$ct" < s"$cl"
      case _ => super.cmp(td)
    }
  }
}

/** The AtomicType object, implementing an apply method in order to
 * deal with EmptyType and TopType construction.
 */
object SAtomic {
  def apply(ct: Class[_]): SimpleTypeD = {
    if (ct == classOf[Nothing]) SEmpty
    else if (ct == classOf[Any]) STop
    else new SAtomic(ct)
  }
}