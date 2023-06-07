package forcomprehension


case class DynMagma[T](gen1: () => LazyList[T],
                       op1: (T, T) => T,
                       member1: T => Boolean) extends Magma[T] {
  override def toString: String = "dyn"

  def gen(): LazyList[T] = gen1()

  def op(a: T, b: T): T = op1(a, b)

  def member(a: T): TrueOrFalseBecause = member1(a) match {
    case true => True(s"$a is member")
    case false => False(s"$a not a member")
  }
}

