package forcomprehension

abstract class ModP(p: Int) extends Magma[Int] {
  override def toString: String = s"ModP($p)"

  override def gen(): LazyList[Int] = Magma.genFinite(p - 1)

  override def equiv(a: Int, b: Int): TrueOrFalseBecause =
    if (a == b)
      True(s"$a equiv $b")
    else
      False(s"$a not equiv $b")

  override def member(a: Int): TrueOrFalseBecause =
    if (a < 0)
      False(s"$a is not member because $a < 0")
    else if (a >= p)
      False(s"$a is not a member because $a >= $p")
    else
      True(s"0 <= $a < $p")
}

class AdditionModP(p: Int) extends ModP(p) {
  override def toString: String = s"AdditionModP($p)"

  override def op(a: Int, b: Int): Int = (a + b) % p

  def suggestInverse(a:Int):Option[Int] = {
    Some(0)
  }
}

class MultiplicationModP(p: Int) extends ModP(p) {
  override def toString: String = s"MultiplicationModP($p)"

  override def gen(): LazyList[Int] = {
    super.gen().filter { a => a != 0 }
  }

  override def member(a: Int): TrueOrFalseBecause = {
    if (a <= 0)
      False(s"$a <= 0")
    else
      super.member(a)
  }

  override def op(a: Int, b: Int): Int = (a * b) % p
}
