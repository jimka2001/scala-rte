package forcomprehension

class GaussianIntModP(p: Int) extends Magma[(Int,Int)] {
  override def toString: String = s"GaussianModP($p)"
  val zero = (0,0)
  val one = (1,0)

  def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    ((x + u) % p, (y + v) % p)
  }

  def subtract(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    ((x - u) % p, (y - v) % p)
  }

  def mult(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    ((x * u) % p, (y * v) % p)
  }

  def op(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    add(a,b)
  }

  override def gen(): LazyList[(Int,Int)] = {
    def loop(u:Int,v:Int):LazyList[(Int,Int)] = {
      if (u==p && v == p)
        LazyList.empty
      else if (v==p)
        loop(u+1,0)
      else
        (u,v) #:: loop(u+1,v)
    }
    loop(0,0)
  }

  override def equiv(a: (Int,Int), b: (Int,Int)): TrueOrFalseBecause =
    if (a == b)
      True(s"$a equiv $b")
    else
      False(s"$a not equiv $b")

  override def member(a: (Int,Int)): TrueOrFalseBecause = {
    val (x,y) = a
    if (x < 0)
      False(s"$a is not member because $x < 0")
    else if (y < 0)
      False(s"$a is not member because $y < 0")
    else if (x >= p)
      False(s"$a is not a member because $x >= $p")
    else if (y >= p)
      False(s"$a is not a member because $y >= $p")
    else
      True(s"0 <= $x < $p and 0 <= $y < $p")
  }
}

object testGaussianInt {
  def main(argv: Array[String]): Unit = {
    for {p <- 2 to 4
         m = new GaussianIntModP(p)
         f = Magma.isField(m.gen, m.member,
                           m.add, m.mult,
                           locally {
                             def maybe_add_invert(a: (Int, Int)): Option[(Int, Int)] =
                               Some(m.subtract(m.zero, a))

                             maybe_add_invert _
                           },
                           locally {
                             def maybe_mult_invert(a: (Int, Int)): Option[(Int, Int)] =
                               m.gen().find(b => m.mult(a, b) == m.one)

                             maybe_mult_invert _
                           },
                           m.one, m.zero
                           )
         } println(s"$p $f")
  }
}