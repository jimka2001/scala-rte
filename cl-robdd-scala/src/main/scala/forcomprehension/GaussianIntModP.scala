package forcomprehension

class GaussianIntModP(p: Int) extends Magma[(Int,Int)] {
  override def toString: String = s"GaussianModP($p)"
  val zero = (0,0)
  val one = (1,0)

  def mod(a:Int):Int = {
    ((a % p) + p) % p
  }

  def mod2(a:Int, b:Int):(Int,Int) = {
    (mod(a), mod(b))
  }

  def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(x + u, y + v)
  }

  def subtract(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(x - u, y - v)
  }

  def mult(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val (x, y) = a
    val (u, v) = b
    mod2(u*x - v*y, u*y  + v*x)
  }

  def mult_inv(ab:(Int,Int)):Option[(Int,Int)] = {
    // a+bi has inverse (a - bi)/(a^2 + b^2) in the
    //   case that a^2 + b^2 is non-zero (modulo p)
    val (a,b) = ab
    val denom = mod(a*a + b*b)
    if (denom == 0)
      None
    else
      (1 until p).find(z => mod(z*denom) == 1)
        .map(z => mod2(a*z, -b*z))
  }

  def op(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    add(a,b)
  }

  override def gen(): LazyList[(Int,Int)] = {
    def loop(u:Int,v:Int):LazyList[(Int,Int)] = {
      if (u==p)
        LazyList.empty
      else if (v==p)
        loop(u+1,0)
      else
        (u,v) #:: loop(u,v+1)
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

    for {p <- (2 to 30).view
         m = new GaussianIntModP(p)
         f = Magma.isField[(Int,Int)](m.gen, m.member,
                           m.add, m.mult,
                           a => Some(m.subtract(m.zero, a)),
                           m.mult_inv,
                           m.one, m.zero
                           ).ifFalse(s"$m not a field") &&
           True(s"$m is a field")
         } println(s"$p: $f")
  }
}