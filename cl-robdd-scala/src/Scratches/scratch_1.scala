
def block[A](body:(A=>Nothing)=>A):A = {
  // CL like block/return, the name of the return() function is provided
  //  by the caller.
  //  Usage:  block{ ret =>  ... ret(someValue) ...}

  import scala.util.control.NoStackTrace

  class NonLocalExit(val data:A) extends Exception with NoStackTrace {}
  def ret(data:A):Nothing = {
    throw new NonLocalExit(data)
  }
  try{
    body(ret)
  }
  catch{
    case nonLocalExit: NonLocalExit => nonLocalExit.data
  }
}

def getKeyFromMap[T,S](key:T,base:Map[T,S]):Option[T] = {
  block { produce =>
    base.map{ case (k:T, _:S) => if (key == k) produce(Some(k)) }
    produce(None)
  }
}

def normalizeSequence[T,S](seq:Seq[T],base:Map[T,S]):Seq[T] = {
  seq.map{x:T =>
    getKeyFromMap(x,base) match {
      case Some(key) => key
      case _ => x
    }
  }
}

val seq = List((1,2),(2,3),(1,2),(1,2),(2,3))
val base = seq.map{ tuple => (tuple -> true)}.toMap
val normalizedBase = normalizeSequence(seq,base)

for { x1 <- normalizedBase
      x2 <- normalizedBase
      if x1 == x2
      } assert (x1 eq x2)
