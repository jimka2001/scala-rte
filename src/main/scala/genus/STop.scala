package genus


/** The super type, super type of all types. */
object STop extends STerminal {
  override def toString = "STop"

  override def toLaTeX(): String = "\\Sigma"

  override def toDot(): String = "STop"

  override def toMachineReadable(): String = "STop"

  override def typep(a: Any): Boolean = true

  override protected def inhabitedDown: Some[Boolean] = Some(true)

  override protected def disjointDown(t: SimpleTypeD): Option[Boolean] = {
    t match {
      case SEmpty => Some(true)
      case _ => Some(false)
    }
  }

  override protected def subtypepDown(t: SimpleTypeD): Option[Boolean] = {
    // STop is only a subtype of itself, or anything which is equivalent to itself.
    SNot(t).inhabited match {
      case None => None
      case Some(true) => Some(false) // if not(t) is inhabited then t is not STop
      case Some(false) => Some(true) // if not(t) is NOT inhabited, then t is STop
    }
  }

  // comparing STop to itself must return false
  override def cmpToSameClassObj(t: SimpleTypeD): Boolean = false

  override lazy val sampleValues:Set[Any] = genus.RandomType.getInterestingValues()
}
