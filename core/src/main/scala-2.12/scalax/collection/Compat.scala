package scalax.collection

object Compat {
  implicit final class ToExts[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to[MSet]
  }

  trait ExtBitSet {
    protected def writeReplace(): Any = throw new UnsupportedOperationException()
  }

  trait Growable[A] {
    def addOne(elem: A): this.type
    @inline def +=(elem: A): this.type = addOne(elem)
  }

  // TODO maybe Growable, Shrinkable, Mutable?
  trait AddSubtract[A, +This] {
    def addOne(elem: A): this.type
    def subtractOne(elem: A): this.type
    @inline def +=(elem: A): this.type = addOne(elem)
    @inline def -=(elem: A): this.type = subtractOne(elem)
  }

  trait InclExcl[A, +This] {
    def incl(elem: A): This
    def excl(elem: A): This
    @inline def +(elem: A): This = incl(elem)
    @inline def -(elem: A): This = excl(elem)
  }
}

