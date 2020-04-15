package scalax.collection

object Compat {
  type IterableOnce[A] = TraversableOnce[A]

  trait CompatTraversable[+A] extends scala.collection.Traversable[A] {
    final override def foreach[U](f: A => U): Unit = autarkicForeach(f)
    protected def autarkicForeach[U](f: A => U): Unit
  }

  implicit final class IterableEnrichments[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to[MSet]
  }

  implicit final class MSetEnrichments[A](val self: MSet[A]) extends AnyVal {
    def filterInPlace(p: A => Boolean): self.type = {
      if (self.nonEmpty) {
        val array       = self.toArray[Any]
        val arrayLength = array.length
        var i           = 0
        while (i < arrayLength) {
          val elem = array(i).asInstanceOf[A]
          if (!p(elem)) {
            self -= elem
          }
          i += 1
        }
      }
      self
    }
  }

  trait Growable[A] {
    def addOne(elem: A): this.type
    @inline def +=(elem: A): this.type = addOne(elem)
  }

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

  trait ExtBitSet {
    protected def writeReplace(): Any = throw new UnsupportedOperationException()
  }
}
