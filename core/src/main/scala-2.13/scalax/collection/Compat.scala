package scalax.collection

object Compat {
  type AbstractTraversable[+A] = scalax.collection.AbstractTraversable[A]

  implicit final class TraversableEnrichments[A](val self: Traversable[A]) extends AnyVal {
    def toList: List[A] = self.to(List)
    def toMSet: MSet[A] = self.to(MSet)
    def toSet: Set[A]   = self.to(Set)
  }

  implicit final class IterableEnrichments[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to(MSet)
  }

  trait ExtBitSet

  trait Growable[-A]
  trait AddSubtract[A, +This]
  trait InclExcl[A, +This]
}
