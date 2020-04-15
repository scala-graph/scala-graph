package scalax.collection

object Compat {
  type IterableOnce[A] = scala.collection.IterableOnce[A]

  type CompatTraversable[+A] = scalax.collection.ForeachBasedDetachingIterable[A]

  implicit final class IterableEnrichments[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to(MSet)
  }

  object MSetEnrichments

  trait Growable[-A]
  trait AddSubtract[A, +This]
  trait InclExcl[A, +This]

  trait ExtBitSet
}
