package scalax.collection

// TODO is this an appropriate name? question also applies to Growable, AddSubtract, InclExcl
object Compat {
  implicit final class ToExts[A](val self: Iterable[A]) extends AnyVal {
    def toMSet: MSet[A] = self.to(MSet)
  }

  trait ExtBitSet

  trait Growable[-A]
  trait AddSubtract[A, +This]
  trait InclExcl[A, +This]
}
