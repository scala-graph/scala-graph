package scala.util.compat

/** Used for cross compilation, this is close to Scala 3's scala.util.boundary that does not work for Scala 2.13.
  */
final class Boundary {
  import Boundary.*

  private val label = new Boundary.Label

  def break[T](value: T): Nothing = throw new Break(label, value)

  def apply[T](body: (T => Nothing) => T): T =
    try body(break[T])
    catch {
      case ex: Break[T] @unchecked =>
        if (ex.label eq label) ex.value
        else throw ex
    }
}

object Boundary {
  def boundary: Boundary = new Boundary

  final private class Break[T] private[Boundary] (val label: Label, val value: T)
      extends RuntimeException(
        /*message*/ null, /*cause*/ null, /*enableSuppression=*/ false, /*writableStackTrace*/ false
      )

  final private class Label
}
