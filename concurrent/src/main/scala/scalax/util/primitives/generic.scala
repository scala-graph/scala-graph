package scalax.util.primitives

/** Support for opaque types with a limited value range.
  * @tparam A the underlying primitive type.
  * @tparam O the opaque type.
  */
trait Limited[A <: AnyVal, O]:
  def lowerLimit: A
  def upperLimit: A

  def valid(a: A): Boolean

  final inline def from(a: A): Option[O] =
    Option.when(valid(a))(a.asInstanceOf[O])

  /** @throws ValueOutOfBoundsException if `a` is not valid. */
  final inline def unsafe(a: A): O =
    if valid(a) then a.asInstanceOf[O]
    else throw ValueOutOfBoundsException(a, "is invalid")

  final inline def trust(a: A): O = a.asInstanceOf[O]

  extension (limited: O)
    inline def value: A                 = limited.asInstanceOf[A]
    inline infix def ===(b: A): Boolean = limited.asInstanceOf[A] == b

    infix def <(b: O): Boolean

    /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
    infix def +(b: O): O

    /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
    def incr: O

protected object LimitedIntImpl:
  inline def incr[O](a: O)(using lim: Limited[Int, O]): O =
    if a.value < lim.upperLimit then (a.value + 1).asInstanceOf[O]
    else throw LimitOverflowException

  def add[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    val sum = a.value + b.value
    if sum > a.value && sum <= lim.upperLimit then sum.asInstanceOf[O]
    else throw LimitOverflowException

private[scalax] class ValueOutOfBoundsException(value: AnyVal, cause: String) extends Exception(s"Value $value $cause.")

private[scalax] object LimitOverflowException extends Exception
