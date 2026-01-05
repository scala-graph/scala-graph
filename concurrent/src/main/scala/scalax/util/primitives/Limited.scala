package scalax.util.primitives

/** Support for opaque types with a limited value range.
  * @tparam A the underlying primitive type.
  * @tparam O the opaque type.
  */
trait Limited[A <: AnyVal, O]:
  def lowerLimit: A
  def upperLimit: A
  def lowerLimitAsOpaque: O = lowerLimit.asInstanceOf[O]
  def upperLimitAsOpaque: O = upperLimit.asInstanceOf[O]

  def valid(a: A): Boolean

  final inline def from(a: A): Option[O] =
    Option.when(valid(a))(a.asInstanceOf[O])

  /** @throws ValueOutOfBoundsException if `a` is not valid. */
  final inline def unsafe(a: A): O =
    if valid(a) then a.asInstanceOf[O]
    else throw ValueOutOfBoundsException(a, "is invalid")

  final inline def trust(a: A): O = a.asInstanceOf[O]

  extension (limited: O)
    inline def value: A           = limited.asInstanceOf[A]
    inline def ===(b: A): Boolean = limited.asInstanceOf[A] == b

    def <(b: O): Boolean

    /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
    def incr: O
    def incrTrusted: O

    /** @throws LimitUnderflowException if the result would fall below `lowerLimit`. */
    def decr: O
    def decrTrusted: O

    /** @return the result of `f` as `Some` if valid, otherwise `None`. */
    def mapValidated(f: A => A): Option[O] =
      from(f(limited.value))

    /** @return the result of `f` as `O` without validation. */
    def mapTrusted(f: A => A): O =
      f(limited.value).asInstanceOf[O]

final class ValueOutOfBoundsException(value: AnyVal, cause: String) extends Exception(s"Value $value $cause.")

final class LimitOverflowException  extends Exception
final class LimitUnderflowException extends Exception

trait LimitedArithmetics[A <: AnyVal, O]:
  this: Limited[A, O] =>

  extension (limited: O)
    def >(b: O): Boolean
    def <=(b: O): Boolean
    def >=(b: O): Boolean

    /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
    def +(b: O): O

    /** @throws LimitOverflowException if the result escapes the `valid` range. */
    def *(b: O): O

    /** @throws LimitUnderflowException if the result falls below `lowerLimit`. */
    def -(b: O): O

    def /(b: O): O
