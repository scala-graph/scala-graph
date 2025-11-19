package scalax.util.primitives

import scala.compiletime.{codeOf, error}

/** Support for opaque types with a limited value range.
  * @tparam A the underlying primitive type.
  * @tparam O the opaque type.
  */
trait Limited[A <: AnyVal, O]:
  inline def lowerLimit: O
  inline def upperLimit: O

  protected inline def lt(a: O, b: O): Boolean
  protected inline def valid(a: A): Boolean
  protected inline def unsafeIncrement(a: A): O
  protected inline def errMsgSuffix: String

  final inline def apply(a: A): O =
    inline if valid(a) then a.asInstanceOf[O]
    else error(codeOf(a) + errMsgSuffix + ".")

  final inline def from(a: A): Option[O] =
    Option.when(valid(a))(a.asInstanceOf[O])

  /** @throws ValueOutOfBoundsException if `a` is not valid. */
  final inline def unsafe(a: A): O =
    if valid(a) then a.asInstanceOf[O]
    else throw ValueOutOfBoundsException(a, errMsgSuffix)

  /** @throws LimitOverflowException if `a` is not valid. */
  final protected[primitives] inline def unsafeOp(a: A): O =
    if valid(a) then a.asInstanceOf[O]
    else throw LimitOverflowException

  extension (a: O)
    inline def value: A                 = a.asInstanceOf[A]
    inline infix def ===(b: A): Boolean = a.asInstanceOf[A] == b
    inline def <(b: O): Boolean         = lt(a, b)
    inline infix def +(b: O): O

    /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
    inline def incr: O

trait LimitedInt[O] extends Limited[Int, O]:
  final protected inline def unsafeIncrement(a: Int): O = (a + 1).asInstanceOf[O]
  final protected inline def lt(a: O, b: O): Boolean    = a.asInstanceOf[Int] < b.asInstanceOf[Int]
  extension (limited: O)
    inline infix def +(b: O): O = unsafeOp(limited.asInstanceOf[Int] + b.asInstanceOf[Int])
    inline def incr: O          =
      if lt(limited, upperLimit) then unsafeIncrement(limited.asInstanceOf[Int])
      else throw LimitOverflowException

trait LimitedLong[O] extends Limited[Long, O]:
  final protected inline def unsafeIncrement(a: Long): O = (a + 1).asInstanceOf[O]
  final protected inline def lt(a: O, b: O): Boolean     = a.asInstanceOf[Long] < b.asInstanceOf[Long]
  extension (limited: O)
    inline infix def +(b: O): O = unsafeOp(limited.asInstanceOf[Long] + b.asInstanceOf[Long])
    inline def incr: O          =
      if lt(limited, upperLimit) then unsafeIncrement(limited.asInstanceOf[Long])
      else throw LimitOverflowException

private[scalax] class ValueOutOfBoundsException(value: AnyVal, cause: String) extends Exception(s"Value $value $cause.")

private[scalax] object LimitOverflowException extends Exception
