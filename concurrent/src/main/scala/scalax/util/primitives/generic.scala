package scalax.util.primitives

import scala.compiletime.{codeOf, error}

/** Support for validated opaque types.
  * @tparam A the underlying primitive type.
  * @tparam O the opaque type.
  */
trait Validated[A <: AnyVal, O]:
  protected inline def fromValid(a: A): O
  protected inline def valid(a: A): Boolean
  protected inline def errMsgSuffix: String

  final inline def apply(a: A): O =
    inline if valid(a) then fromValid(a)
    else error(codeOf(a) + errMsgSuffix + ".")

  transparent inline def from(a: A): Option[O] =
    Option.when(valid(a))(fromValid(a))

  /** @throws ValueOutOfBoundsException if `a` is not `valid`. */
  final inline def unsafe(a: A): O =
    if valid(a) then fromValid(a)
    else throw new ValueOutOfBoundsException(a, errMsgSuffix)

/** Support for opaque types with a limited value range.
  * @tparam A the underlying primitive type.
  * @tparam O the opaque type.
  */
trait Limited[A <: AnyVal, O]:
  inline def lowerLimit: O
  inline def upperLimit: O
  inline def lt(a: O, b: O): Boolean

  protected[scalax] inline def underlying(a: O): A
  protected inline def fromValid(a: A): O
  protected inline def validIncrement(a: A): A
  protected inline def unsafeAdd(a: A, b: A): A

  inline def incr(a: O): O =
    if lt(a, upperLimit) then fromValid(validIncrement(underlying(a)))
    else throw LimitOverflowException

  inline def added(a: O, b: O): O =
    val sum = fromValid(unsafeAdd(underlying(a), underlying(b)))
    if lt(upperLimit, sum) || lt(sum, a) then throw LimitOverflowException
    else sum

extension [A <: AnyVal, O](a: O)(using limited: Limited[A, O])
  protected[util] inline def underlying: A = limited.underlying(a)

  inline def <(b: O): Boolean = limited.lt(a, b)

  /** @throws LimitOverflowException if the result exceeds `upperLimit`. */
  inline def incr: O = limited.incr(a)

  /** @throws LimitOverflowException if the sum exceeds `upperLimit`. */
  inline def +(summand: O): O = limited.added(a, summand)

trait LimitedInt[O] extends Limited[Int, O]:
  final protected inline def validIncrement(a: Int): Int    = a + 1
  final protected inline def unsafeAdd(a: Int, b: Int): Int = a + b

trait LimitedLong[O] extends Limited[Long, O]:
  final protected inline def validIncrement(a: Long): Long = a + 1
  final protected inline def unsafeAdd(a: Long, b: Long): Long = a + b

private class ValueOutOfBoundsException(value: AnyVal, cause: String) extends Exception(s"Value $value $cause.")

private object LimitOverflowException extends Exception
