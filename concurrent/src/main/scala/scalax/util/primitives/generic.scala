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

    /** @throws LimitUnderflowException if the result would fall below `lowerLimit`. */
    def decr: O

    /** @return the result of `f` as `Some` if valid, otherwise `None`. */
    def mapValidated(f: A => A): Option[O] =
      from(f(limited.value))

    /** @return the result of `f` as `O` without validation. */
    def mapTrusted(f: A => A): O =
      f(limited.value).asInstanceOf[O]

object LimitedIntImpl:
  inline def incr[O](a: O)(using lim: Limited[Int, O]): O =
    if a.value < lim.upperLimit then lim.trust(a.value + 1)
    else throw new LimitOverflowException

  inline def decr[O](a: O)(using lim: Limited[Int, O]): O =
    if a.value > lim.lowerLimit then lim.trust(a.value - 1)
    else throw new LimitUnderflowException

  def addNonNegative[O](a: O, b: O)(using lim: Limited[Int, O]): O = add(a, b, a.value)

  def addPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O = add(a, b, a.incr.value)

  private inline def add[O](a: O, b: O, lowerLimit: Int)(using lim: Limited[Int, O]): O =
    val sum = a.value + b.value
    if sum >= lowerLimit && sum <= lim.upperLimit then lim.trust(sum)
    else throw new LimitOverflowException

  def mulNonNegative[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    if a === 0 then a
    else if b === 0 then b
    else mulPositive(a, b)

  def mulPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    val product = a.value * b.value
    if product >= a.value && product <= lim.upperLimit then lim.trust(product)
    else throw new LimitOverflowException

final private[scalax] class ValueOutOfBoundsException(value: AnyVal, cause: String)
    extends Exception(s"Value $value $cause.")

final private[scalax] class LimitOverflowException  extends Exception
final private[scalax] class LimitUnderflowException extends Exception

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
