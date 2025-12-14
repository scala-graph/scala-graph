package scalax.util.primitives

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

  def subNonNegative[O](a: O, b: O)(using lim: Limited[Int, O]): O = subtract(a, b, a.value)

  def subPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O = subtract(a, b, a.decr.value)

  private inline def subtract[O](a: O, b: O, upperLimit: Int)(using lim: Limited[Int, O]): O =
    val diff = a.value - b.value
    if diff >= lim.lowerLimit && diff <= upperLimit then lim.trust(diff)
    else throw new LimitUnderflowException
