package scalax.util.primitives

object LimitedIntImpl:
  inline def incr[O](a: O)(using lim: Limited[Int, O]): O =
    if a.value < lim.upperLimit then lim.trust(a.value + 1)
    else throw new LimitOverflowException

  inline def decr[O](a: O)(using lim: Limited[Int, O]): O =
    if a.value > lim.lowerLimit then lim.trust(a.value - 1)
    else throw new LimitUnderflowException

  def addNonNegative[O](a: O, b: O)(using lim: Limited[Int, O]): O = add(a, b, a.value)

  def addNonNegativeOrLimit[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    try addNonNegative(a, b)
    catch case _: LimitOverflowException => lim.trust(lim.upperLimit)

  def addPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O = add(a, b, a.incr.value)

  def addPositiveOrLimit[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    try addPositive(a, b)
    catch case _: LimitOverflowException => lim.trust(lim.upperLimit)

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

  def subNonNegativeOrLimit[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    try subNonNegative(a, b)
    catch case _: LimitUnderflowException => lim.trust(lim.lowerLimit)

  def subPositiveOrLimit[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    try subPositive(a, b)
    catch case _: LimitUnderflowException => lim.trust(lim.lowerLimit)

  def subPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O = subtract(a, b, a.decr.value)

  def divPositive[O](a: O, b: O)(using lim: Limited[Int, O]): O =
    val quotient = a.value / b.value
    if quotient > 0 then lim.trust(quotient)
    else throw new LimitUnderflowException

  private inline def subtract[O](a: O, b: O, upperLimit: Int)(using lim: Limited[Int, O]): O =
    val diff = a.value - b.value
    if diff >= lim.lowerLimit && diff <= upperLimit then lim.trust(diff)
    else throw new LimitUnderflowException

  inline def ordering[O](using lim: Limited[Int, O]): Ordering[O] =
    Ordering.Int.asInstanceOf[Ordering[O]]
