package scalax.util.primitives

object Log2ValueImpl:
  def mul[K](n: K, k: K)(using log2: Log2Value[K]): K =
    val product = n.value + k.value
    if product <= log2.upperLimit then log2.trust(product.toByte)
    else throw new LimitOverflowException

  def mulO[O, K](n: O, k: K)(using lim: Limited[Int, O], log2: Log2Value[K]): O =
    val product = n.value << k.value
    if product >= n.value && product <= lim.upperLimit then lim.trust(product)
    else throw new LimitOverflowException

  def divO[O, K](n: O, k: K)(using lim: Limited[Int, O], log2: Log2Value[K]): O =
    val quotient = n.value >> k.value
    if quotient >= lim.lowerLimit then lim.trust(quotient)
    else throw new LimitUnderflowException
