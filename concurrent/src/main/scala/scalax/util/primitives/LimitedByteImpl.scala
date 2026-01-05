package scalax.util.primitives

object LimitedByteImpl:
  inline def incr[O](a: O)(using lim: Limited[Byte, O]): O =
    if a.value < lim.upperLimit then lim.trust((a.value + 1).toByte)
    else throw new LimitOverflowException

  inline def decr[O](a: O)(using lim: Limited[Byte, O]): O =
    if a.value > lim.lowerLimit then lim.trust((a.value - 1).toByte)
    else throw new LimitUnderflowException
