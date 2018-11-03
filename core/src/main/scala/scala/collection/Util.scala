package scala.collection

import java.lang.Integer.numberOfLeadingZeros

object Util {

  /** A power of 2 >= `target`.
    */
  def nextPositivePowerOfTwo(target: Int): Int = 1 << -numberOfLeadingZeros(target - 1)
}
