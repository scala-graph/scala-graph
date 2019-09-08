package scalax.collection.generator.parameters

import scala.util.Random

abstract class RandomRange(min: Int, max: Int, uniform: Boolean) {
  require(min <= max)
  val span            = max - min + 1
  val halfSpan        = span.toFloat / 2
  def mean            = min + (span - 1).toFloat / 2
  private val r, sign = new Random
  def draw = if (uniform) r.nextInt(span) + min
  else ((r.nextGaussian + 1 + min) * halfSpan).toInt
  def drawHalf = {
    val d = draw
    val even =
      if (d % 2 == 0) d
      else d + (if (sign.nextInt(2) == 1) 1 else -1)
    even / 2
  }
  def includes(i: Int) = i >= min && i <= max
}

/** The arity of edges to be generated.
  */
case class EdgeArityRange(min: Int, max: Int, uniform: Boolean = true) extends RandomRange(min, max, uniform) {
  require(min >= 2)
}

/** Edge arity for non-hyperedges equaling to 2.
  */
object EdgeArity2 extends EdgeArityRange(2, 2)

/** The node degrees to be generated.
  *
  * @param min the minimum degree nodes should have.
  * @param max the maximum degree nodes should have with high probability.
  * @param uniform `true` for uniform, `false` for normal distribution of node degrees.
  */
case class NodeDegreeRange(min: Int, max: Int, uniform: Boolean = true) extends RandomRange(min, max, uniform) {
  require(min >= 0)
}

case class MultiplicityRange(min: Int, max: Int, uniform: Boolean = true) extends RandomRange(min, max, uniform) {
  require(min >= 1)
}

object NoMultipleEdges extends MultiplicityRange(1, 1)
