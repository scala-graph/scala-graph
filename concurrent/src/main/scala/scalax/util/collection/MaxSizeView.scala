package scalax.util.collection

import scala.collection.mutable.ArrayBuffer
import scalax.util.primitives.{Positive, Size}

/** Wrapped `Iterator` providing materialization with preallocation defined by `maxSize`.
  * This is optimal whenever the actual size is probably just slightly below `maxSize`.
  *
  * @param iterator the underlying `Iterator` with a probably unknown size but a maximum size of `maxSize`.
  * @param maxSize the size `iterator` is expected to reach at most.
  */
final class MaxSizeView[+A](val iterator: Iterator[A], private val maxSize: Size):
  private def allocationSize: Size =
    val size = iterator.knownSize
    if size >= 0 then Size.trust(size) else maxSize

  /** @param shrinkFactor the divisor `maxSize` is to be divided by to get the estimated `maxSize`
    *                     of the filtered iterator, with a default value of 1 for unchanged.
    */
  def filter(p: A => Boolean, shrinkFactor: Positive = Positive(1)): MaxSizeView[A] =
    new MaxSizeView(iterator.filter(p), allocationSize / shrinkFactor.asNonNegative)

  def map[B](f: A => B): MaxSizeView[B] = new MaxSizeView(iterator.map(f), allocationSize)

  def toBuffer[B >: A]: ArrayBuffer[B] = new ArrayBuffer(allocationSize.value) ++= iterator

  def toList: List[A] = iterator.toList
