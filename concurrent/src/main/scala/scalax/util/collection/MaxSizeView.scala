package scalax.util.collection

import scala.collection.Iterator
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scalax.util.primitives.{Positive, Size}

/** Wrapped `Iterator` providing materialization with preallocation defined by `maxSize`.
  * This fits well whenever the actual size is equal to or is just slightly below `maxSize`.
  *
  * @param iterator the underlying `Iterator` with a probably unknown size but a maximum size of `maxSize`.
  * @param maxSize the size `iterator` is expected to reach at most.
  *
  * @define ShrinkFactorDoc the divisor `maxSize` is to be divided by to get the estimated `maxSize`
  *                         of the filtered iterator, with a default value of 1 for unchanged.
  */
final class MaxSizeView[+A](val iterator: Iterator[A], private val maxSize: Size):
  def allocationSize: Size =
    val size = iterator.knownSize
    if size >= 0 then Size.trust(size) else maxSize

  private def shrankMaxSize(shrinkFactor: Positive) =
    val a = allocationSize
    if shrinkFactor === 1 then allocationSize
    else if a === 0 then Size.zero
    else (allocationSize / shrinkFactor.asNonNegative).mapTrusted(q => math.max(q, 1))

  /** @param shrinkFactor $ShrinkFactorDoc */
  def collect[B](pf: PartialFunction[A, B], shrinkFactor: Positive = Positive(1)): MaxSizeView[B] =
    new MaxSizeView(iterator.collect(pf), shrankMaxSize(shrinkFactor))

  /** @param shrinkFactor $ShrinkFactorDoc */
  def filter(p: A => Boolean, shrinkFactor: Positive = Positive(1)): MaxSizeView[A] =
    new MaxSizeView(iterator.filter(p), shrankMaxSize(shrinkFactor))

  def map[B](f: A => B): MaxSizeView[B] = new MaxSizeView(iterator.map(f), allocationSize)

  def toBuffer[B >: A]: Buffer[B] = new ArrayBuffer(allocationSize.value) ++= iterator

  def toList: List[A] = iterator.toList

object MaxSizeView:
  def empty[A] = MaxSizeView(Iterator.empty[A], Size.zero)
