package scala.collection
import java.util.NoSuchElementException

import scala.annotation.switch

final class Iterator_1[A](a: A) extends AbstractIterator[A] {
  private[this] var _hasNext = true

  def hasNext: Boolean = _hasNext
  def next: A =
    if (_hasNext) {
      _hasNext = false
      a
    } else throw new NoSuchElementException
}

final class Iterator_N[A](as: A*) extends AbstractIterator[A] {
  private[this] var i = 0

  def hasNext: Boolean = i < size
  def next: A =
    if (i >= 0) {
      val a = as(i)
      i += 1; a
    } else throw new NoSuchElementException

  override val size: Int = as.size
}
