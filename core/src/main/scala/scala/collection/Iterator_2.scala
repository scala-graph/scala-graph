package scala.collection

import java.util.NoSuchElementException

final class Iterator_2[A](a_1: A, a_2: A) extends AbstractIterator[A] {
  private[this] var i = 0

  def hasNext: Boolean = i < 2

  def next: A = {
    i += 1
    if (i == 1) a_1
    else if (i == 2) a_2
    else throw new NoSuchElementException
  }

  override val size: Int = 2
}
