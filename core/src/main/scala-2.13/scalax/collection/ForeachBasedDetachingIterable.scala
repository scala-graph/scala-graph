/* Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 * Copyright Peter Empen
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scalax.collection

import scala.collection.immutable.VectorBuilder
import scala.collection.{Factory, Iterable, IterableFactory}

/** Substitute for Scala 2.12 `Traversable` to continue support for collections that cannot implement `hasNext`/`next` easily.
  * The methods of Scala 2.13's `IterableOnce` are implemented in terms of `foreach`.
  *
  * All methods with a collection result detach from this `Iterable` to `Vector`.
  * This makes sense whenever the `foreach` implementation
  * - causes significant computation overhead or
  * - is not valid for arbitrary subcollections.
  */
trait ForeachBasedDetachingIterable[+A] extends Iterable[A] {

  private type CC[+X]     = Vector[X]
  private type Builder[X] = VectorBuilder[X]

  override def iterableFactory: IterableFactory[Vector] = Vector

  final override def foreach[U](f: A => U): Unit = autarkicForeach(f)
  protected def autarkicForeach[U](f: A => U): Unit

  def iterator: Iterator[A] =
    withBuilder[A](b => for (x <- this) b += x).iterator

  // Map Operations

  final override def map[B](f: A => B): Iterable[B] =
    withBuilder[B](b => for (x <- this) b += f(x))

  final override def flatMap[B](f: A => IterableOnce[B]): Iterable[B] =
    withBuilder[B](b => for (x <- this) b ++= f(x))

  final override def collect[B](pf: PartialFunction[A, B]): Iterable[B] =
    withBuilder[B](b => foreach(pf.runWith(b += _)))

  // Conversions

  final override def to[C1](factory: Factory[A, C1]): C1 = {
    val b = factory.newBuilder
    this foreach b.+=
    b.result
  }

  // Size info

  final override def size: Int = {
    var i = 0
    for (_ <- this) i += 1
    i
  }

  final override def isEmpty: Boolean  = headOption.isEmpty
  final override def nonEmpty: Boolean = !isEmpty

  // Element Retrieval

  final override def head: A = headOption.get

  final override def headOption: Option[A] = {
    for (x <- this) return Some(x)
    None
  }

  final override def find(p: A => Boolean): Option[A] = {
    for (x <- this)
      if (p(x)) return Some(x)
    None
  }

  final override def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    val sentinel: A => Any = _ => this
    for (x <- this) {
      val r = pf.applyOrElse(x, sentinel)
      if (r.asInstanceOf[AnyRef] ne sentinel) return Some(r.asInstanceOf[B])
    }
    None
  }

  // Subcollections

  @inline final override def filter(p: A => Boolean): CC[A]    = filterImpl(p, isFlipped = false)
  @inline final override def filterNot(p: A => Boolean): CC[A] = filterImpl(p, isFlipped = true)

  private def filterImpl(p: A => Boolean, isFlipped: Boolean): CC[A] =
    withBuilder[A] { b =>
      for (x <- this)
        if (p(x) != isFlipped) b += x
    }

  final override def take(n: Int): Iterable[A] = slice(0, n)

  @inline final override def drop(n: Int): CC[A] =
    if (n <= 0) to(Vector)
    else slice(n, Int.MaxValue)

  final override def slice(from: Int, until: Int): CC[A] =
    math.max(from, 0) pipe { from =>
      def block(b: Builder[A]): Unit =
        if (until > from) {
          var i = 0
          for (x <- this) {
            if (i >= from) b += x
            i += 1
            if (i >= until) return
          }
        }
      withBuilder(block)
    }

  final override def takeWhile(p: A => Boolean): CC[A] = {
    def block(b: Builder[A]): Unit =
      for (x <- this) {
        if (!p(x)) return
        b += x
      }
    withBuilder(block)
  }

  final override def dropWhile(p: A => Boolean): CC[A] =
    withBuilder[A] { b =>
      var go = false
      for (x <- this) {
        if (!go && !p(x)) go = true
        if (go) b += x
      }
    }

  // Subdivisions

  final override def scanLeft[B](z: B)(op: (B, A) => B): CC[B] =
    withBuilder[B] { b =>
      var acc = z
      b += acc
      for (x <- this) { acc = op(acc, x); b += acc }
    }

  final override def span(p: A => Boolean): (CC[A], CC[A]) =
    withBuilders[A] {
      case (l, r) =>
        var toLeft = true
        for (x <- this) {
          toLeft = toLeft && p(x)
          (if (toLeft) l else r) += x
        }
    }

  final override def splitAt(n: Int): (CC[A], CC[A]) =
    withBuilders[A] {
      case (l, r) =>
        var i = 0
        for (x <- this) {
          (if (i < n) l else r) += x
          i += 1
        }
    }

  // Element Conditions

  final override def forall(p: A => Boolean): Boolean = {
    for (x <- this)
      if (!p(x)) return false
    true
  }

  final override def exists(p: A => Boolean): Boolean = {
    for (x <- this)
      if (p(x)) return true
    false
  }

  final override def count(p: A => Boolean): Int = {
    var i = 0
    for (x <- this) if (p(x)) i += 1
    i
  }

  // Folds

  final override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this) result = op(result, x)
    result
  }

  final override def reduceLeft[B >: A](op: (B, A) => B): B = {
    var first  = true
    var acc: B = 0.asInstanceOf[B]
    for (x <- this) {
      if (first) {
        acc = x
        first = false
      } else acc = op(acc, x)
    }
    if (first) throw new UnsupportedOperationException("empty.reduceLeft")
    else acc
  }

  @inline final override def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (isEmpty) None else Some(reduceLeft(op))

  @inline final override def reduce[B >: A](op: (B, B) => B): B               = reduceLeft(op)
  @inline final override def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)

  // Specific Folds

  @inline final override def sum[B >: A](implicit num: Numeric[B]): B     = if (isEmpty) num.zero else reduce(num.plus)
  @inline final override def product[B >: A](implicit num: Numeric[B]): B = if (isEmpty) num.one else reduce(num.times)

  @inline final override def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    reduceLeft(ord.min)
  }

  @inline final override def minOption[B >: A](implicit ord: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(min(ord))

  final override def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    reduceLeft(ord.max)
  }

  @inline final override def maxOption[B >: A](implicit ord: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(max(ord))

  final override def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    var maxF: B    = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first      = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.gt(fx, maxF)) {
        maxElem = elem
        maxF = fx
        first = false
      }
    }
    maxElem
  }

  @inline final override def maxByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(maxBy(f)(cmp))

  final override def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    var minF: B    = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first      = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

  @inline final override def minByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(minBy(f)(cmp))

  // Strings

  final override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    val jsb = b.underlying
    if (start.length != 0) jsb.append(start)
    var first = true
    for (x <- this) {
      if (first) first = false else jsb.append(sep)
      jsb.append(x)
    }
    if (end.length != 0) jsb.append(end)
    b
  }

  // Zippers

  final override def zipWithIndex: CC[(A, Int)] =
    withBuilder[(A, Int)] { b =>
      var i = 0
      for (x <- this) {
        b += x -> i; i += 1
      }
    }

  // Builder utilities

  @inline private def withBuilder[B](block: Builder[B] => Unit): CC[B] = {
    val b = new Builder[B]
    block(b)
    b.result()
  }

  private def withBuilders[B](block: (Builder[B], Builder[B]) => Unit): (CC[B], CC[B]) = {
    val l, r = new Builder[B]
    block(l, r)
    (l.result(), r.result())
  }
}
