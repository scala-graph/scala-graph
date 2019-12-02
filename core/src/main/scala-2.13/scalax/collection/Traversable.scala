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

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.mutable.Builder
import scala.collection.{Factory, IterableFactory, IterableOnce}

/** Substitute for Scala 2.12 `Traversable` to continue support for collections that cannot implement `hasNext`/`next` easily.
  * The methods of Scala 2.13's `IterableOnce` are implemented in terms of `foreach`.
  *
  * Some less frequently used or not meaningful methods are not implemented intentionally.
  * In case you need any of these, use `foreach` to avoid an intermediate collection
  * or convert this collection to your favorite `Iterable` that will provide the method you are missing.
  *
  * All methods returning a collection take an implicit parameter `factory` for your preferred collection type.
  * The default collection type is set to `Vector`.
  */
trait Traversable[+A] {

  def foreach[U](f: A => U): Unit

  /** The expected maximum size. You should override this whenever known. */
  protected def sizeHint: Int

  /** The factory that will be used by default to compute standard library collection results. */
  final def defaultFactory = Vector

  final def isTraversableAgain: Boolean = true

  // Map Operations

  final def map[B, CC[_]](f: A => B)(implicit factory: IterableFactory[CC] = defaultFactory): CC[B] =
    withBuilder(factory)(b => for (x <- this) b += f(x))

  final def flatMap[B, CC[_]](f: A => IterableOnce[B])(implicit factory: IterableFactory[CC] = defaultFactory): CC[B] =
    withBuilder(factory)(b => for (x <- this) b ++= f(x))

  final def collect[B, CC[_]](pf: PartialFunction[A, B])(
      implicit factory: IterableFactory[CC] = defaultFactory): CC[B] =
    withBuilder(factory)(b => foreach(pf.runWith(b += _)))

  // Conversions

  final def to[CC[_]](factory: IterableFactory[CC]): CC[A @uV] = withBuilder[A, CC](factory)(b => this foreach b.+=)

  final def to[C](factory: Factory[A, C]): C = {
    val b = factory.newBuilder
    b.sizeHint(sizeHint)
    this foreach b.+=
    b.result
  }

  // Size info

  @inline final def size: Int = {
    var i = 0
    for (_ <- this) i += 1
    i
  }

  @inline final def isEmpty: Boolean  = headOption.isEmpty
  @inline final def nonEmpty: Boolean = !isEmpty

  // Element Retrieval

  @inline final def head: A = headOption.get

  @inline final def headOption: Option[A] = {
    for (x <- this) return Some(x)
    None
  }

  @inline final def find(p: A => Boolean): Option[A] = {
    for (x <- this)
      if (p(x)) return Some(x)
    None
  }

  final def collectFirst[B, CC[_]](pf: PartialFunction[A, B])(
      implicit factory: IterableFactory[CC] = defaultFactory): Option[B] = {
    val sentinel: scala.Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any] {
      def apply(a: A) = this
    }
    for (x <- this) {
      val r = pf.applyOrElse(x, sentinel)
      if (r.asInstanceOf[AnyRef] ne sentinel) return Some(r.asInstanceOf[B])
    }
    None
  }

  // Subcollections

  @inline final def filter[CC[_]](p: A => Boolean)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] =
    filterImpl(p, isFlipped = false, factory)

  @inline final def filterNot[CC[_]](p: A => Boolean)(
      implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] =
    filterImpl(p, isFlipped = true, factory)

  private def filterImpl[CC[_]](p: A => Boolean, isFlipped: Boolean, factory: IterableFactory[CC]): CC[A @uV] =
    withBuilder(factory) { b =>
      for (x <- this)
        if (p(x) != isFlipped) b += x
    }

  @inline final def take[CC[_]](n: Int)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] = slice(0, n)

  @inline final def drop[CC[_]](n: Int)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] =
    if (n <= 0) to(factory)
    else slice(n, Int.MaxValue)

  final def slice[CC[_]](from: Int, until: Int)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] =
    math.max(from, 0) pipe { from =>
      def block(b: Builder[A, CC[A]]): Unit =
        if (until > from) {
          var i = 0
          for (x <- this) {
            if (i >= from) b += x
            i += 1
            if (i >= until) return
          }
        }
      withBuilder(factory, math.min(until - from, sizeHint))(block)
    }

  final def takeWhile[CC[_]](p: A => Boolean)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] = {
    def block(b: Builder[A, CC[A]]): Unit =
      for (x <- this) {
        if (!p(x)) return
        b += x
      }
    withBuilder(factory)(block)
  }

  final def dropWhile[CC[_]](p: A => Boolean)(implicit factory: IterableFactory[CC] = defaultFactory): CC[A @uV] =
    withBuilder(factory) { b =>
      var go = false
      for (x <- this) {
        if (!go && !p(x)) go = true
        if (go) b += x
      }
    }

  // Subdivisions

  final def scanLeft[B, CC[_]](z: B)(op: (B, A) => B)(implicit factory: IterableFactory[CC] = defaultFactory): CC[B] =
    withBuilder(factory) { b =>
      var acc = z
      b += acc
      for (x <- this) { acc = op(acc, x); b += acc }
    }

  final def span[CC[_]](p: A => Boolean)(
      implicit factory: IterableFactory[CC] = defaultFactory): (CC[A @uV], CC[A @uV]) =
    withBuilders(factory) {
      case (l, r) =>
        var toLeft = true
        for (x <- this) {
          toLeft = toLeft && p(x)
          (if (toLeft) l else r) += x
        }
    }

  final def splitAt[CC[_]](n: Int)(implicit factory: IterableFactory[CC] = defaultFactory): (CC[A @uV], CC[A @uV]) =
    withBuilders(factory, n, sizeHint - n) {
      case (l, r) =>
        var i = 0
        for (x <- this) {
          (if (i < n) l else r) += x
          i += 1
        }
    }

  // Element Conditions

  @inline final def forall(p: A => Boolean): Boolean = {
    for (x <- this)
      if (!p(x)) return false
    true
  }

  @inline final def exists(p: A => Boolean): Boolean = {
    for (x <- this)
      if (p(x)) return true
    false
  }

  @inline final def count(p: A => Boolean): Int = {
    var i = 0
    for (x <- this) if (p(x)) i += 1
    i
  }

  // Folds

  final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this) result = op(result, x)
    result
  }

  @inline final def fold[B >: A](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  final def reduceLeft[B >: A](op: (B, A) => B): B = {
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

  @inline final def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = if (isEmpty) None else Some(reduceLeft(op))
  @inline final def reduce[B >: A](op: (B, B) => B): B                   = reduceLeft(op)
  @inline final def reduceOption[B >: A](op: (B, B) => B): Option[B]     = reduceLeftOption(op)

  // Specific Folds

  @inline final def sum[B >: A](implicit num: Numeric[B]): B     = if (isEmpty) num.zero else reduce(num.plus)
  @inline final def product[B >: A](implicit num: Numeric[B]): B = if (isEmpty) num.one else reduce(num.times)

  @inline final def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    reduceLeft(ord.min)
  }

  @inline final def minOption[B >: A](implicit ord: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(min(ord))

  final def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    reduceLeft(ord.max)
  }

  @inline final def maxOption[B >: A](implicit ord: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(max(ord))

  final def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
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

  @inline final def maxByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(maxBy(f)(cmp))

  final def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
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

  @inline final def minByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] =
    if (isEmpty) None
    else Some(minBy(f)(cmp))

  // Strings

  final def mkString(start: String, sep: String, end: String): String =
    if (isEmpty) start + end
    else addString(new StringBuilder(), start, sep, end).result()

  @inline final def mkString(sep: String): String = mkString("", sep, "")
  @inline final def mkString: String              = mkString("")

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
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

  @inline final def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")
  @inline final def addString(b: StringBuilder): StringBuilder              = addString(b, "")

  // Zippers

  final def zipWithIndex[CC[_]](implicit factory: IterableFactory[CC] = defaultFactory): CC[(A, Int) @uV] =
    withBuilder(factory) { b =>
      var i = 0
      for (x <- this) {
        b += x -> i; i += 1
      }
    }

  // builder utilities

  private def withBuilder[B, CC[_]](factory: IterableFactory[CC], size: Int = sizeHint)(
      block: Builder[B, CC[B]] => Unit): CC[B] = {
    val b = factory.newBuilder[B]
    b.sizeHint(size)
    block(b)
    b.result()
  }

  private def withBuilders[B, CC[_]](factory: IterableFactory[CC], lSize: Int = sizeHint, rSize: Int = sizeHint)(
      block: (Builder[B, CC[B]], Builder[B, CC[B]]) => Unit): (CC[B], CC[B]) = {
    val l, r = factory.newBuilder[B]
    l.sizeHint(size)
    r.sizeHint(size)
    block(l, r)
    (l.result(), r.result())
  }
}

/** Explicit instantiation of the trait to reduce class file size in subclasses. */
abstract class AbstractTraversable[+A] extends Traversable[A]
