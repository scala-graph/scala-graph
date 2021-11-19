package scalax

import scala.collection.{AbstractIterable, AbstractIterator, ExtSetMethods}

/** Contains the base traits and objects needed to use '''Graph for Scala'''.
  *
  * See also the
  * [[http://www.scala-graph.org/guides/core-introduction Graph for Scala Core User Guide]].
  *
  * @author Peter Empen
  */
package object collection {

  protected[collection] def NeverUsed = ???

  /** The default filter function for degrees to be included in
    * degree calculation always returning true.
    */
  val AnyDegree = (_: Int) => true

  /** [[scala.collection.Set]] extended by some useful methods in the context of Graph.
    */
  type ExtSet[A] = scala.collection.Set[A] with ExtSetMethods[A]

  protected[scalax] type AnySet[A] = scala.collection.Set[A]

  protected[scalax] type MSet[A] = scala.collection.mutable.Set[A]
  @inline final protected[scalax] def MSet = scala.collection.mutable.Set

  protected[scalax] type MMap[K, V] = scala.collection.mutable.Map[K, V]
  @inline final protected[scalax] def MMap = scala.collection.mutable.Map

  @inline final protected[scalax] def mkIterable[A](it: => Iterator[A]): Iterable[A] = new AbstractIterable[A] {
    override def iterator = it
  }

  /** Adds chaining methods `tap` and `pipe` to `Any`. "Back-ported" from Scala 2.13.
    */
  implicit final class ChainingOps[A](val self: A) extends AnyVal {
    def tap[U](f: A => U): A = { f(self); self }
    def pipe[B](f: A => B): B = f(self)
  }

  implicit final class Iterable$Enrichments(val it: Iterator.type) extends AnyVal {

    /** Optimized Iterator for two elements.
      */
    def double[A](_1: A, _2: A): Iterator[A] = new AbstractIterator[A] {
      private[this] var i = 0

      def hasNext: Boolean = i < 2

      def next(): A = {
        i += 1
        if (i == 1) _1
        else if (i == 2) _2
        else throw new NoSuchElementException
      }

      override val size: Int = 2
    }
  }
}
