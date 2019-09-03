package scalax

import scala.collection.ExtSetMethods

/** Contains the base traits and objects needed to use '''Graph for Scala'''.
  *
  * See also the
  * [[http://www.scala-graph.org/guides/core-introduction Graph for Scala Core User Guide]].
  *
  * @author Peter Empen
  */
package object collection {

  /** The default filter function for degrees to be included in
    * degree calculation always returning true.
    */
  val AnyDegree = (degree: Int) => true

  /** [[scala.collection.Set]] extended by some useful methods in the context of Graph.
    */
  type ExtSet[A] = scala.collection.Set[A] with ExtSetMethods[A]

  protected[scalax] type AnySet[A] = scala.collection.Set[A]

  protected[scalax] type MSet[A] = scala.collection.mutable.Set[A]
  @inline final protected[scalax] def MSet = scala.collection.mutable.Set

  /** Adds chaining methods `tap` and `pipe` to `Any`. Back ported from Scala 2.13.
    */
  implicit final class ChainingOps[A](val self: A) extends AnyVal {
    def tap[U](f: A => U): A  = { f(self); self }
    def pipe[B](f: A => B): B = f(self)
  }
}
