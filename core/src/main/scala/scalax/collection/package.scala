package scalax
/**
 * Contains the base traits and objects needed to use '''Graph for Scala'''.
 *
 * The '''Graph4Scala-UserGuide''' is available at 
 * [[http://www.assembla.com/spaces/scala-graph/documents]].
 * @author Peter Empen
 */
package object collection {
  /**
   * The default filter function for degrees to be included in
   * degree calculation always returning true.
   */
  val AnyDegree = (degree: Int) => true
  /**
   * [[scala.collection.Set]] extended by some useful methods in the context of Graph.
   */
  type ExtSet[A] = scala.collection.Set[A] with interfaces.ExtSetMethods[A]
  /**
   * Same as `private[scala] scala.collection.AbstractIterator`.
   */
  private[scalax] abstract class AbstractIterator[+A] extends Iterator[A]
}
