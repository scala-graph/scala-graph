package scalax.collection

import scala.language.{higherKinds, implicitConversions}

import GraphPredef.EdgeLikeIn
import GraphEdge.EdgeCompanionBase

/** This package helps you to create random graphs with predefined metrics. It is not only possible
  *  to create random graph instances but also [[http://scalacheck.org/ Scalacheck]] generators.
  *
  * @author Peter Empen
  */
package object generator {

  /* for some reason Set(DiEdge) is not accepted by the compiler to be of type
   * Set[EdgeCompanionBase[DiEdge]] so we need this implicit conversion
   */
  implicit def toEdgeCompanionSet[E[X] <: EdgeLikeIn[X], C <: EdgeCompanionBase[E]](
      set: Set[C]): Set[EdgeCompanionBase[E]] = set.asInstanceOf[Set[EdgeCompanionBase[E]]]

  type NodeDegreeRange = parameters.NodeDegreeRange
  val NodeDegreeRange = parameters.NodeDegreeRange
}
