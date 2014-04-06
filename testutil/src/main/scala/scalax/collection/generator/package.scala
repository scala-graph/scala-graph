package scalax.collection

/** This package helps you to create random graphs with predefined metrics. It is not only possible
 *  to create random graph instances but also [[http://scalacheck.org/ Scalacheck]] generators.
 * 
 * @author Peter Empen
 */
package object generator {

  type NodeDegreeRange = parameters.NodeDegreeRange
  val  NodeDegreeRange = parameters.NodeDegreeRange
}