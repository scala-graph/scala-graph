package scalax.collection
package rdf

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._

/** Custom edge type representing RDF triples as directed hyperedges.
 * 
 * This representation stores not only subjects and objects but also
 * predicates as nodes. Predicates are checked for equality.
 * RDF statements with a given predicate can be directly accessed
 * by the edges connecting to the predicate node. 
 */
class TripleDiHyperEdge[N] private (nodes: Product)
  extends DiHyperEdge[N](nodes)
  with    EdgeCopy[TripleDiHyperEdge]
  with    OuterEdge[N,TripleDiHyperEdge] 
{
  private def node(i: Int): N = iterator.drop(i).next match {
    case inner: Graph[N,_]#NodeT @unchecked => inner.value
    case outer => outer
  }
  def subject   = node(0).asInstanceOf[Subject]
  def predicate = node(1).asInstanceOf[Predicate]
  def `object`  = node(2).asInstanceOf[Object]

  override def copy[NN](newNodes: Product) = new TripleDiHyperEdge[NN](newNodes)

  override def toString = s"($subject, $predicate, ${`object`})"
}
object TripleDiHyperEdge {
  def apply(subject: Subject, predicate: Predicate, `object`: Object) =
    new TripleDiHyperEdge[RdfNode]((subject, predicate, `object`))

  def unapply(edge: TripleDiHyperEdge[RdfNode]): Option[(Subject, Predicate, Object)] =
    Some((edge.subject, edge.predicate, edge.`object`))
}