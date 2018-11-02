package scalax.collection
package rdf

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._

/** Custom edge type representing RDF triples as directed edges.
 * 
 * This representation does not relate predicates to subjects or objects
 * of other triples. Predicates are not checked for equality.
 * RDF statements with a given predicate can only be computed by filtering
 * the complete edge set. 
 */
class TripleDiEdge[N] private (nodes: Product, val predicate: Predicate)
  extends DiEdge[N](nodes)
  with    ExtendedKey[N]
  with    EdgeCopy[TripleDiEdge]
  with    OuterEdge[N,TripleDiEdge] 
{
  private def node(i: Int): N = iterator.drop(i).next match {
    case inner: Graph[N,_]#NodeT @unchecked => inner.value
    case outer => outer
  }
  def subject  = node(0).asInstanceOf[Subject]
  def `object` = node(1).asInstanceOf[Object]

  def keyAttributes = predicate :: Nil

  override def copy[NN](newNodes: Product) =
    new TripleDiEdge[NN](newNodes, predicate)

  override def toString = s"($subject, $predicate, ${`object`})"
}
object TripleDiEdge {
  def apply(subject: Subject, predicate: Predicate, `object`: Object) =
    new TripleDiEdge[RdfNode]((subject, `object`), predicate)
  def unapply(edge: TripleDiEdge[RdfNode]) =
    Some((edge.subject, edge.predicate, edge.`object`))
}