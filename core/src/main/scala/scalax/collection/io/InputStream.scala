package scalax.collection
package io

import language.higherKinds

import GraphEdge._
import edge.{CEdge, CHyperEdge}
import edge. WBase.{    WEdge,  WHyperEdgeCompanion,  WHyperEdgeBound,  WEdgeCompanion,  WEdgeBound}
import edge. LBase.{    LEdge,  LHyperEdgeCompanion,  LHyperEdgeBound,  LEdgeCompanion,  LEdgeBound}
import edge.WLBase.{   WLEdge, WLHyperEdgeCompanion, WLHyperEdgeBound, WLEdgeCompanion, WLEdgeBound}
import edge. CBase.{Attributes, CHyperEdgeCompanion,  CHyperEdgeBound,  CEdgeCompanion,  CEdgeBound}

/**
 * Root trait for edge streams. Edge stream methods return the components of an edge
 * from which an inner (or outer) edge in the sense of `Graph` may be composed
 * with the help of a given edge factory. An edge is made up of the components nodes
 * and optionally a weight and a label.
 * Classes derived from this trait may be used as an io-interface to load/unload
 * `Graph` data.   
 * 
 * @tparam N type of nodes.
 * @tparam E type of edges.
 * @tparam L type of edge labels or Nothing.
 * @author Peter Empen
 */
sealed trait EdgeAdapterBase[N, E[N] <: EdgeLike[N], L]
/** Adapter submitting the weight component of an edge. */
sealed trait WeightedAdapter   {
  /** This method will be called by the consumer such as a `Graph` factory and
   * must return the weight of the edge the edge stream cursor has been moved to. */
  def weight: Long
}
/** Adapter submitting the label component of an edge. */
sealed trait LabeledAdapter[L] {
  /** This method will be called by the consumer such as a `Graph` factory and
   * must return the label of the edge the edge stream cursor has been moved to. */
  def label: L
}
/** Adapter submitting the custom attributes of an edge. */
sealed trait AttributedAdapter {
  /** This method will be called by the consumer such as a `Graph` factory and
   * must return the label of the edge the edge stream cursor has been moved to. */
  def attributes: Product
}
/** Adapter submitting the components of a non-weighted and non-labeled hyperedge. */
trait HyperEdgeAdapter  [N, E[N] <: HyperEdge[N], L]
  extends EdgeAdapterBase[N,E,L] {
  /** This method will be called by the consumer such as a `Graph` factory and
   * must return the nodes of the hyperedge the edge stream cursor has been moved to. */
  def nodes: Seq[N]
}
/** Adapter submitting the components of a weighted hyperedge. */
trait WHyperEdgeAdapter [N, E[N] <: HyperEdge[N] with WEdge[N], L]
  extends HyperEdgeAdapter[N,E,L] with WeightedAdapter
/** Adapter submitting the components of a labeled hyperedge. */
trait LHyperEdgeAdapter[N, E[N] <: HyperEdge[N] with LEdge[N], L]
  extends HyperEdgeAdapter[N,E,L] with LabeledAdapter[L]
/** Adapter submitting the components of a weighted and labeled hyperedge. */
trait WLHyperEdgeAdapter[N, E[N] <: HyperEdge[N] with WLEdge[N], L]
  extends WHyperEdgeAdapter[N,E,L] with LabeledAdapter[L]
/** Adapter submitting the components of a custom hyperedge. */
trait CHyperEdgeAdapter[N, E[N] <: CHyperEdge[N], L]
  extends HyperEdgeAdapter[N,E,L] with AttributedAdapter

/** Adapter submitting the components of a non-weighted and non-labeled edge. */
trait EdgeAdapter [N, E[N] <: UnDiEdge[N], L]
  extends EdgeAdapterBase[N,E,L] {
  /** This method will be called by the consumer such as a `Graph` factory and
   * must return the nodes of the edge the edge stream cursor has been moved to. */
  def nodes: Tuple2[N,N]
}
/** Adapter submitting the components of a weighted edge. */
trait WEdgeAdapter [N, E[N] <: UnDiEdge[N] with WEdge[N], L]
  extends  EdgeAdapter[N,E,L] with WeightedAdapter
/** Adapter submitting the components of a labeled edge. */
trait LEdgeAdapter[N, E[N] <: UnDiEdge[N] with LEdge[N], L]
  extends  EdgeAdapter[N,E,L] with LabeledAdapter[L]
/** Adapter submitting the components of a weighted and labeled edge. */
trait WLEdgeAdapter[N, E[N] <: UnDiEdge[N] with WLEdge[N], L]
  extends WEdgeAdapter[N,E,L] with LabeledAdapter[L]
/** Adapter submitting the components of a custom edge. */
trait CEdgeAdapter[N, E[N] <: CEdge[N], L]
  extends EdgeAdapter[N,E,L] with AttributedAdapter

/**
 * Generic type for edge input streams to be used for type definitions of
 * collections containing `EdgeInputStream` instances of unknown types for
 * `L`, `C` and `A`. Example: `Iterable[GenEdgeInputStream[E,N]]`.
 *
 * @tparam N type of nodes.
 * @tparam E kind of the edges.
 */
sealed abstract class GenEdgeInputStream[N,E[N] <: EdgeLike[N]]
                                        (val factory: EdgeCompanionBase[E])
/**
 * This abstract class is to be used to construct a concrete edge input stream
 * that may be passed to the `from` factory method of a `Graph` companion object.
 * 
 * @tparam N type of nodes to be passed to a `Graph` factory method from this stream.
 * @tparam E kind of the edges to be passed to a `Graph` factory method from this stream.
 * @tparam L type of edge labels or `Nothing`.
 * @tparam C kind of the edge factory to be invoked when creating edges from the
 *           raw edge data read from this stream.
 * @tparam A kind of edge adapter that will be the return type of `next`.
 */
abstract class EdgeInputStream[N,
                               E[N] <: EdgeLike[N],
                               L,
                              +C[  EE[N]<:E[N]] <: EdgeCompanionBase[EE],
                              +A[N,EE[N]<:E[N],AL<:L] <: EdgeAdapterBase[N,EE,AL]]
              (override val factory: C[E])
  extends GenEdgeInputStream[N,E](factory)
  with    EdgeAdapterBase[N,E,L]
  with    Iterator[A[N,E,L]]
{
  /**
   * The consumer of the stream calls this method to get informed whether there are
   * any more edges left in the stream. After having served this inquiry, the producer
   * will be asked by a call to `next` to move to the next edge available. 
   * 
   * @return `true` if there is at least one more edge, `false` if the stream has reached
   *         its end.
   */
  def hasNext: Boolean
  /**
   * The consumer of the stream calls this method to tell the provider to move to
   * the next edge. Moving the edge stream cursor to the next edge must assert that
   * all subsequent calls to methods of the returned adapter will return data belonging
   * to that next input edge.
   * 
   * Note that this semantic is different from that normally experienced in that
   * `next` does not return the expected result (nodes, weight and label) but an adapter
   * to be queried first to get the expected results.     
   *
   * @return the adapter that the edge stream cursor has been moved to.
   */
  def next: A[N,E,L]
}
/**
 * This abstract class is to be used to construct a concrete node input stream
 * that may be passed to the `from` factory method of a `Graph` companion object.
 * 
 * @tparam N type of nodes to be passed to a `Graph` factory method from this stream
 *           that will be the return type of `next`.
 * @author Peter Empen
 */
abstract class NodeInputStream[N]
  extends Iterator[N]
{
  /**
   * The consumer of the stream calls this method to get informed whether there are
   * any more nodes left in the stream. After having served this inquiry, the producer
   * will be asked by a call to `next` to make to the next edge available. 
   *  
   * @return `true` if there is at least one more node, `false` if the stream has reached
   *         its end.
   */
  def hasNext: Boolean
  /**
   * The consumer of the stream calls this method to tell the provider to move to
   * the next node.
   * 
   * @return the node that the node stream cursor has been moved to.
   */
  def next: N
}
