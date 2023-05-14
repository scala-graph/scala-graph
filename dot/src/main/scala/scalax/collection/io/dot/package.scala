package scalax.collection
package io

import scala.language.implicitConversions

import edges.DiEdge
import immutable.Graph
import generic.Edge

/** Enables to export `Graph` instances to the DOT language by means of user-defined edge and node transformers.
  * Transformers may enrich the DOT structure with arbitrary DOT attributes and also establish subgraph relationships.
  *
  * As a starting point, please refer to [[scalax.collection.io.dot.Export]]`.toDot`.
  *
  * See also the [[http://www.scala-graph.org/guides/dot Graph for Scala DOT User Guide]].
  *
  * @author Peter Empen
  */
package object dot {
  protected[dot] type DotAST = mutable.Graph[DotCluster, DiEdge[DotCluster]]
  protected[dot] def DotAST = mutable.Graph

  /** Enables to call `<g>.toDot` with `<g>` being a `Graph` instance. */
  implicit class Graph2DotExport[N, E <: Edge[N]](val graph: Graph[N, E]) extends AnyVal with Export[N, E]

  type NodeTransformer[N, E <: Edge[N]]      = Graph[N, E]#NodeT => Option[(DotGraph, DotNodeStmt)]
  type EdgeTransformer[N, E <: Edge[N]]      = Graph[N, E]#EdgeT => Option[(DotGraph, DotEdgeStmt)]
  type HyperEdgeTransformer[N, E <: Edge[N]] = Graph[N, E]#EdgeT => Iterable[(DotGraph, DotEdgeStmt)]

  object implicits {
    implicit def toId(s: String): Id = Id(s)
    implicit def toId(i: Int): Id    = Id(i)
    implicit def toId(l: Long): Id   = Id(l)
    implicit def toId(f: Float): Id  = Id(f)
    implicit def toId(d: Double): Id = Id(d)

    implicit def toNodeId(s: String): NodeId = NodeId(s)
    implicit def toNodeId(i: Int): NodeId    = NodeId(i)
    implicit def toNodeId(l: Long): NodeId   = NodeId(l)
    implicit def toNodeId(f: Float): NodeId  = NodeId(f)
    implicit def toNodeId(d: Double): NodeId = NodeId(d)
    implicit def toNodeId(id: Id): NodeId    = NodeId(id.toString)
  }
}
