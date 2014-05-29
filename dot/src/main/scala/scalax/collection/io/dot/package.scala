package scalax.collection
package io

import language.{implicitConversions, higherKinds}
import mutable.{Graph => MGraph}

import GraphPredef.EdgeLikeIn
import GraphEdge.DiEdge

/**
 * Enables to export `Graph` instances to the DOT language by means of user-defined
 * edge and node transformers. Transformers may enrich the DOT structure with arbitrary
 * DOT attributes and also establish subgraph relationships.
 *
 * As a starting point when reading the API, please refer to
 * [[scalax.collection.io.dot.Export]]`.toDot`.
 * 
 * See also the 
 * [[http://www.scala-graph.org/guides/dot Graph for Scala DOT User Guide]].
 *
 * @author Peter Empen
 */
package object dot {
  protected[dot] type DotAST = MGraph[DotCluster, DiEdge]
  protected[dot] def  DotAST = MGraph

  /** Enables to call `<g>.toDot` with `<g>` being a `Graph` instance. */
  implicit def graph2DotExport[N, E[X]<:EdgeLikeIn[X]](graph: Graph[N,E]): Export[N,E] =
    new Export[N,E](graph)

  type NodeTransformer[N, E[X]<:EdgeLikeIn[X]] = 
    Graph[N,E]#NodeT => Option[(DotGraph, DotNodeStmt)]
    
  type EdgeTransformer[N, E[X]<:EdgeLikeIn[X]] =
    Graph[N,E]#EdgeT => Option[(DotGraph, DotEdgeStmt)]
    
  type HyperEdgeTransformer[N, E[X]<:EdgeLikeIn[X]] =
    Graph[N,E]#EdgeT => Traversable[(DotGraph, DotEdgeStmt)]
}
