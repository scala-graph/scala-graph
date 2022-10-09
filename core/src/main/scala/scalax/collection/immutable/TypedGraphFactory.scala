package scalax.collection.immutable

import scalax.collection.config.GraphConfig

import scala.language.implicitConversions
import scalax.collection.{OuterEdge, OuterNode}
import scalax.collection.generic.{Edge, TypedGraphCoreFactory}

trait TypedGraphFactory[N, E <: Edge[N]] extends TypedGraphCoreFactory[N, E, Graph] {

  def empty(implicit config: GraphConfig = defaultConfig): Graph[N, E] = Graph.empty[N, E]

  def from(edges: Iterable[E]): Graph[N, E] = DefaultGraphImpl.from[N, E](Nil, edges)(defaultConfig)

  def from(nodes: Iterable[N], edges: Iterable[E])(implicit config: GraphConfig = defaultConfig): Graph[N, E] =
    DefaultGraphImpl.from[N, E](nodes, edges)(config)

  object OuterImplicits {
    @inline implicit def toOuterNode(n: N): OuterNode[N]    = OuterNode(n)
    @inline implicit def toOuterEdge(e: E): OuterEdge[N, E] = OuterEdge(e)
  }
}
