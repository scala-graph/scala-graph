package scalax.collection.io.jsoniter
package nonlabeled

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.config.GraphConfig
import scalax.collection.generic.{Edge, SingleLabel}
import scalax.collection.{AnyGraph, OneOrMore, Several}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.NotGiven

object GraphCodec extends GraphCodec:

  def withNodeReferences[N <: AnyRef, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[
    X,
    Y
  ], Id <: AnyVal | String, ER <: WithNodeReferences[Id]: ClassTag](
      id: N => Id,
      withNodeReferences: (E, N => Id) => ER,
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      config: GraphConfig,
      onJsonNull: => G[N, E],
      edgeFactory: Option[PartialFunction[(String, N, N), E]] = None,
      diHyperEdgeFactory: Option[PartialFunction[(String, OneOrMore[N], OneOrMore[N]), E]] = None,
      hyperEdgeFactory: Option[PartialFunction[(String, Several[N]), E]] = None
  )(using
      nodeCodec: JsonValueCodec[N],
      idCodec: JsonValueCodec[Id],
      edgeWithNodeReferencesCodec: JsonValueCodec[ER],
      exclude: NotGiven[E <:< SingleLabel[_]]
  ): JsonValueCodec[G[N, E]] =
    new JsonValueCodec[G[N, E]]:
      override def decodeValue(in: JsonReader, default: G[N, E]): G[N, E] =
        new Decoder[N, E, G, ER](factory, default, config, in) {
          private val nodesById = new mutable.HashMap[Id, N]((config.orderHint * 1.2).toInt, 0.8)

          private def node(id: Id) =
            nodesById.getOrElse(id, throw new IllegalArgumentException(s"Unexpected node id $id"))

          override protected def readNodes(): Iterable[N] =
            scanArray[N](node => nodesById += id(node) -> node)
            nodesById.values

          override protected def toEdge(edgeWithNodeReferences: ER): E =
            edgeWithNodeReferences match
              case DiEdgeWithNodeReferences(edgeT, sourceId, targetId) =>
                edgeFactory
                  .getOrElse(throw new IllegalArgumentException(s"Missing edgeFactory."))
                  .apply(edgeT, node(sourceId), node(targetId))
              case UnDiEdgeWithNodeReferences(edgeT, id1, id2) =>
                edgeFactory
                  .getOrElse(throw new IllegalArgumentException(s"Missing edgeFactory."))
                  .apply(edgeT, node(id1), node(id2))
              case HyperEdgeWithNodeReferences(edgeT, ids) =>
                hyperEdgeFactory
                  .getOrElse(throw new IllegalArgumentException(s"Missing hyperEdgeFactory."))
                  .apply(edgeT, Several.fromUnsafe(ids map node))
              case DiHyperEdgeWithNodeReferences(edgeT, sourceIds, targetIds) =>
                diHyperEdgeFactory
                  .getOrElse(throw new IllegalArgumentException(s"Missing diHyperEdgeFactory."))
                  .apply(edgeT, OneOrMore.fromUnsafe(sourceIds map node), OneOrMore.fromUnsafe(targetIds map node))
        }.apply()

      override def encodeValue(g: G[N, E], out: JsonWriter): Unit =
        encode(
          g,
          (edge, out) => edgeWithNodeReferencesCodec.encodeValue(withNodeReferences(edge, id), out),
          out
        )

      override def nullValue: G[N, E] = onJsonNull

  end withNodeReferences
