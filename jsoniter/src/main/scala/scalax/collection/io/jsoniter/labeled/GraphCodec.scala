package scalax.collection.io.jsoniter
package labeled

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.config.GraphConfig
import scalax.collection.generic.Edge
import scalax.collection.{AnyGraph, OneOrMore, Several}

import scala.reflect.ClassTag

object GraphCodec extends IdBasedGraphCodec:

  def withNodeReferences[N <: AnyRef, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[
    X,
    Y
  ], Id <: AnyVal | String, L, ER <: WithNodeReferences[Id, L]: ClassTag](
      id: N => Id,
      toLabel: E => L,
      withNodeReferences: (E, N => Id, L) => ER,
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      config: GraphConfig,
      onJsonNull: => G[N, E],
      edgeFactory: Option[PartialFunction[(String, N, N, L), E]] = None,
      diHyperEdgeFactory: Option[PartialFunction[(String, OneOrMore[N], OneOrMore[N], L), E]] = None,
      hyperEdgeFactory: Option[PartialFunction[(String, Several[N], L), E]] = None
  )(using
      nodeCodec: JsonValueCodec[N],
      idCodec: JsonValueCodec[Id],
      labelCodec: JsonValueCodec[L],
      edgeWithNodeReferencesCodec: JsonValueCodec[ER]
  ): JsonValueCodec[G[N, E]] =
    new JsonValueCodec[G[N, E]]:
      override def decodeValue(in: JsonReader, default: G[N, E]): G[N, E] =
        new IdBasedDecoder[N, E, G, ER, Id](id, factory, default, config, in) {

          override protected def toEdge(edgeWithNodeReferences: ER): E =
            edgeWithNodeReferences match
              case AnyEdgeWithNodeReferences(edgeT, id1, id2, label) =>
                edgeFactory.getOrElse(throwMissingEdgeFactoryException).apply(edgeT, node(id1), node(id2), label)
              case HyperEdgeWithNodeReferences(edgeT, ids, label) =>
                hyperEdgeFactory
                  .getOrElse(throwMissingHyperEdgeFactoryException)
                  .apply(edgeT, Several.fromUnsafe(ids map node), label)
              case DiHyperEdgeWithNodeReferences(edgeT, sourceIds, targetIds, label) =>
                diHyperEdgeFactory
                  .getOrElse(throwMissingDiHyperEdgeFactoryException)
                  .apply(
                    edgeT,
                    OneOrMore.fromUnsafe(sourceIds map node),
                    OneOrMore.fromUnsafe(targetIds map node),
                    label
                  )
        }.apply()

      override def encodeValue(g: G[N, E], out: JsonWriter): Unit =
        encode(
          g,
          (edge, out) => edgeWithNodeReferencesCodec.encodeValue(withNodeReferences(edge, id, toLabel(edge)), out),
          out
        )

      override def nullValue: G[N, E] = onJsonNull

  end withNodeReferences
