package scalax.collection.io.jsoniter
package nonlabeled

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.config.GraphConfig
import scalax.collection.generic.{Edge, SingleLabel}
import scalax.collection.{AnyGraph, OneOrMore, Several}

import scala.reflect.ClassTag
import scala.util.NotGiven

object GraphCodec extends IdBasedGraphCodec:

  /** Produces a codec for a Graph with non-labeled edges where JSON edge ends are represented by node IDs.
    *
    * Replacing edge ends by node IDs facilitates a more compact JSON representation.
    * This is because nodes are encoded into an array of nodes so edges,
    * that get encoded into a separate array, do not need to also embed complete nodes.
    * Therefore, prefer this Graph codec over embedded nodes unless nodes are of some primitive-like type.
    *
    * For edges, this codec uses the predefined ADT [[WithNodeReferences]] which is an intermediate JSON
    * representation of edges with edge ends of type `N` replaced by node IDs of type `Id`.
    *
    * @param id a one-to-one function producing an ID for a given node.
    * @param withNodeReferences factory for the JSON representation of edges.
    *                           Just supply the appropriate predefined factory like `DiEdgeWithNodeReferences.apply`.
    *                           The predefined factories also support mixed Graphs.
    * @param factory to create the Graph from the decoded JSON based on `Iterable`s of nodes and edges.
    *                Typically, you can pass `Graph.from(_, _)(_)` where `Graph` is either mutable or immutable.
    * @param config to be passed to the Graph after JSON decoding. Use `GraphCodec.graphConfig` to produce your
    *               best-guess configuration.
    * @param onJsonNull is used in case the complete JSON is `null`. Either supply a default, usually empty, Graph or
    *                   `null.asInstanceOf[...]` to let the `null` case fail. See also
    *                   [[https://github.com/plokhotnyuk/jsoniter-scala/blob/ebf18b1e7b369107aa52ff1f00338452829f8a91/jsoniter-scala-core/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/JsonCodec.scala#L51 JsonValueCodec.nullValue]]
    * @param edgeFactory to produce edges from JSON. Provide separate `case`s for all used edge types.
    *                    The first, `String` member of the tuple corresponds to the `edgeT` parameter of the intermediate
    *                    `*WithNodeReferences` class.
    * @param diHyperEdgeFactory to produce directed hyperedges from JSON. Provide separate `case`s for all used directed hyperedge types.
    * @param hyperEdgeFactory to produce hyperedges from JSON. Provide separate `case`s for all used hyperedge types.
    * @param nodeCodec codec for `N`.
    * @param idCodec codec for `Id`.
    * @param edgeWithNodeReferencesCodec codec for the intermediate JSON edge type `ER`. Use `JsonCodecMaker.make`
    *                                    to create this codec. In case you are using one of the ADTs `Any*WithNodeReferences`,
    *                                    the JSON will also include the concrete type like `"type": "DiEdgeWithNodeReferences"`.
    *                                    You can replace these verbose class names by shortcuts by invoking the macro
    *                                    like `JsonCodecMaker.make(compactClassNames)`.
    * @param exclude causes a compile error if edges have the superclass `SingleLabel`. Note that this is just a partial
    *                check since there exists no ultimate library abstraction for labeled classes.
    * @tparam N the type of nodes.
    * @tparam E the type of edges.
    * @tparam G the type of Graph.
    * @tparam Id the type of node IDs constrained to primitive-like types.
    * @tparam ER the type of the JSON representation of edges with edge ends of type `Id`.
    */
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
        new IdBasedDecoder[N, E, G, ER, Id](id, factory, default, config, in) {

          override protected def toEdge(edgeWithNodeReferences: ER): E =
            edgeWithNodeReferences match
              case AnyEdgeWithNodeReferences(edgeT, id1, id2) =>
                edgeFactory.getOrElse(throwMissingEdgeFactoryException).apply(edgeT, node(id1), node(id2))
              case HyperEdgeWithNodeReferences(edgeT, ids) =>
                hyperEdgeFactory
                  .getOrElse(throwMissingHyperEdgeFactoryException)
                  .apply(edgeT, Several.fromUnsafe(ids map node))
              case DiHyperEdgeWithNodeReferences(edgeT, sourceIds, targetIds) =>
                diHyperEdgeFactory
                  .getOrElse(throwMissingDiHyperEdgeFactoryException)
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
