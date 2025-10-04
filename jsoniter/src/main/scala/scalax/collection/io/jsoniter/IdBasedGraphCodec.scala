package scalax.collection.io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec}
import scalax.collection.AnyGraph
import scalax.collection.config.GraphConfig
import scalax.collection.generic.Edge

import scala.collection.mutable

protected[jsoniter] trait IdBasedGraphCodec extends GraphCodec:

  abstract protected class IdBasedDecoder[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[
    X,
    Y
  ], ER, Id <: AnyVal | String](
      id: N => Id,
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      default: G[N, E],
      config: GraphConfig,
      in: JsonReader
  )(using
      nodeCodec: JsonValueCodec[N],
      edgeCodec: JsonValueCodec[ER]
  ) extends Decoder[N, E, G, ER](factory, default, config, in):

    private val nodesById = new mutable.HashMap[Id, N]((config.orderHint * 1.2).toInt, 0.8)

    protected def node(id: Id): N =
      nodesById.getOrElse(id, throw new IllegalArgumentException(s"Unexpected node id $id"))

    override protected def readNodes(): Iterable[N] =
      scanArray[N](node => nodesById += id(node) -> node)
      nodesById.values

    protected def throwMissingEdgeFactoryException: Nothing =
      throw new IllegalArgumentException(s"Missing edgeFactory.")

    protected def throwMissingHyperEdgeFactoryException: Nothing =
      throw new IllegalArgumentException(s"Missing hyperEdgeFactory.")

    protected def throwMissingDiHyperEdgeFactoryException: Nothing =
      throw new IllegalArgumentException(s"Missing diHyperEdgeFactory.")
