package scalax.collection.io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.{AnyGraph, OneOrMore, Several}
import scalax.collection.config.{CoreConfig, GraphConfig}
import scalax.collection.generic.{Edge, SingleLabel}
import scalax.collection.mutable.ArraySet

import java.io.ByteArrayInputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.NotGiven

object GraphCodec:

  def withEmbeddedNodes[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y]](
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      config: GraphConfig,
      onJsonNull: => G[N, E]
  )(using
      nodeCodec: JsonValueCodec[N],
      edgeCodec: JsonValueCodec[E]
  ): JsonValueCodec[G[N, E]] =
    new JsonValueCodec[G[N, E]]:
      override def decodeValue(in: JsonReader, default: G[N, E]): G[N, E] =
        new Decoder[N, E, G, E](factory, default, config, in) {
          override protected def toEdge(edge: E): E = edge
        }.apply()

      override def encodeValue(g: G[N, E], out: JsonWriter): Unit =
        encode(g, edgeCodec.encodeValue, out)

      override def nullValue: G[N, E] = onJsonNull
  end withEmbeddedNodes

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
                  .apply(edgeT, ids map node)
              case DiHyperEdgeWithNodeReferences(edgeT, sourceIds, targetIds) =>
                diHyperEdgeFactory
                  .getOrElse(throw new IllegalArgumentException(s"Missing diHyperEdgeFactory."))
                  .apply(edgeT, sourceIds map node, targetIds map node)
        }.apply()

      override def encodeValue(g: G[N, E], out: JsonWriter): Unit =
        encode(
          g,
          (edge, out) => edgeWithNodeReferencesCodec.encodeValue(withNodeReferences(edge, id), out),
          out
        )

      override def nullValue: G[N, E] = onJsonNull

  end withNodeReferences

  def labeledWithNodeReferences[N <: AnyRef, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[
    X,
    Y
  ], Id <: AnyVal | String, L](
      id: N => Id,
      fromLabel: L => E,
      toLabel: E => L,
      onJsonNull: => G[N, E]
  )(using
      nodeCodec: JsonValueCodec[N],
      idCodec: JsonValueCodec[Id],
      labelCodec: JsonValueCodec[L]
  ): JsonValueCodec[G[N, E]] =
    ???
  end labeledWithNodeReferences

  val Nodes = "nodes"
  val Edges = "edges"

  private def encode[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y]](
      g: G[N, E],
      encodeEdge: (E, JsonWriter) => Unit,
      out: JsonWriter
  )(using
      nodeCodec: JsonValueCodec[N]
  ): Unit =
    def writeNodes(): Unit =
      for (node <- g.nodes)
        nodeCodec.encodeValue(node.outer, out)

    def writeEdges(): Unit =
      for (edge <- g.edges)
        encodeEdge(edge.outer, out)

    def encodeGraph(writeNodes: => Unit, writeEdges: => Unit): Unit = {
      def writeArray(writeElems: => Unit): Unit =
        out.writeArrayStart()
        writeElems
        out.writeArrayEnd()

      out.writeObjectStart()
      out.writeNonEscapedAsciiKey(Nodes)
      writeArray(writeNodes)
      out.writeNonEscapedAsciiKey(Edges)
      writeArray(writeEdges)
      out.writeObjectEnd()
    }

    encodeGraph(writeNodes(), writeEdges())
  end encode

  abstract private class Decoder[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y], ER](
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      default: G[N, E],
      config: GraphConfig,
      in: JsonReader
  )(using
      nodeCodec: JsonValueCodec[N],
      edgeCodec: JsonValueCodec[ER]
  ):
    protected def readNodes(): Iterable[N] =
      val buf = new ArrayBuffer[N](config.orderHint)
      scanArray[N](buf += _)
      buf

    private def readEdges(): Iterable[E] =
      val buf = new ArrayBuffer[E](degreeHint(config))
      scanArray[ER](buf += toEdge(_))
      buf

    protected def toEdge(edgeWithNodeReferences: ER): E

    final protected def scanArray[A: JsonValueCodec](f: A => Unit): Unit =
      scanJsonArrayFromStreamReentrant[A](new ByteArrayInputStream(in.readRawValAsBytes())) { elem =>
        f(elem)
        true
      }

    def apply(): G[N, E] =
      if in.isNextToken('{') then
        var keyLen = in.readKeyAsCharBuf()
        if in.isCharBufEqualsTo(keyLen, Nodes) then
          val nodes = readNodes()

          if in.isNextToken(',') then
            keyLen = in.readKeyAsCharBuf()
            if (in.isCharBufEqualsTo(keyLen, Edges))
              val edges = readEdges()
              if !in.isNextToken('}') then in.objectEndOrCommaError()
              factory(nodes, edges, config)
            else in.unexpectedKeyError(keyLen)
          else in.commaError()
        else in.unexpectedKeyError(keyLen)
      else in.readNullOrTokenError(default, '{')

  end Decoder

  def graphConfig(orderHint: Int, degreeHint: Int): GraphConfig =
    CoreConfig(orderHint, ArraySet.Hints(degreeHint, degreeHint))

  private def degreeHint(config: GraphConfig): Int = config match {
    case CoreConfig(_, ArraySet.CheckedHints(degreeHint, _, _, _)) => degreeHint
    case c: GraphConfig                                            => c.orderHint * 50
  }
