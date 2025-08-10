package scalax.collection.io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.AnyGraph
import scalax.collection.config.{CoreConfig, GraphConfig}
import scalax.collection.generic.Edge
import scalax.collection.mutable.ArraySet

import java.io.ByteArrayInputStream
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object GraphCodec extends GraphCodec:

  /** Produces a Graph codec with a JSON where edge ends are represented by the full JSON of `N`.
    *
    * This codec is the right choice whenever `N` is a primitive-like type, including all primitive types, `String`,
    * or any type with a fairly small JSON representation.
    * Further, you might opt for this codec, even if `N` should be more complex, provided you are NOT concerned about
    * the total length of the JSON.
    * For instance, the resulting verbose edge ends might add value in an educational environment.
    *
    * @param factory to create the Graph from the decoded JSON based on `Iterable`s of nodes and edges.
    *                Typically, you can pass `Graph.from(_, _)(_)` where `Graph` is either mutable or immutable.
    * @param config to be passed to the Graph after JSON decoding. Use `GraphCodec.graphConfig` to produce your
    *               best-guess configuration.
    * @param onJsonNull is used in case the complete JSON is `null`. Either supply a default, usually empty, Graph or
    *                   `null.asInstanceOf[...]` to let the `null` case fail. See also
    *                   [[https://github.com/plokhotnyuk/jsoniter-scala/blob/ebf18b1e7b369107aa52ff1f00338452829f8a91/jsoniter-scala-core/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/JsonCodec.scala#L51 JsonValueCodec.nullValue]]
    * @param nodeCodec codec for `N`.
    * @param edgeCodec codec for `E`. If `E` is a concrete class or an ADT of edges, use `JsonCodecMaker.make` directly.
    *                  For mixed Graphs with an `E` being `AnyEdge[N]` or alike, where your concrete edge classes do not
    *                  build an ADT, refer to `EdgeCodec.makePolymorphicWithEmbeddedNodes`.
    */
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

protected[jsoniter] trait GraphCodec:
  /** The key of the array of nodes in the JSON. */
  val Nodes = "nodes"

  /** The key of the array of edges in the JSON. */
  val Edges = "edges"

  protected def encode[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y]](
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

  abstract protected class Decoder[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y], ER](
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

  /** Produces an optimal Graph configuration for the purpose of reducing internal memory allocations.
    *
    * @param orderHint number of nodes in the Graph with a high percentile.
    * @param degreeHint edge degree with a high percentile.
    */
  def graphConfig(orderHint: Int, degreeHint: Int): GraphConfig =
    CoreConfig(orderHint, ArraySet.Hints(degreeHint, degreeHint))

  protected def degreeHint(config: GraphConfig): Int = config match
    case CoreConfig(_, ArraySet.CheckedHints(degreeHint, _, _, _)) => degreeHint
    case c: GraphConfig                                            => c.orderHint * 50
