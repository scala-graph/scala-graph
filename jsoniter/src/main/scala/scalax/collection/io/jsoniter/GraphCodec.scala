package scalax.collection.io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.AnyGraph
import scalax.collection.config.{CoreConfig, GraphConfig}
import scalax.collection.generic.Edge
import scalax.collection.mutable.ArraySet

import java.io.ByteArrayInputStream
import scala.collection.mutable.ArrayBuffer

object GraphCodec:

  def withEmbeddedNodes[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y]](
      onJsonNull: => G[N, E],
      factory: (Iterable[N], Iterable[E], GraphConfig) => G[N, E],
      config: GraphConfig
  )(implicit
      nodeCodec: JsonValueCodec[N],
      edgeCodec: JsonValueCodec[E]
  ): JsonValueCodec[G[N, E]] =
    new JsonValueCodec[G[N, E]]:
      private val Nodes = "nodes"
      private val Edges = "edges"

      override def decodeValue(in: JsonReader, default: G[N, E]): G[N, E] = {
        def readNodes(): Iterable[N] =
          val buf = new ArrayBuffer[N](config.orderHint)
          scanJsonArrayFromStreamReentrant[N](new ByteArrayInputStream(in.readRawValAsBytes())) { node =>
            buf += node
            true
          }
          buf

        def readEdges(): Iterable[E] =
          val buf = new ArrayBuffer[E](degreeHint(config))
          scanJsonArrayFromStreamReentrant[E](new ByteArrayInputStream(in.readRawValAsBytes())) { edge =>
            buf += edge
            true
          }
          buf

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
      }

      override def encodeValue(g: G[N, E], out: JsonWriter): Unit = {
        def writeNodes(out: JsonWriter): Unit =
          for (node <- g.nodes)
            nodeCodec.encodeValue(node.outer, out)

        def writeEdges(out: JsonWriter): Unit =
          for (edge <- g.edges)
            edgeCodec.encodeValue(edge.outer, out)

        def encodeGraph(writeNodes: JsonWriter => Unit, writeEdges: JsonWriter => Unit): Unit = {
          def writeArray(writeElems: JsonWriter => Unit): Unit =
            out.writeArrayStart()
            writeElems(out)
            out.writeArrayEnd()

          out.writeObjectStart()
          out.writeNonEscapedAsciiKey(Nodes)
          writeArray(writeNodes)
          out.writeNonEscapedAsciiKey(Edges)
          writeArray(writeEdges)
          out.writeObjectEnd()
        }

        encodeGraph(writeNodes, writeEdges)
      }

      override def nullValue: G[N, E] = onJsonNull
  end withEmbeddedNodes

  def withNodeReferences[N <: AnyRef, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y], Id](
      id: N => Id,
      onJsonNull: => G[N, E]
  )(implicit
      nCodec: JsonValueCodec[N],
      idCodec: JsonValueCodec[Id]
  ): JsonValueCodec[G[N, E]] =
    ???
  end withNodeReferences

  def graphConfig(orderHint: Int, degreeHint: Int): GraphConfig =
    CoreConfig(orderHint, ArraySet.Hints(degreeHint, degreeHint))

  private def degreeHint(config: GraphConfig): Int = config match {
    case CoreConfig(_, ArraySet.CheckedHints(degreeHint, _, _, _)) => degreeHint
    case c: GraphConfig                                            => c.orderHint * 50
  }
