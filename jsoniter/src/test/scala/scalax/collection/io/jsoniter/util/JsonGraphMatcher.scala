package scalax.collection.io.jsoniter
package util

import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromString, scanJsonArrayFromStreamReentrant, JsonReader, JsonValueCodec, JsonWriter
}
import org.scalatest.matchers.{BeMatcher, MatchResult}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

/** Compares JSON strings of graphs in that node and edge array elements are treated order independently.
  * This is necessary to handle nodes and edges as sets even though they are represented as arrays in JSON.
  */
def sameAs(expectedJson: String): JsonGraphMatcher =
  new JsonGraphMatcher(expectedJson)

protected class JsonGraphMatcher(expectedJson: String) extends BeMatcher[String]:
  import JsonGraphMatcher.*

  def apply(json: String) =
    def jsonG(s: String): Try[Json] =
      given codec: JsonValueCodec[Json] = jsonStringCodec
      Try(readFromString[Json](s.minified))

    val jsonActual   = jsonG(json)
    val jsonExpected = jsonG(expectedJson)

    (jsonActual, jsonExpected) match
      case (Success(actual), Success(expected)) =>
        def leftOnly(left: Set[String], right: Set[String]) = left diff right mkString ", "
        MatchResult(
          actual == expected,
          s"""Actual $json
             |was not the same as expected
             |$expectedJson.
             |Differences:
             |  nodes not expected:         ${leftOnly(actual.nodes, expected.nodes)}
             |  nodes expected but missing: ${leftOnly(expected.nodes, actual.nodes)}
             |  edges not expected:         ${leftOnly(actual.edges, expected.edges)}
             |  edges expected but missing: ${leftOnly(expected.edges, actual.edges)}
             |""".stripMargin,
          s"Actual JSON was not different from expected JSON."
        )
      case (Failure(e), _) =>
        val msg = s"Bad actual JSON graph: ${e.getMessage}."
        MatchResult(false, msg, msg)
      case (_, Failure(e)) =>
        val msg = s"Bad expected JSON graph: ${e.getMessage}."
        MatchResult(false, msg, msg)

private object JsonGraphMatcher:
  private case class Json(nodes: Set[String], edges: Set[String])

  private val jsonStringCodec =
    new JsonValueCodec[Json]:
      private def unexpectedEncoding =
        throw new IllegalArgumentException("This codec is intended for decoding only.")

      override def decodeValue(in: JsonReader, default: Json): Json =
        import GraphCodec.{Edges, Nodes}

        val elemsAsStringCodec =
          new JsonValueCodec[String]:
            override def decodeValue(in: JsonReader, default: String): String =
              new String(in.readRawValAsBytes(), StandardCharsets.UTF_8)

            override def encodeValue(g: String, out: JsonWriter): Unit = unexpectedEncoding
            override def nullValue: String                             = null.asInstanceOf[String]

        def readElements(): Set[String] =
          given stringCodec: JsonValueCodec[String] = elemsAsStringCodec
          var set                                   = Set.empty[String]
          scanJsonArrayFromStreamReentrant[String](new ByteArrayInputStream(in.readRawValAsBytes())) { elem =>
            set = set + elem
            true
          }
          set

        if in.isNextToken('{') then
          var keyLen = in.readKeyAsCharBuf()
          if in.isCharBufEqualsTo(keyLen, Nodes) then
            val nodes = readElements()

            if in.isNextToken(',') then
              keyLen = in.readKeyAsCharBuf()
              if (in.isCharBufEqualsTo(keyLen, Edges))
                val edges = readElements()
                if !in.isNextToken('}') then in.objectEndOrCommaError()
                Json(nodes, edges)
              else in.unexpectedKeyError(keyLen)
            else in.commaError()
          else in.unexpectedKeyError(keyLen)
        else in.readNullOrTokenError(default, '{')

      override def encodeValue(g: Json, out: JsonWriter): Unit = unexpectedEncoding
      override def nullValue: Json                             = null.asInstanceOf[Json]
