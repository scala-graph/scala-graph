package scalax.collection.io.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import scalax.collection.AnyGraph

extension [G <: AnyGraph[_, _]](graph: G)(using codec: JsonValueCodec[G])
  /** syntactic sugar for `jsoniter_scala.core.writeToString(graph)` */
  def toJson: String = writeToString(graph)

extension (json: String)
  /** syntactic sugar for `jsoniter_scala.core.readFromString[G](json)` */
  def toGraph[G <: AnyGraph[_, _]](using codec: JsonValueCodec[G]): G =
    readFromString[G](json)
