package scalax.collection
package io.json
package serializer

import scala.reflect.classTag

import net.liftweb.json._

import scalax.collection.config.CoreConfig
import scalax.collection.generic.Edge

/** This custom serializer is to be registered whenever a class to be (de)serialized contains any `Graph` instances.
  *  For usage see `scalax.collection.io.json.serializer.TGraphSerializer`.
  */
final class GraphSerializer[N, E <: Edge[N]](descriptor: Descriptor[N])(implicit edgeManifest: Manifest[E])
    extends Serializer[AnyGraph[N, E]] {

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) if clazz.isAssignableFrom(classOf[AnyGraph[N, E]]) =>
      json match {
        case JObject(_) =>
          mutable.Graph.fromJson[N, E](json, descriptor)(CoreConfig())
        case x => throw new MappingException("Can't convert " + x + " to " + clazz.getName)
      }
  }

  override def serialize(implicit format: Formats) = { case graph: AnyGraph[N, E] =>
    val `export` = new exp.Export(graph, descriptor)
    import `export`._
    jsonAST(List(jsonASTNodes, jsonASTEdges))
  }
}
