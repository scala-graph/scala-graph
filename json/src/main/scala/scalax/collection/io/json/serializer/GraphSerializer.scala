package scalax.collection.io.json
package serializer

import net.liftweb.json._
import scalax.collection.GraphPredef._
import scalax.collection.Graph

/** This custom serializer is to be registered whenever a class to be (de)serialized   
 *  contains on or more `Graph` instances.
 *  For usage see `scalax.collection.io.json.serializer.TGraphSerializer`.
 */
final class GraphSerializer[N, E[X] <: EdgeLikeIn[X]]
    (descriptor: Descriptor[N])
    (implicit edgeManifest: Manifest[E[N]])
    extends Serializer[Graph[N, E]] {

  override def deserialize(implicit format: Formats) = {
    case (TypeInfo(clazz, _), json) if clazz == classOf[Graph[N,E]] => json match {
      case JObject(text) =>
        import scalax.collection.io.json.imp.Parser
        Graph.fromJson[N,E](compact(render(json)), descriptor)
      case x => throw new MappingException(
        "Can't convert " + x + " to " + clazz.getSimpleName)
    }
  }
  override def serialize(implicit format: Formats) = {
    case graph: Graph[N, E] =>
      val export = new exp.Export(graph, descriptor)
      import export._
      jsonAST(jsonASTNodes ++ jsonASTEdges)
  }
}
