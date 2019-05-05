package scalax.collection.io

import net.liftweb.json.JValue
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.edge.LBase._
import scalax.collection.edge.WBase._
import scalax.collection.edge.WLBase._
import scalax.collection.edge._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.io.json.imp.JsonList
import scalax.collection.io.json.imp.Parser.parse
import scalax.collection.io.json.imp.Stream.createOuterElems
import scalax.collection.{Graph, GraphLike}

import scala.language.higherKinds
import scala.reflect.ClassTag

/** Facilitates populating graphs with nodes/edges from JSON text
  * and exporting `Graph`instances to JSON text.
  *
  * See also the
  * [[http://www.scala-graph.org/guides/json Graph for Scala JSON User Guide]].
  *
  * @define DESCR top level JSON import/export descriptor to be filled with all `NodeDescriptor`s and `EdgeDescriptors`.
  * @author Peter Empen
  */
package object json {
  type Descriptor[N]                                                            = descriptor.Descriptor[N]
  type NodeDescriptor[N]                                                        = descriptor.NodeDescriptor[N]
  type EdgeDescriptorBase[N, E[X] <: EdgeLikeIn[X], +C <: EdgeCompanionBase[E]] = descriptor.EdgeDescriptorBase[N, E, C]
  type EdgeDescriptor[N, E[X] <: UnDiEdge[X], +C <: EdgeCompanion[E]]           = descriptor.EdgeDescriptor[N, E, C]
  type WEdgeDescriptor[N, E[X] <: UnDiEdge[X] with WEdge[X], +C <: WEdgeCompanion[E]] =
    descriptor.WEdgeDescriptor[N, E, C]
  type LEdgeDescriptor[N, E[X] <: UnDiEdge[X] with LEdge[X], +C <: LEdgeCompanion[E], L <: AnyRef] =
    descriptor.LEdgeDescriptor[N, E, C, L]
  type WLEdgeDescriptor[N, E[X] <: UnDiEdge[X] with WLEdge[X], +C <: WLEdgeCompanion[E], L <: AnyRef] =
    descriptor.WLEdgeDescriptor[N, E, C, L]
  type HyperEdgeDescriptor[N, E[X] <: HyperEdge[X], +C <: HyperEdgeCompanion[E]] =
    descriptor.HyperEdgeDescriptor[N, E, C]
  type WHyperEdgeDescriptor[N, E[X] <: WHyperEdge[X] with WEdge[X], +C <: WHyperEdgeCompanion[E]] =
    descriptor.WHyperEdgeDescriptor[N, E, C]
  type LHyperEdgeDescriptor[N, E[X] <: LHyperEdge[X] with LEdge[X], +C <: LHyperEdgeCompanion[E], L <: AnyRef] =
    descriptor.LHyperEdgeDescriptor[N, E, C, L]
  type WLHyperEdgeDescriptor[N, E[X] <: WLHyperEdge[X] with WLEdge[X], +C <: WLHyperEdgeCompanion[E], L <: AnyRef] =
    descriptor.WLHyperEdgeDescriptor[N, E, C, L]

  implicit final class JsonGraphCoreCompanion[+G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]](
      val companion: GraphCoreCompanion[G])
      extends AnyVal {

    /** Creates a new Graph instance and populates it with all nodes/edges found in
      * the node/edge sections of a JSON text.
      *
      * @param jsonAST the JSON tree to be parsed for node/edge sections
      * @param descriptor $DESCR
      * @return new `Graph` instance populated from `jsonAST`
      */
    def fromJson[N, E[X] <: EdgeLikeIn[X]](jsonAST: JValue, descriptor: Descriptor[N])(
        implicit edgeT: ClassTag[E[N]],
        config: companion.Config): G[N, E] =
      fromJson[N, E](parse(jsonAST, descriptor), descriptor)

    /** Creates a new Graph instance and populates it with all nodes/edges found in
      * the node/edge sections of a JSON text.
      *
      * @param jsonText the JSON text to be parsed for node/edge sections
      * @param descriptor $DESCR
      * @return new `Graph` instance populated from `jsonText`
      */
    def fromJson[N, E[X] <: EdgeLikeIn[X]](jsonText: String, descriptor: Descriptor[N])(
        implicit edgeT: ClassTag[E[N]],
        config: companion.Config = companion.defaultConfig): G[N, E] =
      fromJson[N, E](parse(jsonText, descriptor), descriptor)

    /** Creates a new Graph instance and populates it with all nodes/edges found in
      * `jsonLists`.
      *
      * @param jsonLists node/edge lists usually attained by parsing a JSON text
      * @param descriptor $DESCR
      * @return new `Graph` instance populated from `jsonText`
      */
    def fromJson[N, E[X] <: EdgeLikeIn[X]](jsonLists: Iterable[JsonList], descriptor: Descriptor[N])(
        implicit edgeT: ClassTag[E[N]],
        config: companion.Config): G[N, E] = {
      val target = createOuterElems[N, E](jsonLists, descriptor)
      companion.from[N, E](nodes = target._1, edges = target._2)(edgeT, config)
    }
  }

  implicit final class JsonGraph[N, E[X] <: EdgeLikeIn[X]](val graph: Graph[N, E]) extends AnyVal {

    /** Creates a JSON text including all nodes/edges in this graph.
      *
      * @param descriptor $DESCR
      * @return the JSON text
      */
    def toJson(descriptor: Descriptor[N]): String = {
      val export = new exp.Export[N, E](graph, descriptor)
      import export._
      jsonText(jsonAST(List(jsonASTNodes, jsonASTEdges)))
    }
  }

  /** Replaces all occurrences of `paramPlaceholder` in source with the elements
    * in `params` one by one. The result is guaranteed not to become longer than
    * `maxLength`.
    */
  def replacePlaceholders(source: String,
                          params: Iterable[String],
                          maxLength: Int = 50,
                          paramPlaceholder: String = "{}"): String = {
    var target = source
    val it     = params.iterator
    var i      = 0
    while ({ i = target.indexOfSlice(paramPlaceholder); i >= 0 } &&
    it.hasNext) {
      val param = it.next
      target = target patch (i, if (param.length < maxLength) param
      else param.substring(0, maxLength - 3) + "...", 2)
    }
    target
  }
}
