package scalax.collection.io.jsoniter
package labeled

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import scalax.collection.generic.{AnyDiEdge, AnyDiHyperEdge, AnyHyperEdge, AnyUnDiEdge}
import scalax.collection.OneOrMore

/** ADT for JSON codecs of labeled edges where edge ends are represented by node IDs.
  * `L` may be different from the logical label of the edge class to be mapped.
  */
sealed trait WithNodeReferences[Id <: AnyVal | String, L]:
  def edgeT: String
  def label: L

sealed trait AnyHyperEdgeWithNodeReferences[Id <: AnyVal | String, L]   extends WithNodeReferences[Id, L]
sealed trait AnyDiHyperEdgeWithNodeReferences[Id <: AnyVal | String, L] extends AnyHyperEdgeWithNodeReferences[Id, L]
sealed trait AnyEdgeWithNodeReferences[Id <: AnyVal | String, L]        extends AnyHyperEdgeWithNodeReferences[Id, L]:
  def id1: Id
  def id2: Id

object AnyHyperEdgeWithNodeReferences:
  inline def compactClassNames: CodecMakerConfig =
    nonlabeled.AnyHyperEdgeWithNodeReferences.compactClassNames

object AnyDiHyperEdgeWithNodeReferences:
  inline def compactClassNames: CodecMakerConfig =
    nonlabeled.AnyDiHyperEdgeWithNodeReferences.compactClassNames

object AnyEdgeWithNodeReferences:
  def unapply[Id <: AnyVal | String, L](e: AnyEdgeWithNodeReferences[Id, L]): (String, Id, Id, L) =
    (e.edgeT, e.id1, e.id2, e.label)

  inline def compactClassNames: CodecMakerConfig =
    nonlabeled.AnyEdgeWithNodeReferences.compactClassNames

case class DiEdgeWithNodeReferences[Id <: AnyVal | String, L](edgeT: String, sourceId: Id, targetId: Id, label: L)
    extends AnyEdgeWithNodeReferences[Id, L]
    with AnyDiHyperEdgeWithNodeReferences[Id, L]:
  def id1: Id = sourceId
  def id2: Id = targetId

object DiEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String, L](edge: AnyDiEdge[N], id: N => Id, label: L): DiEdgeWithNodeReferences[Id, L] =
    apply(edge, id, edge.getClass.getSimpleName, label)

  def apply[N, Id <: AnyVal | String, L](
      edge: AnyDiEdge[N],
      id: N => Id,
      edgeT: String,
      label: L
  ): DiEdgeWithNodeReferences[Id, L] =
    new DiEdgeWithNodeReferences[Id, L](
      edgeT,
      id(edge.source),
      id(edge.target),
      label
    )

case class UnDiEdgeWithNodeReferences[Id <: AnyVal | String, L](edgeT: String, id1: Id, id2: Id, label: L)
    extends AnyEdgeWithNodeReferences[Id, L]

object UnDiEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String, L](
      edge: AnyUnDiEdge[N],
      id: N => Id,
      label: L
  ): UnDiEdgeWithNodeReferences[Id, L] =
    apply(edge, id, edge.getClass.getSimpleName, label)

  def apply[N, Id <: AnyVal | String, L](
      edge: AnyUnDiEdge[N],
      id: N => Id,
      edgeT: String,
      label: L
  ): UnDiEdgeWithNodeReferences[Id, L] =
    new UnDiEdgeWithNodeReferences[Id, L](
      edgeT,
      id(edge.node1),
      id(edge.node2),
      label
    )

case class DiHyperEdgeWithNodeReferences[Id <: AnyVal | String, L](
    edgeT: String,
    sourceIds: Iterable[Id],
    targetIds: Iterable[Id],
    label: L
) extends AnyDiHyperEdgeWithNodeReferences[Id, L]

object DiHyperEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String: ClassTag, L](
      edge: AnyDiHyperEdge[N],
      id: N => Id,
      label: L
  ): AnyDiHyperEdgeWithNodeReferences[Id, L] =
    apply(edge, id, edge.getClass.getSimpleName, label)

  def apply[N, Id <: AnyVal | String: ClassTag, L](
      edge: AnyDiHyperEdge[N],
      id: N => Id,
      edgeT: String,
      label: L
  ): AnyDiHyperEdgeWithNodeReferences[Id, L] =
    edge match
      case e: AnyDiEdge[N]      => DiEdgeWithNodeReferences(e, id, edgeT, label)
      case h: AnyDiHyperEdge[N] =>
        def toIterator(ends: OneOrMore[N]) = ArraySeq.from(ends.iterator map id)
        new DiHyperEdgeWithNodeReferences[Id, L](
          edgeT,
          toIterator(edge.sources),
          toIterator(edge.targets),
          label
        )

case class HyperEdgeWithNodeReferences[Id <: AnyVal | String, L](edgeT: String, endIds: Iterable[Id], label: L)
    extends AnyHyperEdgeWithNodeReferences[Id, L]

object HyperEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String: ClassTag, L](
      edge: AnyHyperEdge[N],
      id: N => Id,
      label: L
  ): AnyHyperEdgeWithNodeReferences[Id, L] =
    apply(edge, id, edge.getClass.getSimpleName, label)

  def apply[N, Id <: AnyVal | String: ClassTag, L](
      edge: AnyHyperEdge[N],
      id: N => Id,
      edgeT: String,
      label: L
  ): AnyHyperEdgeWithNodeReferences[Id, L] =
    edge match
      case e: AnyDiEdge[N]      => DiEdgeWithNodeReferences(e, id, edgeT, label)
      case e: AnyUnDiEdge[N]    => UnDiEdgeWithNodeReferences(e, id, edgeT, label)
      case h: AnyDiHyperEdge[N] => DiHyperEdgeWithNodeReferences(h, id, edgeT, label)
      case h: AnyHyperEdge[N]   =>
        new HyperEdgeWithNodeReferences[Id, L](edgeT, ArraySeq.from(edge.ends.iterator map id), label)
