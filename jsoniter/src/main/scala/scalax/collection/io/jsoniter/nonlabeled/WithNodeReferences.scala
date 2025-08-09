package scalax.collection.io.jsoniter.nonlabeled

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalax.collection.generic.{AnyDiEdge, AnyDiHyperEdge, AnyHyperEdge, AnyUnDiEdge}
import scalax.collection.{OneOrMore, Several}

/** ADT for JSON codecs of non-labeled edges where edge ends are represented by node IDs.
  */
sealed trait WithNodeReferences[Id <: AnyVal | String]:
  def edgeT: String

sealed trait AnyHyperEdgeWithNodeReferences[Id <: AnyVal | String]   extends WithNodeReferences[Id]
sealed trait AnyDiHyperEdgeWithNodeReferences[Id <: AnyVal | String] extends AnyHyperEdgeWithNodeReferences[Id]
sealed trait AnyEdgeWithNodeReferences[Id <: AnyVal | String]        extends AnyHyperEdgeWithNodeReferences[Id]:
  def id1: Id
  def id2: Id

object AnyHyperEdgeWithNodeReferences:
  inline def compactClassNames: CodecMakerConfig =
    CodecMakerConfig.withAdtLeafClassNameMapper { className =>
      JsonCodecMaker.simpleClassName(className) match
        case "HyperEdgeWithNodeReferences"   => "HyperR"
        case "DiHyperEdgeWithNodeReferences" => "DiHyperR"
        case "UnDiEdgeWithNodeReferences"    => "UnDiR"
        case "DiEdgeWithNodeReferences"      => "DiR"
        case x                               => throw new IllegalArgumentException(s"Unexpected className $x")
    }

object AnyDiHyperEdgeWithNodeReferences:
  inline def compactClassNames: CodecMakerConfig =
    CodecMakerConfig.withAdtLeafClassNameMapper { className =>
      JsonCodecMaker.simpleClassName(className) match
        case "DiHyperEdgeWithNodeReferences" => "DiHyperR"
        case "DiEdgeWithNodeReferences"      => "DiR"
        case x                               => throw new IllegalArgumentException(s"Unexpected className $x")
    }

object AnyEdgeWithNodeReferences:
  def unapply[Id <: AnyVal | String](e: AnyEdgeWithNodeReferences[Id]): (String, Id, Id) =
    (e.edgeT, e.id1, e.id2)

  inline def compactClassNames: CodecMakerConfig =
    CodecMakerConfig.withAdtLeafClassNameMapper { className =>
      JsonCodecMaker.simpleClassName(className) match
        case "UnDiEdgeWithNodeReferences" => "UnDiR"
        case "DiEdgeWithNodeReferences"   => "DiR"
        case x                            => throw new IllegalArgumentException(s"Unexpected className $x")
    }

case class DiEdgeWithNodeReferences[Id <: AnyVal | String](edgeT: String, sourceId: Id, targetId: Id)
    extends AnyEdgeWithNodeReferences[Id]
    with AnyDiHyperEdgeWithNodeReferences[Id]:
  def id1: Id = sourceId
  def id2: Id = targetId

object DiEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String](edge: AnyDiEdge[N], id: N => Id): DiEdgeWithNodeReferences[Id] =
    apply(edge, id, edge.getClass.getSimpleName)

  def apply[N, Id <: AnyVal | String](edge: AnyDiEdge[N], id: N => Id, edgeT: String): DiEdgeWithNodeReferences[Id] =
    new DiEdgeWithNodeReferences[Id](
      edgeT,
      id(edge.source),
      id(edge.target)
    )

  import scalax.collection.edges.DiEdge
  def diEdgeFactory[N]: PartialFunction[(String, N, N), DiEdge[N]] =
    case ("DiEdge", source, target) => DiEdge(source, target)

case class UnDiEdgeWithNodeReferences[Id <: AnyVal | String](edgeT: String, id1: Id, id2: Id)
    extends AnyEdgeWithNodeReferences[Id]

object UnDiEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String](edge: AnyUnDiEdge[N], id: N => Id): UnDiEdgeWithNodeReferences[Id] =
    apply(edge, id, edge.getClass.getSimpleName)

  def apply[N, Id <: AnyVal | String](
      edge: AnyUnDiEdge[N],
      id: N => Id,
      edgeT: String
  ): UnDiEdgeWithNodeReferences[Id] =
    new UnDiEdgeWithNodeReferences[Id](
      edgeT,
      id(edge.node1),
      id(edge.node2)
    )

  import scalax.collection.edges.UnDiEdge
  def unDiEdgeFactory[N]: PartialFunction[(String, N, N), UnDiEdge[N]] = { case ("UnDiEdge", source, target) =>
    UnDiEdge(source, target)
  }

case class DiHyperEdgeWithNodeReferences[Id <: AnyVal | String](
    edgeT: String,
    sourceIds: Iterable[Id],
    targetIds: Iterable[Id]
) extends AnyDiHyperEdgeWithNodeReferences[Id]

object DiHyperEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String: ClassTag](
      edge: AnyDiHyperEdge[N],
      id: N => Id
  ): AnyDiHyperEdgeWithNodeReferences[Id] =
    apply(edge, id, edge.getClass.getSimpleName)

  def apply[N, Id <: AnyVal | String: ClassTag](
      edge: AnyDiHyperEdge[N],
      id: N => Id,
      edgeT: String
  ): AnyDiHyperEdgeWithNodeReferences[Id] =
    edge match
      case e: AnyDiEdge[N]      => DiEdgeWithNodeReferences(e, id, edgeT)
      case h: AnyDiHyperEdge[N] =>
        def toIterator(ends: OneOrMore[N]) = ArraySeq.from(ends.iterator map id)
        new DiHyperEdgeWithNodeReferences[Id](
          edgeT,
          toIterator(edge.sources),
          toIterator(edge.targets)
        )

  import scalax.collection.hyperedges.{ordered, DiHyperEdge}
  def diHyperEdgeFactory[N]: PartialFunction[(String, OneOrMore[N], OneOrMore[N]), DiHyperEdge[N]] = {
    case ("DiHyperEdge", sources, targets) =>
      DiHyperEdge(sources, targets)
  }
  def orderedDiHyperEdgeFactory[N]: PartialFunction[(String, OneOrMore[N], OneOrMore[N]), ordered.DiHyperEdge[N]] = {
    case ("ordered.DiHyperEdge", sources, targets) =>
      ordered.DiHyperEdge(sources, targets)
  }

case class HyperEdgeWithNodeReferences[Id <: AnyVal | String](edgeT: String, endIds: Iterable[Id])
    extends AnyHyperEdgeWithNodeReferences[Id]

object HyperEdgeWithNodeReferences:
  def apply[N, Id <: AnyVal | String: ClassTag](
      edge: AnyHyperEdge[N],
      id: N => Id
  ): AnyHyperEdgeWithNodeReferences[Id] =
    apply(edge, id, edge.getClass.getSimpleName)

  def apply[N, Id <: AnyVal | String: ClassTag](
      edge: AnyHyperEdge[N],
      id: N => Id,
      edgeT: String
  ): AnyHyperEdgeWithNodeReferences[Id] =
    edge match
      case e: AnyDiEdge[N]      => DiEdgeWithNodeReferences(e, id, edgeT)
      case e: AnyUnDiEdge[N]    => UnDiEdgeWithNodeReferences(e, id, edgeT)
      case h: AnyDiHyperEdge[N] => DiHyperEdgeWithNodeReferences(h, id, edgeT)
      case h: AnyHyperEdge[N]   =>
        new HyperEdgeWithNodeReferences[Id](edgeT, ArraySeq.from(edge.ends.iterator map id))

  import scalax.collection.hyperedges.{ordered, HyperEdge}
  def hyperEdgeFactory[N]: PartialFunction[(String, Several[N]), HyperEdge[N]] = { case ("HyperEdge", ends) =>
    HyperEdge(ends)
  }
  def orderedHyperEdgeFactory[N]: PartialFunction[(String, Several[N]), ordered.HyperEdge[N]] = {
    case ("ordered.HyperEdge", ends) =>
      ordered.HyperEdge(ends)
  }
