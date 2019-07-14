package scalax.collection.constrained
package mutable

import scala.collection.Set
import scala.language.higherKinds

import scalax.collection.GraphPredef.{EdgeLikeIn, InnerEdgeParam, InnerNodeParam, OuterEdge, OuterNode, Param}
import scalax.collection.mutable.{GraphLike => SimpleGraphLike}

/* Operations for mutable constrained graphs that also return information on any constraint violation.
   These ops are counterparts of non-constrained mutable graph ops that do not expose constraint violations.
   These enhanced ops bear the same name but a postfix `?` for operator identifiers respectively `_?` for plain identifiers.

   $define Info returns additional information on any potential constraint violation
 */
trait GraphOps[
    N,
    E[X] <: EdgeLikeIn[X],
    +This[X, Y[X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Set[Param[X, Y]] with Graph[X, Y]
] { _: This[N, E] with SimpleGraphLike[N, E, This] with GraphOps[N, E, This] =>

  /** Same as `add` but $Info. */
  def add_?(node: N): Either[ConstraintViolation, Boolean]

  /** Same as `+=` but $Info. */
  def +=?(node: N): Either[ConstraintViolation, this.type] = add_?(node) map (_ => this)

  /** Same as `add` but $Info. */
  def add_?(edge: E[N]): Either[ConstraintViolation, Boolean]

  protected def +=#?(edge: E[N]): Either[ConstraintViolation, this.type] = add_?(edge) map (_ => this)

  /** Same as `+=` but $Info. */
  def +=?(elem: Param[N, E]): Either[ConstraintViolation, this.type] = elem match {
    case n: OuterNode[N]               => this +=? n.value
    case n: InnerNodeParam[N]          => this +=? n.value
    case e: OuterEdge[N, E]            => this +=#? e.edge
    case e: InnerEdgeParam[N, E, _, E] => this +=#? e.asEdgeTProjection[N, E].toOuter
  }

  /** Same as `++=` but $Info. */
  def ++=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type]

  /** Same as `remove` but $Info. */
  def remove_?(node: N): Either[ConstraintViolation, Boolean]

  /** Same as `-=` but $Info. */
  def -=?(node: N): Either[ConstraintViolation, this.type] = remove_?(node) map (_ => this)

  /** Same as `remove` but $Info. */
  def remove_?(edge: E[N]): Either[ConstraintViolation, Boolean]

  protected def -=#?(edge: E[N]): Either[ConstraintViolation, this.type] = remove_?(edge) map (_ => this)

  /** Same as `-=` but $Info. */
  def -=?(elem: Param[N, E]): Either[ConstraintViolation, this.type] = elem match {
    case n: OuterNode[N]               => this -=? n.value
    case n: InnerNodeParam[N]          => this -=? n.value
    case e: OuterEdge[N, E]            => this -=#? e.edge
    case e: InnerEdgeParam[N, E, _, E] => this -=#? e.asEdgeTProjection[N, E].toOuter
  }

  /** Same as `--=` but $Info. */
  def --=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type]
}
