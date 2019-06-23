package scalax.collection.constrained
package mutable

import scala.collection.{GenTraversableOnce, Set}
import scala.language.higherKinds

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.mutable.{GraphLike => SimpleGraphLike}

/* Operations for mutable constrained graphs that also return information on any constraint violation.
   These ops are counterparts of non-constrained mutable graph ops that do not expose constraint violations.
   These enhanced ops bear the same name but a postfix `?` for operator identifiers respectively `_?` for plain identifiers.

  $define Info additional information on any potential constraint violation
 */
trait GraphOps[
    N,
    E[X] <: EdgeLikeIn[X],
    +This[X, Y[X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Set[Param[X, Y]] with Graph[X, Y]
] { _: This[N, E] with SimpleGraphLike[N, E, This] with GraphOps[N, E, This] =>

  /** Same as `add` but $Info. */
  def add_?(node: N): Either[ConstraintViolation, Boolean] = ???

  /** Same as `+=` but $Info. */
  def +=?(node: N): Either[ConstraintViolation, This[N, E]] = ???

  /** Same as `+=` but $Info. */
  def +=?(elem: Param[N, E]): Either[ConstraintViolation, this.type] = ???

  protected def +=#?(edge: E[N]): Either[ConstraintViolation, this.type] = ???

  /** Same as `++=` but $Info. */
  def ++=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type]

  /** Same as `remove` but $Info. */
  def remove_?(node: N): Either[ConstraintViolation, Boolean] = ???

  /** Same as `-=` but $Info. */
  def -=?(node: N): Either[ConstraintViolation, This[N, E]] = ???

  /** Same as `-=` but $Info. */
  def -=?(elem: Param[N, E]): Either[ConstraintViolation, this.type] = ???

  protected def -=#?(edge: E[N]): Either[ConstraintViolation, this.type] = ???

  /** Same as `--=` but $Info. */
  def --=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type]
}
