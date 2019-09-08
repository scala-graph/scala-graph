package scalax.collection.constrained

import scala.collection.{GenTraversableOnce, Set}
import scala.language.higherKinds

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.{GraphLike => SimpleGraphLike}

/* Operations for constrained graphs that also return information on any constraint violation.
   These ops are counterparts of non-constrained graph ops that do not expose constraint violations.
   These enhanced ops bear the same name but a postfix `?` for operator identifiers respectively `_?` for plain identifiers.

   $define Info returns additional information on any potential constraint violation
 */
trait GraphOps[
    N,
    E[X] <: EdgeLikeIn[X],
    +This[X, Y[X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Set[Param[X, Y]] with Graph[X, Y]
] { _: This[N, E] with SimpleGraphLike[N, E, This] with GraphOps[N, E, This] =>

  /** Same as `+` but $Info. */
  def +?(node: N): Either[ConstraintViolation, This[N, E]]

  final override def +(elem: Param[N, E]): This[N, E] = +?(elem) getOrElse this

  /** Same as `+` but $Info. */
  def +?(elem: Param[N, E]): Either[ConstraintViolation, This[N, E]]

  final override def ++(elems: GenTraversableOnce[Param[N, E]]): This[N, E] = ++?(elems) getOrElse this

  /** Same as `++` but $Info. */
  def ++?(elems: GenTraversableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]]

  /** Same as `-` but $Info. */
  def -?(node: N): Either[ConstraintViolation, This[N, E]]

  final override def -(elem: Param[N, E]): This[N, E] = -?(elem) getOrElse this

  /** Same as `-` but $Info. */
  def -?(elem: Param[N, E]): Either[ConstraintViolation, This[N, E]]

  final override def --(elems: GenTraversableOnce[Param[N, E]]): This[N, E] = --?(elems) getOrElse this

  /** Same as `--` but $Info. */
  def --?(elems: GenTraversableOnce[Param[N, E]]): Either[ConstraintViolation, This[N, E]]
}
