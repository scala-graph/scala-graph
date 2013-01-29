package scalax.collection.constrained
package constraints

import scalax.collection.GraphPredef._
/**
 * The empty constraint treating any addition or subtraction as valid.
 */
class NoneConstraint[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
  extends Constraint[N,E] (self)
  with    ConstraintHandlerMethods[N,E]
{
  import PreCheckFollowUp.Complete
  override def preAdd     (node:   N ) = PreCheckResult(Complete)
  override def preAdd     (edge: E[N]) = PreCheckResult(Complete)
  override def preSubtract(node: self.NodeT, forced: Boolean) = PreCheckResult(Complete)
  override def preSubtract(node: self.EdgeT, forced: Boolean) = PreCheckResult(Complete)
}
object NoneConstraint extends ConstraintCompanion[NoneConstraint] {
  def apply [N, E[X] <: EdgeLikeIn[X]](self: Graph[N,E]) = new NoneConstraint[N,E](self) 
}