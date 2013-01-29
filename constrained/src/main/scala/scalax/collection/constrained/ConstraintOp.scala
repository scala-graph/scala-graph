package scalax.collection.constrained

import scala.collection.Set
import scala.collection.mutable.{Map => MMap}

import scalax.collection.GraphPredef._
import PreCheckFollowUp._

sealed trait Op
sealed trait BinaryOp extends Op
case object And extends BinaryOp 
case object Or  extends BinaryOp

abstract class ConstraintOp[N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E], val operator: Op)
  extends Constraint[N,E] (self)

class ConstraintBinaryOp[N, E[X] <: EdgeLikeIn[X]]
     (override val self: Graph[N,E],
      operator:          BinaryOp,
      left:              Constraint[N,E],
      right:             Constraint[N,E]) 
  extends ConstraintOp[N,E] (self, operator)
{
  assert((self eq left.self) && (left.self eq right.self))

  protected class PreCheckResults(override val followUp: PreCheckFollowUp,
                                  constraint:            Constraint[N,E],
                                  result:                PreCheckResult) 
    extends PreCheckResult(followUp)
  {
    val results: MMap[Constraint[N,E], PreCheckResult] = result match {
      case r: PreCheckResults => MMap(r.results.toSeq: _*)
      case _                  => MMap(constraint -> result)
    }
    def += (op: Constraint[N,E], result: PreCheckResult) = {
      result match {
        case r: PreCheckResults => results ++= r.results
        case _                  => results  += op -> result
      }
      this
    }
    override def get[N, E[X] <: EdgeLikeIn[X]](op: Constraint[N,E]) =
      results.asInstanceOf[MMap[Constraint[N,E], PreCheckResult]] get op
  }
  protected
  def eval(left:  Constraint[N,E], leftResult:  PreCheckResult,
           right: Constraint[N,E], rightResult: => PreCheckResult): PreCheckResult = {
    var rightDone = false
    val       leftFollowUp = leftResult.followUp
    lazy val rightFollowUp = {rightDone = true; rightResult.followUp }
    val followUp =
      operator match {
        case And => if (leftFollowUp != Abort) min(leftFollowUp, rightFollowUp)
                    else                       Abort
        case Or  => if (leftFollowUp != Abort) leftFollowUp
                    else                       rightFollowUp
      }
    if (followUp == Abort) PreCheckResult(Abort)
    else {
      if (rightDone) {
        leftResult match {
          case r: PreCheckResults => r += (right, rightResult)
          case _                  => new PreCheckResults( min(leftFollowUp, rightFollowUp),
                                                          left, leftResult) += (right, rightResult)
        }
      } else leftResult
    }
  }
  protected def eval(left: Boolean, right: => Boolean): Boolean = {
    operator match {
      case And => left && right
      case Or  => left || right
    }
  }
  private type LNodeT = left .self.NodeT
  private type RNodeT = right.self.NodeT
  private type LEdgeT = left .self.EdgeT
  private type REdgeT = right.self.EdgeT

  override final def preCreate(nodes: collection.Iterable[N],
                               edges: collection.Iterable[E[N]]) =
    eval(left,  left  preCreate(nodes, edges),
         right, right preCreate(nodes, edges))
  override final def preAdd(node: N   ) = eval(left, left preAdd node, right, right preAdd node)
  override final def preAdd(edge: E[N]) = eval(left, left preAdd edge, right, right preAdd edge)
  override final def preAdd(elems: GraphParamIn[N,E]*) =
    eval(left,  left  preAdd (elems: _*),
         right, right preAdd (elems: _*)) 
  override final def postAdd (newGraph   : scalax.collection.constrained.Graph[N,E],
                                passedNodes: Iterable[N],
                                passedEdges: Iterable[E[N]],
                                preCheck   : PreCheckResult) =
    eval(left  postAdd (newGraph, passedNodes, passedEdges, preCheck),
         right postAdd (newGraph, passedNodes, passedEdges, preCheck))
  override final def preSubtract (node: self.NodeT, forced: Boolean) =
    eval(left,  left  preSubtract (node.asInstanceOf[LNodeT], forced),
         right, right preSubtract (node.asInstanceOf[RNodeT], forced))
  override final def preSubtract (edge: self.EdgeT, simple: Boolean) =
    eval(left,  left  preSubtract (edge.asInstanceOf[LEdgeT], simple),
         right, right preSubtract (edge.asInstanceOf[REdgeT], simple))
  override final def preSubtract (nodes: => Set[self.NodeT],
                                  edges: => Set[self.EdgeT], simple: Boolean) =
    eval(left,  left  preSubtract (nodes.asInstanceOf[Set[LNodeT]], edges.asInstanceOf[Set[LEdgeT]], simple),
         right, right preSubtract (nodes.asInstanceOf[Set[RNodeT]], edges.asInstanceOf[Set[REdgeT]], simple))
  override final def postSubtract(newGraph   : Graph[N,E],
                                    passedNodes: Iterable[N],
                                    passedEdges: Iterable[E[N]],
                                    preCheck   : PreCheckResult) =
    eval(left  postSubtract (newGraph, passedNodes, passedEdges, preCheck),
         right postSubtract (newGraph, passedNodes, passedEdges, preCheck))
  override final def onAdditionRefused (refusedNodes: Iterable[N],
                                        refusedEdges: Iterable[E[N]],
                                        graph:        Graph[N,E]) = {
    left .onAdditionRefused(refusedNodes, refusedEdges, graph) ||
    right.onAdditionRefused(refusedNodes, refusedEdges, graph)
  }
  override final def onSubtractionRefused(refusedNodes: Iterable[Graph[N,E]#NodeT],
                                          refusedEdges: Iterable[Graph[N,E]#EdgeT],
                                          graph:        Graph[N,E]) = {
    left .onSubtractionRefused(refusedNodes, refusedEdges, graph) ||
    right.onSubtractionRefused(refusedNodes, refusedEdges, graph)
  }
}
/** Base class for any operation on `ConstraintCompanion`s. */
abstract class ConstraintCompanionOp (val operator: Op)
  extends ConstraintCompanion[Constraint]

/** Facilitates binary operations on `ConstraintCompanion`s. */
class ConstraintCompanionBinaryOp(operator: BinaryOp,
                                  left:     ConstraintCompanion[Constraint],
                                  right:    ConstraintCompanion[Constraint])
  extends ConstraintCompanionOp(operator)
{
  def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
    new ConstraintBinaryOp[N,E] (self, operator, left(self), right(self))
}
