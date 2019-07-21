package scalax.collection.constrained
package constraints

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, postfixOps}
import scala.collection.Set
import scala.collection.mutable.{Set => MutableSet}

import scalax.collection.GraphPredef._
import scalax.collection.{Graph => SimpleGraph}
import PreCheckFollowUp._
import scalax.collection.config.CoreConfig

/** Ensures that the underlying `Graph` is acyclic at any time. */
class Acyclic[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G) extends Constraint[N, E, G](self) {
  override def preCreate(nodes: Traversable[N], edges: Traversable[E[N]]): PreCheckResult =
    PreCheckResult.postCheck(edges forall (_.nonLooping))

  /** Adding a single node cannot produce a cycle. */
  override def preAdd(node: N) = PreCheckResult(Complete)

  /** When inserting an edge with a source contained in the graph a cycle is produced
    * if there exists a target node of this edge such that there is a path from the target node to the source node.
    */
  def preAdd(edge: E[N]): PreCheckResult = PreCheckResult.complete(
    edge.nonLooping && {
      val isUndirected       = edge.isUndirected
      val checkFrom, checkTo = MutableSet.empty[self.NodeT]
      def find(n: N)         = self find n
      def add1(n: N) {
        find(n) map { inner =>
          checkFrom += inner
          if (isUndirected) checkTo += inner
        }
      }
      def add2(toSet: MutableSet[self.NodeT], n: N, nUndi: N) {
        find(n) map (inner => toSet += inner)
        find(nUndi) map { inner =>
          if (isUndirected) toSet += inner
        }
      }
      add2(checkTo, edge._1, edge._2)
      add2(checkFrom, edge._2, edge._1)
      for (n <- edge.iterator.drop(2))
        add1(n)

      !((for {
        from <- checkFrom
        to   <- checkTo
      } yield (from, to)) exists (t => t._1 hasSuccessor t._2))
    }
  )

  /** This class makes it possible to carry over nodes that are passed to a pre-check and
    *  are already contained in the graph to `postAdd`. Thus we can limit the number of start
    *  nodes for cycle detection in the post-check to these passed over docking nodes.
    */
  protected class Result(followUp: PreCheckFollowUp, val dockingNodes: Set[self.NodeT]) extends PreCheckResult(followUp)

  protected object Result extends PreCheckResultCompanion {
    def apply(followUp: PreCheckFollowUp)                                = new Result(followUp, Set.empty[self.NodeT])
    def apply(followUp: PreCheckFollowUp, nodesToCheck: Set[self.NodeT]) = new Result(followUp, nodesToCheck)
    def unapply(result: Result): Option[Set[self.NodeT]]                 = Some(result.dockingNodes)
  }

  /** If `elems` is relatively small, preventively ensure that there is no cycle within the new elements.
    */
  override def preAdd(elems: InParam[N, E]*): PreCheckResult =
    if (elems.size * 10 < self.size) {
      val p = Param.Partitions(elems)
      val graphAdd =
        SimpleGraph.from(p.toOuterNodes, p.toOuterEdges)(
          self.edgeT,
          CoreConfig(self.config.orderHint, self.config.adjacencyListHints))
      if (graphAdd.isCyclic)
        PreCheckResult(Abort)
      else {
        val found = (for (n <- p.toOuterNodes) yield self find n) filter (_.isDefined)
        Result(PostCheck, (found map (_.get)) toSet)
      }
    } else PreCheckResult(PostCheck)

  override def postAdd(newGraph: G @uV,
                       passedNodes: Traversable[N],
                       passedEdges: Traversable[E[N]],
                       preCheck: PreCheckResult): Either[PostCheckFailure, G] = {
    def msg(at: Option[self.NodeT]) =
      s"Unexpected cycle ${at.fold("")("at " + _)}when adding $passedNodes, $passedEdges."
    preCheck match {
      case Result(docking) =>
        docking find (_.findCycle.isDefined) match {
          case Some(node) => Left(PostCheckFailure(msg(Some(node))))
          case None       => Right(newGraph)
        }
      case _ =>
        if (newGraph.isAcyclic) Right(newGraph)
        else Left(PostCheckFailure(msg(None)))
    }
  }

  override def preSubtract(node: self.NodeT, forced: Boolean) = PreCheckResult(Complete)
  override def preSubtract(edge: self.EdgeT, forced: Boolean) = PreCheckResult(Complete)
  override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
    PreCheckResult(Complete)
}
object Acyclic extends ConstraintCompanion[Acyclic] {
  def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new Acyclic[N, E, G](self)
}
