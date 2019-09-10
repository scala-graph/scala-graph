package scalax.collection.constrained
package mutable

import scala.language.higherKinds
import scala.collection.Set

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.mutable.{AdjacencyListGraph => SimpleAdjacencyListGraph}
import scalax.collection.config.{AdjacencyListArrayConfig, GraphConfig}

import PreCheckFollowUp._
import config.GenConstrainedConfig

/** Implements an adjacency list based graph representation.
  *
  * An adjacency list based representation speeds up traversing a graph along its paths
  * by storing the list of connecting edges to each node.
  *
  * @author Peter Empen
  */
trait AdjacencyListGraph[
    N, E[X] <: EdgeLikeIn[X], +This[X, Y[X] <: EdgeLikeIn[X]] <: AdjacencyListGraph[X, Y, This] with Graph[X, Y]]
    extends SimpleAdjacencyListGraph[N, E, This]
    with GraphLike[N, E, This] {
  selfGraph: This[N, E] =>

  protected type Config <: GraphConfig with GenConstrainedConfig with AdjacencyListArrayConfig

  override protected def initialize(nodes: Traversable[N], edges: Traversable[E[N]]): Unit = withoutChecks {
    super.initialize(nodes, edges)
  }

  type NodeSetT = NodeSet
  @SerialVersionUID(8083L)
  final class NodeSet extends super[AdjacencyListGraph].NodeSet with super.NodeSet {

    override def add(node: NodeT): Boolean = add_?(node) getOrElse false

    def add_?(node: NodeT): Either[ConstraintViolation, Boolean] = {
      def doAdd = { coll += node; true }
      if (coll.contains(node)) Right(false)
      else if (checkSuspended) Right(doAdd)
      else {
        val preCheckResult = preAdd(node)
        preCheckResult.followUp match {
          case Complete => Right(doAdd)
          case PostCheck =>
            doAdd
            postAdd(AdjacencyListGraph.this, Set(node.value), Set.empty, preCheckResult).fold(
              { failure =>
                withoutChecks(coll -= node)
                Left(failure)
              },
              _ => Right(true)
            )
          case Abort => Left(preCheckResult)
        }
      }
    }
  }

  type EdgeSetT = EdgeSet
  @SerialVersionUID(8084L)
  final class EdgeSet extends super.EdgeSet {

    override def add(edge: EdgeT): Boolean = add_?(edge) getOrElse false

    def add_?(edge: EdgeT): Either[ConstraintViolation, Boolean] = {
      def added = super.add(edge)
      if (checkSuspended) Right(added)
      else {
        val preCheckResult = preAdd(edge.toOuter)
        preCheckResult.followUp match {
          case Complete => Right(added)
          case PostCheck =>
            if (added)
              postAdd(selfGraph, Set.empty[N], Set(edge.toOuter), preCheckResult).fold(
                failure => {
                  remove(edge)
                  (edge.nodes filterNot contains) foreach nodes.remove
                  Left(failure)
                },
                _ => Right(true)
              )
            else Right(false)
          case Abort => Left(preCheckResult)
        }
      }
    }

    protected def checkedRemove(edge: EdgeT,
                                forced: Boolean,
                                remove: EdgeT => Boolean): Either[ConstraintViolation, Boolean] =
      if (checkSuspended) Right(remove(edge))
      else {
        val preCheckResult = preSubtract(edge.asInstanceOf[self.EdgeT], !forced)
        preCheckResult.followUp match {
          case Complete => Right(remove(edge))
          case PostCheck =>
            if (remove(edge))
              postSubtract(selfGraph, Set.empty[N], Set(edge.toOuter), preCheckResult).fold(
                failure => { selfGraph += edge; Left(failure) },
                _ => Right(true)
              )
            else Right(false)
          case Abort => Left(preCheckResult)
        }
      }

    override def remove(edge: EdgeT): Boolean                       = remove_?(edge) getOrElse false
    def remove_?(edge: EdgeT): Either[ConstraintViolation, Boolean] = checkedRemove(edge, forced = false, super.remove)

    override def removeWithNodes(edge: EdgeT): Boolean = removeWithNodes_?(edge) getOrElse false
    def removeWithNodes_?(edge: EdgeT): Either[ConstraintViolation, Boolean] =
      checkedRemove(edge, forced = true, (e: EdgeT) => withoutChecks(super.removeWithNodes(e)))
  }

  def add_?(node: N): Either[ConstraintViolation, Boolean]    = nodes add_? Node(node)
  def add_?(edge: E[N]): Either[ConstraintViolation, Boolean] = edges add_? Edge(edge)

  def remove_?(node: N): Either[ConstraintViolation, Boolean]    = nodes remove_? Node(node)
  def remove_?(edge: E[N]): Either[ConstraintViolation, Boolean] = edges remove_? Edge(edge)
}
