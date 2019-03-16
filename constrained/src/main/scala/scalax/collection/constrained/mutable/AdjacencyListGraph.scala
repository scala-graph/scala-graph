package scalax.collection.constrained
package mutable

import scala.language.{higherKinds, postfixOps}
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
    extends GraphLike[N, E, This]
    with SimpleAdjacencyListGraph[N, E, This] {
  selfGraph: This[N, E] =>

  protected type Config <: GraphConfig with GenConstrainedConfig with AdjacencyListArrayConfig

  override protected def initialize(nodes: Traversable[N], edges: Traversable[E[N]]): Unit = withoutChecks {
    super.initialize(nodes, edges)
  }

  @SerialVersionUID(8083L)
  class NodeSet extends super.NodeSet {

    override def add(node: NodeT): Boolean = {
      def doAdd  = coll += node
      var handle = false
      if (coll.contains(node)) false
      else if (checkSuspended) doAdd
      else {
        val preCheckResult = preAdd(node)
        preCheckResult.followUp match {
          case Complete => doAdd
          case PostCheck =>
            doAdd
            if (!postAdd(AdjacencyListGraph.this, Set(node.value), Set.empty[E[N]], preCheckResult)) {
              handle = true
              withoutChecks(coll -= node)
            }
          case Abort => handle = true
        }
        if (handle)
          onAdditionRefused(Set(node), Set.empty[E[N]], AdjacencyListGraph.this)
      }
      !handle
    }
  }

  @SerialVersionUID(8084L)
  class EdgeSet extends super.EdgeSet {

    override def add(edge: EdgeT): Boolean = {
      val newNodes      = edge.nodes filterNot contains
      def doAdd         = super.add(edge)
      var added, handle = false
      if (checkSuspended) added = doAdd
      else {
        val preCheckResult = preAdd(edge.toOuter)
        preCheckResult.followUp match {
          case Complete => added = doAdd
          case PostCheck =>
            added = doAdd
            if (added)
              if (!postAdd(selfGraph, Set.empty[N], Set(edge.toOuter), preCheckResult)) {
                handle = true
                remove(edge)
                newNodes foreach nodes.remove
              }
          case Abort => handle = true
        }
      }
      if (handle)
        onAdditionRefused(Set.empty[N], Set(edge.toOuter), AdjacencyListGraph.this)
      added && !handle
    }

    /** generic constrained subtraction */
    protected def checkedRemove(edge: EdgeT, forced: Boolean, remove: EdgeT => Boolean): Boolean = {
      var removed, handle = false
      if (checkSuspended) removed = remove(edge)
      else {
        val preCheckResult = preSubtract(edge.asInstanceOf[self.EdgeT], !forced)
        preCheckResult.followUp match {
          case Complete => removed = remove(edge)
          case PostCheck =>
            removed = remove(edge)
            if (removed)
              if (!postSubtract(selfGraph, Set.empty[N], Set(edge.toOuter), preCheckResult)) {
                handle = true
                selfGraph += edge
              }
          case Abort => handle = true
        }
      }
      if (handle) onSubtractionRefused(Set.empty[self.NodeT], Set(edge.asInstanceOf[self.EdgeT]), selfGraph)
      removed && !handle
    }

    override def remove(edge: EdgeT): Boolean = checkedRemove(edge, false, super.remove)

    override def removeWithNodes(edge: EdgeT): Boolean = {
      def uncheckedSuperRemoveWithNodes(e: EdgeT) =
        withoutChecks { super.removeWithNodes(e) }
      checkedRemove(edge, true, uncheckedSuperRemoveWithNodes)
    }
  }
}
