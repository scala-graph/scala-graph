package scalax.collection

import scala.language.higherKinds

import org.scalatest.exceptions.TestFailedException

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.generic.GraphCoreCompanion

trait Visualizer[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]] {

  def factory: GraphCoreCompanion[G]

  final def given[N, E[X] <: EdgeLikeIn[X]](graph: G[N, E])(test: G[N, E] => Unit): Unit =
    try test(graph)
    catch {
      case e: TestFailedException =>
        visualize(graph)
        throw e
    }

  private def visualize[N, E[X] <: EdgeLikeIn[X]](graph: G[N, E]): String = "image file path"
}
