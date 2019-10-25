package scalax.collection.visualization

import scala.language.higherKinds
import scala.util.{Failure, Success}

import org.scalatest.exceptions.TestFailedException
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.{Graph, GraphLike}

/** Scalatest support for graph visualization in case of failures.
  */
trait Visualizer[G[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]] extends Drawable {

  def factory: GraphCoreCompanion[G]

  final def given[N, E[+X] <: EdgeLikeIn[X]](graph: G[N, E])(test: G[N, E] => Unit): Unit = {

    def reThrow(tExc: TestFailedException, secondLine: String) =
      throw tExc.modifyMessage(_.map(testMessage => s"""$testMessage
                                                       |$secondLine
       """.stripMargin))

    try test(graph)
    catch {
      case tExc: TestFailedException =>
        makeImage(
          graph.asInstanceOf[Graph[N, E]],
          path = "log/",
          name = (tExc.failedCodeFileName match {
            case Some(fileName) => fileName
            case None           => "failed_test"
          }) + (tExc.failedCodeLineNumber match {
            case Some(number) => "_line" + number.toString
            case None         => ""
          }) + ".png"
        ) match {
          case Success(f) => reThrow(tExc, s"The graph image is available at file://${f.getAbsolutePath}")
          case Failure(e) => reThrow(tExc, s"Graph image generation failed with `${e.getMessage}`.")
        }
    }
  }
}
