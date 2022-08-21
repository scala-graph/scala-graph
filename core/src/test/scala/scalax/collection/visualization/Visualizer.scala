package scalax.collection.visualization

import scala.util.{Failure, Success}

import org.scalatest.exceptions.TestFailedException

import scalax.collection.generic.Edge
import scalax.collection.AnyGraph

/** Scalatest support for graph visualization in case of failures.
  */
trait Visualizer extends Drawable {

  final def given[N, E <: Edge[N]](graph: AnyGraph[N, E])(test: AnyGraph[N, E] => Unit): Unit = {

    def reThrow(ex: TestFailedException, secondLine: String) =
      throw ex.modifyMessage(_.map { testMessage =>
        s"""$testMessage
           |$secondLine
       """.stripMargin
      })

    try test(graph)
    catch {
      case ex: TestFailedException =>
        makeImage(
          graph,
          path = "log/",
          name = (ex.failedCodeFileName match {
            case Some(fileName) => fileName
            case None           => "failed_test"
          }) + (ex.failedCodeLineNumber match {
            case Some(number) => "_line" + number.toString
            case None         => ""
          }) + ".png"
        ) match {
          case Success(f) => reThrow(ex, s"The graph image is available at file://${f.getAbsolutePath}")
          case Failure(e) => reThrow(ex, s"Graph image generation failed with `${e.getMessage}`.")
        }
    }
  }
}
