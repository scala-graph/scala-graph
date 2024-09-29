package scalax.collection.visualization

import org.scalatest.exceptions.TestFailedException

import scalax.collection.AnyGraph
import scalax.collection.ToString._
import scalax.collection.generic.Edge

/** Scalatest support for graph visualization in case of failures.
  *
  * Drawing is commented out because
  *   - org.gephi with all it's dependencies is rather heavy
  *   - managing Gephi releases proved cumbersome over time
  *   - there was no frequent usage
  *   - permission to write files needs be additionally configured in the CI.
  *
  * However it's intended to add a more lightweight drawing implementation in future.
  */
trait Visualizer /*extends Drawable*/ {

  final def withGraph[N, E <: Edge[N]](graph: AnyGraph[N, E])(test: AnyGraph[N, E] => Unit): Unit = {

    def reThrow(ex: TestFailedException, messageExtension: String) =
      throw ex.modifyMessage(_.map { testMessage =>
        s"""$testMessage
           |------------ given ------------
           |$messageExtension
           |-------------------------------""".stripMargin
      })

    try test(graph)
    catch {
      case ex: TestFailedException =>
        /*
        makeImage(
          graph,
          path = "log/",
          name = (ex.failedCodeFileName match {
            case Some(fileName) => fileName
            case None => "failed_test"
          }) + (ex.failedCodeLineNumber match {
            case Some(number) => "_line" + number.toString
            case None => ""
          }) + ".png"
        ) match {
          case Success(f) => reThrow(ex, s"The graph image is available at file://${f.getAbsolutePath}")
          case Failure(e) => reThrow(ex, s"Graph image generation failed with `${e.getMessage}`.")
        }
         */
        val small = graph.edges.size < 10 && graph.edges.toString.length < 100
        reThrow(ex, graph.render(if (small) SetsOnSeparateLines() else SetElemsOnSeparateLines()))
    }
  }
}
