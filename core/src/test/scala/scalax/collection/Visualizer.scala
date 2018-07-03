package scalax.collection

import scala.language.higherKinds

import org.scalatest.exceptions.TestFailedException

import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.generic.GraphCoreCompanion

trait Visualizer[G[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, G]] extends Drawable {

  def factory: GraphCoreCompanion[G]

  final def given[N, E[X] <: EdgeLikeIn[X]](graph: G[N, E])(test: G[N, E] => Unit): Unit =

    try test(graph)
    catch {
      case e: TestFailedException =>
        // output graph image
        val path = "log/"
        val name =
          (e.failedCodeFileName match {
            case Some(fileName) => fileName
            case None => "failed_test"
          }) + (e.failedCodeLineNumber match {
            case Some(number) => "_line" + number.toString
            case None => ""
          }) + ".png"
        toImageFile[N, E](graph.asInstanceOf[Graph[N, E]], path + name)
        // re-throw modified exception
        throw e.modifyMessage(_.flatMap(msg => Some(msg + "\nCheck graph image at " + path + name)))
    }

}
