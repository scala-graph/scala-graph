package scalax.collection.constrained

import scala.language.higherKinds

import org.scalatest.Matchers
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.constrained.generic.GraphConstrainedCompanion

trait Testing[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] { this: Matchers =>

  def factory: GraphConstrainedCompanion[CC]

  protected def shouldLeaveGraphUnchanged[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E])(
      op: g.type => Either[ConstraintViolation, g.type]): Unit = {
    val before = factory.from(g.nodes.toOuter, g.edges.toOuter)(g.edgeT, g.config)
    op(g) should be('Left)
    g should ===(before)
  }
}
