package scalax.collection.constrained

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.language.higherKinds
import org.scalatest.Matchers
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.constrained.generic.GraphConstrainedCompanion

trait Testing[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] { this: Matchers =>

  def factory: GraphConstrainedCompanion[CC]

  protected def shouldLeaveGraphUnchanged[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E])(
      op: CC[N, E] => Either[ConstraintViolation, CC[N, E]]): Unit = {
    val before = factory.from(g.nodes.toOuter, g.edges.toOuter)(g.edgeT, g.config)
    op(g) should be('Left)
    g should ===(before)
  }

  type PlainOp[N, E[X] <: EdgeLikeIn[X], A]   = (CC[N, E], A) => CC[N, E]
  type VerboseOp[N, E[X] <: EdgeLikeIn[X], A] = (CC[N, E], A) => Either[ConstraintViolation, CC[N, E]]
  type Results[N, E[X] <: EdgeLikeIn[X]]      = (CC[N, E], CC[N, E], Either[ConstraintViolation, CC[N, E]])

  protected def given[N, E[X] <: EdgeLikeIn[X], A](g: CC[N, E], arg: A): (CC[N, E], A) = (g, arg)

  implicit final protected class GraphAndArg[N, E[X] <: EdgeLikeIn[X], A](val gA: (CC[N, E], A)) {
    def both(op1: PlainOp[N, E, A], op2: VerboseOp[N, E, A]): Results[N, E] = {
      val (before, arg) = gA
      (before, op1(before, arg), op2(before, arg))
    }

  }

  def meet[N, E[X] <: EdgeLikeIn[X]](p: CC[N, E] => Boolean): Matcher[Results[N, E]] =
    new Matcher[Results[N, E]] {
      def apply(left: Results[N, E]): MatchResult = left match {
        case (before, after, afterVerbose) =>
          def msg(key: String): String = s"$key of the operators are not returning the expected result"
          MatchResult(p(after) && (afterVerbose match {
            case Right(g) => p(g)
            case Left(_)  => p(before)
          }), msg("one or both"), msg("none"))
      }
    }

}
