package scalax.collection.constrained

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.language.higherKinds
import org.scalatest.Matchers

import scalax.collection.{Graph => SimpleGraph}
import scalax.collection.GraphPredef.EdgeLikeIn
import scalax.collection.constrained.generic.GraphConstrainedCompanion

trait Testing[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] { this: Matchers =>

  def factory: GraphConstrainedCompanion[CC]

  protected def shouldLeaveGraphUnchanged[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E])(
      op: CC[N, E] => Either[ConstraintViolation, CC[N, E]]): Unit = {
    val before = isolated(g)
    op(g) should be('Left)
    g should ===(before)
  }

  protected type PlainOp[N, E[X] <: EdgeLikeIn[X], A]   = (CC[N, E], A) => CC[N, E]
  protected type VerboseOp[N, E[X] <: EdgeLikeIn[X], A] = (CC[N, E], A) => Either[ConstraintViolation, CC[N, E]]
  protected type Results[N, E[X] <: EdgeLikeIn[X]]      = (CC[N, E], CC[N, E], Either[ConstraintViolation, CC[N, E]])

  protected def given[N, E[X] <: EdgeLikeIn[X], A](g: CC[N, E], arg: A): (CC[N, E], A) = (g, arg)

  implicit final protected class GraphAndArg[N, E[X] <: EdgeLikeIn[X], A](val gA: (CC[N, E], A)) {
    def both(op1: PlainOp[N, E, A], op2: VerboseOp[N, E, A]): Results[N, E] = {
      val (before, arg) = gA
      (before, op1(isolated(before), arg), op2(isolated(before), arg))
    }
  }

  protected def equal[N, E[X] <: EdgeLikeIn[X]](expected: SimpleGraph[N, E]): Matcher[Results[N, E]] =
    meet(_ === expected)

  protected def meet[N, E[X] <: EdgeLikeIn[X]](p: CC[N, E] => Boolean): Matcher[Results[N, E]] =
    new Matcher[Results[N, E]] {
      def apply(left: Results[N, E]): MatchResult = left match {
        case (before, after, afterVerbose) =>
          val silentResult = p(after)
          val verboseResult = afterVerbose match {
            case Right(g) => p(g)
            case Left(_)  => p(before)
          }
          def msg(negate: Boolean): String =
            s"""One or both operations ${if (negate) "" else "don't "}meet the expectation:
               |  before:     $before
               |  silent  op: $after
               |  verbose op: $afterVerbose""".stripMargin
          MatchResult(silentResult && verboseResult, msg(false), msg(true))
      }
    }

  protected def beRejected[N, E[X] <: EdgeLikeIn[X]]: Matcher[Results[N, E]] =
    new Matcher[Results[N, E]] {
      def apply(left: Results[N, E]): MatchResult = left match {
        case (before, after, afterVerbose) =>
          val silentResult  = after === before
          val verboseResult = afterVerbose.isLeft
          def msg(negate: Boolean): String =
            s"""One or both operations have ${if (negate) "" else "not "}been rejected:
               |  before:     $before
               |  silent  op: $after
               |  verbose op: $afterVerbose""".stripMargin
          MatchResult(silentResult && verboseResult, msg(false), msg(true))
      }
    }

  private def isolated[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E]): CC[N, E] =
    g match {
      case mG: mutable.Graph[N, E] => factory.from(mG.nodes.toOuter, mG.edges.toOuter)(mG.edgeT, mG.config)
      case iG                      => iG
    }
}
