package scalax.collection.constrained

import scala.collection.Set
import scala.language.higherKinds
import scala.util.Try

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import generic.GraphConstrainedCompanion
import org.scalatest._
import org.scalatest.refspec.RefSpec
class TConstrainedRootTest
    extends Suites(
      new TConstrained[immutable.Graph](immutable.Graph),
      new TConstrained[mutable.Graph](mutable.Graph),
      new TConstrainedMutable)

class TConstrainedMutable extends RefSpec with Matchers with Testing[mutable.Graph] {
  val factory = mutable.Graph

  object `constrains work as expected using mutable operations` {

    def `when constraining Int nodes to even numbers` {
      implicit val config: Config = UserConstraints.EvenNode

      val g = factory[Int, Nothing](1)

      g should be('isEmpty)
      (g += 2) should have size 1
      (g += 3) should have size 1
      (g ++= List(1, 4)) should have size 1
      (g ++= List(2, 4, 6)) should have size 3
    }

    def `when constraining nodes to have a minimum degree` {
      import UserConstraints.MinDegree_2

      implicit val config: Config = MinDegree_2

      val g = factory.empty[Int, UnDiEdge]

      given(g, List(2, 3, 4)) both (_ ++= _, _ ++=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1 ~ 2, 1 ~ 3, 2 ~ 4)) both (_ ++= _, _ ++=? _) should beRejected[Int, UnDiEdge]

      val validEdges = Set(1 ~ 2, 1 ~ 3, 2 ~ 3)
      given(g, validEdges) both (_ ++= _, _ ++=? _) should meet((_: Graph[Int, UnDiEdge]).edges == validEdges)
      g ++= validEdges

      given(g, 3 ~ 4) both (_ += _, _ +=? _) should beRejected[Int, UnDiEdge]
      given(g, 3) both (_ -= _, _ -=? _) should beRejected[Int, UnDiEdge]
      given(g, List(3)) both (_ --= _, _ --=? _) should beRejected[Int, UnDiEdge]
    }

    def `when postSubtract fails` {
      implicit val config: Config = UserConstraints.AlwaysFailingPostSubtract

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1)

      given(g, 1) both (_ -= _, _ -=? _) should beRejected[Int, UnDiEdge]
      given(g, 1 ~ 2) both (_ -= _, _ -=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1)) both (_ --= _, _ --=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1 ~ 2)) both (_ --= _, _ --=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1 ~ 2, 2 ~ 3)) both (_ --= _, _ --=? _) should beRejected[Int, UnDiEdge]
    }

    def `when postAdd fails` {
      import UserConstraints.FailingPostAdd
      implicit val config: Config = FailingPostAdd

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1)

      given(g, 5) both (_ += _, _ +=? _) should beRejected[Int, UnDiEdge]
      (g +=? 5) should be(Left(PostCheckFailure(FailingPostAdd.leftWarning)))
      given(g, 1 ~ 5) both (_ += _, _ +=? _) should beRejected[Int, UnDiEdge]
      given(g, List(5)) both (_ ++= _, _ ++=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1 ~ 5)) both (_ ++= _, _ ++=? _) should beRejected[Int, UnDiEdge]
      given(g, List(1 ~ 5, 5 ~ 6, 6 ~ 2)) both (_ ++= _, _ ++=? _) should beRejected[Int, UnDiEdge]
    }

    def `when cloning a graph` {
      implicit val config: Config = UserConstraints.ThrowingExceptionPreAdd

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3)
      Try(g.clone).isSuccess should be(true)
    }
  }
}

class TConstrained[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphConstrainedCompanion[CC])
    extends RefSpec
    with Matchers
    with Testing[CC] {

  info("factory = " + factory.getClass)

  type UnDi = Graph[Int, UnDiEdge]

  object `constrains take effect` {

    def `when constraining Int nodes to even numbers` {
      implicit val config: Config = UserConstraints.EvenNode

      val g = factory[Int, Nothing](1, 2, 3, 4)
      g should be('isEmpty)
      g + 5 contains 5 should be(false)
      g + 6 contains 6 should be(true)
      (g ++ List[OuterNode[Int]](1, 2, 3)) should be('isEmpty)
      (g ++ List[OuterNode[Int]](2, 4, 6)) should have size 3
    }

    def `when constraining nodes to have a minimum degree` {
      import UserConstraints.MinDegree_2
      implicit val config: Config = MinDegree_2

      factory(1, 2, 3 ~ 4) should be('isEmpty)

      val g = factory.empty[Int, UnDiEdge]
      given(g, 1 ~ 2) both (_ + _, _ +? _) should beRejected[Int, UnDiEdge]

      val g6 = g ++ List(1 ~ 2, 1 ~ 3, 2 ~ 3)
      g6 should have size 6
      val g7 = g6 + 3 ~> 1
      g7 should have size 7
      given(g6, 4) both (_ + _, _ +? _) should beRejected[Int, UnDiEdge]
      given(g6, 3 ~ 4) both (_ + _, _ +? _) should beRejected[Int, UnDiEdge]
      given(g6, 1 ~> 2) both (_ + _, _ +? _) should meet((_: UnDi).graphSize == 4)

      given(g6, 3) both (_ - _, _ -? _) should beRejected[Int, UnDiEdge]
      given(g6, 2 ~ 3) both (_ - _, _ -? _) should beRejected[Int, UnDiEdge]
      given(g7, 3 ~> 1) both (_ - _, _ -? _) should meet((_: UnDi).graphSize == 3)

      given(g6, List(2 ~ 3)) both (_ -- _, _ --? _) should beRejected[Int, UnDiEdge]
      given(g6, List(1, 2, 3)) both (_ -- _, _ --? _) should meet((_: UnDi).isEmpty)
      given(g7, List(3 ~> 1)) both (_ -- _, _ --? _) should meet((_: UnDi).graphSize == 3)
    }
  }

  object `constraints may` {

    def `be defined to return Left on constraint violations` {
      implicit val config: Config = UserConstraints.EvenNode

      factory[Int, Nothing](1, 2, 3, 4) should be('isEmpty)

      val g = factory[Int, Nothing](2, 4)
      g should have size 2
      shouldLeaveGraphUnchanged[Int, Nothing](g)(_ +? 5)

      g + 6 contains 6 should be(true)
      shouldLeaveGraphUnchanged[Int, Nothing](g)(_ ++? List[OuterNode[Int]](1, 2, 3))

      (g ++ List[OuterNode[Int]](2, 4, 6)) should have size 3
    }

    def `be combined` {
      import UserConstraints._
      {
        implicit val config: Config = EvenNode

        val g1 = factory[Int, Nothing](2, 4)
        g1 should have('order (2), 'graphSize (0))
      }
      {
        implicit val config: Config = EvenNode && MinDegree_2

        val g2 = factory.empty[Int, UnDiEdge]
        given(g2, 2) both (_ + _, _ +? _) should beRejected[Int, UnDiEdge]
        given(g2, List(0 ~ 2, 0 ~> 2)) both (_ ++ _, _ ++? _) should meet((_: UnDi).size == 4)
      }
    }
  }
}

private object UserConstraints {
  import PreCheckFollowUp._

  val checkAbort    = PreCheckResult(Abort)
  val postCheck     = PreCheckResult(PostCheck)
  val checkComplete = PreCheckResult(Complete)

  /* Constrains nodes to even numbers of type Int relying solely on pre-checks. */
  class EvenNode[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G) extends Constraint[N, E, G](self) {
    def preAdd(node: N) = PreCheckResult.complete(node match {
      case i: Int => i % 2 == 0
      case _      => false
    })
    def preAdd(edge: E[N]) = PreCheckResult.complete(
      edge forall { !preAdd(_).abort }
    )
    def preSubtract(node: self.NodeT, forced: Boolean) = checkComplete
    def preSubtract(edge: self.EdgeT, simple: Boolean) = checkComplete
  }

  object EvenNode extends ConstraintCompanion[EvenNode] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new EvenNode[N, E, G](self)
  }

  abstract class NoPreCheck[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends Constraint[N, E, G](self) {
    def preAdd(node: N)                                = postCheck
    def preAdd(edge: E[N])                             = postCheck
    def preSubtract(node: self.NodeT, forced: Boolean) = postCheck
    def preSubtract(edge: self.EdgeT, simple: Boolean) = postCheck
  }

  class AlwaysFailingPostSubtract[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends NoPreCheck[N, E, G](self) {

    override def postSubtract(newGraph: G,
                              passedNodes: Traversable[N],
                              passedEdges: Traversable[E[N]],
                              preCheck: PreCheckResult) = Left(PostCheckFailure(()))

  }

  object AlwaysFailingPostSubtract extends ConstraintCompanion[AlwaysFailingPostSubtract] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new AlwaysFailingPostSubtract[N, E, G](self)
  }

  class ThrowingExceptionPreAdd[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends AlwaysFailingPostSubtract[N, E, G](self: G) {
    override def preAdd(node: N): PreCheckResult = throw new NoSuchElementException
  }

  object ThrowingExceptionPreAdd extends ConstraintCompanion[ThrowingExceptionPreAdd] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new ThrowingExceptionPreAdd[N, E, G](self)
  }

  class FailingPostAdd[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends NoPreCheck[N, E, G](self) {

    override def postAdd(newGraph: G,
                         passedNodes: Traversable[N],
                         passedEdges: Traversable[E[N]],
                         preCheck: PreCheckResult) =
      if (passedEdges.size == 4) Right(newGraph) else Left(PostCheckFailure(FailingPostAdd.leftWarning))

  }

  object FailingPostAdd extends ConstraintCompanion[FailingPostAdd] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new FailingPostAdd[N, E, G](self)
    val leftWarning                                                = "warning"
  }

  /* Constrains the graph to nodes having a minimal degree of `min` by utilizing pre- and post-checks.
   */
  abstract class MinDegree[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends Constraint[N, E, G](self) {
    // the required minimal degree
    val min: Int

    // difficult to say so postpone it until post-check
    override def preCreate(nodes: collection.Traversable[N], edges: collection.Traversable[E[N]]) = postCheck
    // this would become an unconnected node with a degree of 0
    def preAdd(node: N) = checkAbort
    // edge ends not yet contained in the graph would have a degree of 1
    def preAdd(edge: E[N]) = PreCheckResult.postCheck(
      edge forall (self contains _)
    )
    // difficult to say so postpone it until post-check
    override def preAdd(elems: InParam[N, E]*) = postCheck

    // inspecting the would-be graph is much easier
    override def postAdd(newGraph: G,
                         passedNodes: Traversable[N],
                         passedEdges: Traversable[E[N]],
                         preCheck: PreCheckResult) =
      if (allNodes(passedNodes, passedEdges) forall (n => (newGraph get n).degree >= min)) Right(newGraph)
      else Left(PostCheckFailure(()))

    def preSubtract(node: self.NodeT, forced: Boolean) = PreCheckResult.complete(
      if (forced) node.neighbors forall (_.degree > min)
      else true
    )

    def preSubtract(edge: self.EdgeT, simple: Boolean) = PreCheckResult.complete(
      if (simple)
        edge.nodes forall (_.degree > min)
      else
        ((for (n <- edge) yield n.neighbors).flatten filterNot (edge contains _)) forall (_.degree > min)
    )

    /** Sub-classed `PreCheckResult` to store `neighbors` which is calculated
      * in the bulk-subtraction pre-check and to be forwarded to `postSubtract`. */
    protected class Result(followUp: PreCheckFollowUp, val nodesToCheck: Set[self.NodeT])
        extends PreCheckResult(followUp)

    object Result extends PreCheckResultCompanion {
      def apply(followUp: PreCheckFollowUp)                                = new Result(followUp, Set.empty[self.NodeT])
      def apply(followUp: PreCheckFollowUp, nodesToCheck: Set[self.NodeT]) = new Result(followUp, nodesToCheck)
      def unapply(result: Result): Option[Set[self.NodeT]]                 = Some(result.nodesToCheck)
    }

    override def preSubtract(nodes: => Set[self.NodeT], edges: => Set[self.EdgeT], simple: Boolean): PreCheckResult =
      Result(
        PostCheck, {
          val nodesToSubtract =
            if (simple) nodes
            else nodes ++ edges.flatMap(_.privateNodes)
          nodes.flatMap(_.neighbors) ++
            edges.flatMap(_.nodes) -- nodesToSubtract
        }
      )

    override def postSubtract(newGraph: G,
                              passedNodes: Traversable[N],
                              passedEdges: Traversable[E[N]],
                              preCheck: PreCheckResult) = preCheck match {
      case Result(nodesToCheck) =>
        if (nodesToCheck forall { n =>
              newGraph.get(n).degree >= min
            }) Right(newGraph)
        else Left(PostCheckFailure(()))
    }

  }

  object MinDegree_2 extends ConstraintCompanion[MinDegree] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) =
      new MinDegree[N, E, G](self) {
        val min = 2
      }
  }

}
