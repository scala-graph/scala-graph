package scalax.collection.constrained

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.Set
import scala.language.{higherKinds, postfixOps}
import scala.reflect.ClassTag
import scala.util.Try

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import generic.GraphConstrainedCompanion
import org.scalatest._
import org.scalatest.refspec.RefSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
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
      val g                       = factory[Int, Nothing](1)

      g should be('isEmpty)
      (g += 2) should have size 1
      (g += 3) should have size 1
      (g ++= List(1, 4)) should have size 1
      (g ++= List(2, 4, 6)) should have size 3
    }

    def `when constraining nodes to have a minimum degree` {
      import UserConstraints.{MinDegreeException, MinDegree_2}

      implicit val config: Config                        = MinDegree_2
      implicit val expectedException: MinDegreeException = new MinDegreeException

      val g = factory.empty[Int, UnDiEdge]

      shouldLeaveGraphUnchanged(g)(_ ++=? List(2, 3, 4))
      shouldLeaveGraphUnchanged(g)(_ ++=? List(1 ~ 2, 1 ~ 3, 2 ~ 4))
      (g ++= List(1 ~ 2, 1 ~ 3, 2 ~ 3)) should have size 6

      shouldLeaveGraphUnchanged(g)(_ +=? 3 ~ 4)
      shouldLeaveGraphUnchanged(g)(_ -=? 3)
      shouldLeaveGraphUnchanged(g)(_ --=? List(3))
    }

    def `when postSubtract fails` {
      implicit val config: Config                              = UserConstraints.AlwaysFailingPostSubtract
      implicit val expectedException: IllegalArgumentException = new IllegalArgumentException

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1)

      shouldLeaveGraphUnchanged(g)(_ -=? 1)
      shouldLeaveGraphUnchanged(g)(_ -=? 1 ~ 2)
      shouldLeaveGraphUnchanged(g)(_ --=? List(1))
      shouldLeaveGraphUnchanged(g)(_ --=? List(1 ~ 2))
      shouldLeaveGraphUnchanged(g)(_ --=? List(1 ~ 2, 2 ~ 3))
    }

    def `when postAdd fails` {
      implicit val config: Config                              = UserConstraints.FailingPostAdd
      implicit val expectedException: IllegalArgumentException = new IllegalArgumentException

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 1)

      shouldLeaveGraphUnchanged(g)(_ +=? 5)
      shouldLeaveGraphUnchanged(g)(_ +=? 1 ~ 5)
      shouldLeaveGraphUnchanged(g)(_ ++=? List(5))
      shouldLeaveGraphUnchanged(g)(_ ++=? List(1 ~ 5))
      shouldLeaveGraphUnchanged(g)(_ ++=? List(1 ~ 5, 5 ~ 6, 6 ~ 2))
    }

    def `when cloning a graph` {
      implicit val config: Config = UserConstraints.AlwaysFailingPreAdd

      val g = factory[Int, UnDiEdge](1 ~ 2, 2 ~ 3)
      Try(g.clone).isSuccess should be(true)
    }
  }
}

class TConstrained[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphConstrainedCompanion[CC])
    extends RefSpec
    with Matchers {

  info("factory = " + factory.getClass)

  object `constrains take effect` {

    def `when constraining Int nodes to even numbers` {
      implicit val config: Config = UserConstraints.EvenNode
      val g                       = factory[Int, Nothing](1, 2, 3, 4)
      g should be('isEmpty)
      g + 5 contains 5 should be(false)
      g + 6 contains 6 should be(true)
      (g ++ List[OuterNode[Int]](1, 2, 3)) should be('isEmpty)
      (g ++ List[OuterNode[Int]](2, 4, 6)) should have size 3
    }

    def `when constraining nodes to have a minimum degree` {
      import UserConstraints.{MinDegreeException, MinDegree_2}
      implicit val config: Config = MinDegree_2

      //@todo how to test factory creation
//      a[MinDegreeException] should be thrownBy { factory(1, 2, 3 ~ 4) }
      val g = factory.empty[Int, UnDiEdge]
      (g +? 1 ~ 2) should be('left)
//      a[MinDegreeException] should be thrownBy { g + 1 ~ 2 }

      val g6 = g ++ List(1 ~ 2, 1 ~ 3, 2 ~ 3)
      g6 should have size 6
      val g7 = g6 + 3 ~> 1
      g7 should have size 7
      (g6 +? 4) should be('left)
      (g6 +? 3 ~ 4) should be('left)
//      a[MinDegreeException] should be thrownBy { g6 + 4 }
//      a[MinDegreeException] should be thrownBy { g6 + 3 ~ 4 }
      g6 + 1 ~> 2 should have('graphSize (4))

      //@todo an implementation is missing, cannot proceed
      a[MinDegreeException] should be thrownBy { g6 - 3 }
      a[MinDegreeException] should be thrownBy { g6 - 2 ~ 3 }
      g7 - 3 ~> 1 should have('graphSize (3))

//      a[MinDegreeException] should be thrownBy { g6 -- List(2 ~ 3) }
      (g6 -- List(1, 2, 3)) should be('empty)
      (g7 -- List(3 ~> 1)) should have('graphSize (3))
    }
  }

  object `constraints may` {

    def `be defined to throw exceptions on constraint violations` {
      implicit val config: Config = UserConstraints.EvenNodeByException
      //@todo how to test factory creation
//      an[IllegalArgumentException] should be thrownBy { factory[Int, Nothing](1, 2, 3, 4) }

      val g = factory[Int, Nothing](2, 4)
      g should have size 2
      (g +? 5) should be('left)
//      an[IllegalArgumentException] should be thrownBy { g + 5 }

      g + 6 contains 6 should be(true)
      (g ++? List[OuterNode[Int]](1, 2, 3)) should be('left)
//      an[IllegalArgumentException] should be thrownBy { g ++ List[OuterNode[Int]](1, 2, 3) }

      (g ++ List[OuterNode[Int]](2, 4, 6)) should have size 3
    }

    def `be combined` {
      import UserConstraints._
      {
        implicit val config: Config = EvenNode && EvenNode
        val g1                      = factory[Int, Nothing](2, 4)
        g1 should have('order (2), 'graphSize (0))
      }
      {
        implicit val config: Config = EvenNode && MinDegree_2
        val g2                      = factory.empty[Int, UnDiEdge]
        (g2 +? 2) should be('left)
//        a[MinDegreeException] should be thrownBy { g2 + 2 }
        g2 ++ List(0 ~ 2, 0 ~> 2) should have size 4
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
  class EvenNode[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends Constraint[N, E, G](self) {
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

  /* Same as EvenNode but throws an exception on constraint violation. */
  class EvenNodeByException[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends EvenNode[N, E, G](self) {
//    override def onAdditionRefused(refusedNodes: Traversable[N], refusedEdges: Traversable[E[N]], graph: G @uV) =
//      throw new IllegalArgumentException("Non-integer or uneven node found.")
  }

  object EvenNodeByException extends ConstraintCompanion[EvenNode] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new EvenNodeByException[N, E, G](self)
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
                                                preCheck: PreCheckResult) = Left(())

//    override def onSubtractionRefused(refusedNodes: Traversable[G#NodeT],
//                                      refusedEdges: Traversable[G#EdgeT],
//                                      graph: G) =
//      throw new IllegalArgumentException
  }

  object AlwaysFailingPostSubtract extends ConstraintCompanion[AlwaysFailingPostSubtract] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new AlwaysFailingPostSubtract[N, E, G](self)
  }

  class AlwaysFailingPreAdd[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends AlwaysFailingPostSubtract[N, E, G](self: G) {
    override def preAdd(node: N): PreCheckResult = throw new NoSuchElementException
  }

  object AlwaysFailingPreAdd extends ConstraintCompanion[AlwaysFailingPreAdd] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new AlwaysFailingPreAdd[N, E, G](self)
  }

  class FailingPostAdd[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](override val self: G)
      extends NoPreCheck[N, E, G](self) {

    override def postAdd(newGraph: G,
                                           passedNodes: Traversable[N],
                                           passedEdges: Traversable[E[N]],
                                           preCheck: PreCheckResult) =
      if (passedEdges.size == 4) Right(newGraph) else Left(())

//    override def onAdditionRefused(refusedNodes: Traversable[N], refusedEdges: Traversable[E[N]], graph: G) =
//      throw new IllegalArgumentException
  }

  object FailingPostAdd extends ConstraintCompanion[FailingPostAdd] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) = new FailingPostAdd[N, E, G](self)
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
      else Left(())

//    override def onAdditionRefused(refusedNodes: Traversable[N], refusedEdges: Traversable[E[N]], graph: G) =
//      throw new MinDegreeException(
//        "Addition refused: " +
//          "nodes = " + refusedNodes + ", " +
//          "edges = " + refusedEdges)

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
            else nodes ++ (edges.map(_.privateNodes).flatten)
          nodes.map(_.neighbors).flatten ++
            edges.map(_.nodes).flatten -- nodesToSubtract
        }
      )

    override def postSubtract(newGraph: G,
                                                passedNodes: Traversable[N],
                                                passedEdges: Traversable[E[N]],
                                                preCheck: PreCheckResult) = preCheck match {
      case Result(nodesToCheck) =>
        if (nodesToCheck forall { n =>
          newGraph.get(n).degree >= min
        }) Right(newGraph) else Left(())
    }

//    override def onSubtractionRefused(refusedNodes: Traversable[G#NodeT],
//                                      refusedEdges: Traversable[G#EdgeT],
//                                      graph: G) =
//      throw new MinDegreeException(
//        "Subtraction refused: " +
//          "nodes = " + refusedNodes + ", " +
//          "edges = " + refusedEdges)
  }

  object MinDegree_2 extends ConstraintCompanion[MinDegree] {
    def apply[N, E[X] <: EdgeLikeIn[X], G <: Graph[N, E]](self: G) =
      new MinDegree[N, E, G](self) {
        val min = 2
      }
  }

  class MinDegreeException(msg: String = "") extends IllegalArgumentException(msg)
}
