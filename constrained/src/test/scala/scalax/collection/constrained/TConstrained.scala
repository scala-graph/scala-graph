package scalax.collection.constrained

import scala.language.{higherKinds, postfixOps}
import scala.collection.Set
  
import org.scalatest.{Spec, Suites}
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Ignore

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import generic.GraphConstrainedCompanion

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TConstrainedRootTest
  extends Suites(
      new TConstrained[immutable.Graph](immutable.Graph),
      new TConstrained[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  import mutable.Graph

  def test_mutableEvenNode {
    implicit val config: Config = UserConstraints.EvenNode 
    val g = Graph[Int,Nothing](1)
    g should be ('isEmpty)
    (g += 2) should have size (1)
    (g += 3) should have size (1)
    (g ++= List(1,4))   should have size (1)
    (g ++= List(2,4,6)) should have size (3)
  }
  def test_mutableMinDegree {
    import UserConstraints.{MinDegree_2, MinDegreeException}
    implicit val config: Config = MinDegree_2  
    val g = Graph.empty[Int,UnDiEdge]
    evaluating { g ++= List(2,3,4)         } should produce [MinDegreeException]
    evaluating { g ++= List(1~2, 1~3, 2~4) } should produce [MinDegreeException]
    (g ++= List(1~2, 1~3, 2~3)) should have size (6)
  }
}

class TConstrained [CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphConstrainedCompanion[CC])
  extends Spec
  with    ShouldMatchers
{
  def test_0(info : Informer) {
    info("factory = " + factory.getClass)
  }
  def test_EvenNode {
    implicit val config: Config = UserConstraints.EvenNode  
    val g = factory[Int,Nothing](1,2,3,4)
    g should be ('isEmpty)
    g + 5 contains 5 should be (false)
    g + 6 contains 6 should be (true)
    (g ++ List[OuterNode[Int]](1,2,3)) should be ('isEmpty)
    (g ++ List[OuterNode[Int]](2,4,6)) should have size (3)
  }
  def test_EvenNodeByException {
    implicit val config: Config = UserConstraints.EvenNodeByException  
    intercept[IllegalArgumentException] {
            factory[Int,Nothing](1,2,3,4)
    }
    val g = factory[Int,Nothing](2,4)
    g should have size (2)
    intercept[IllegalArgumentException] { g + 5 }
    g + 6 contains 6 should be (true)
    intercept[IllegalArgumentException] {
      g ++ List[OuterNode[Int]](1,2,3)
    }
    ( g ++ List[OuterNode[Int]](2,4,6)) should have size (3)
  }
  def test_MinDegree {
    import UserConstraints.{MinDegree_2, MinDegreeException}
    implicit val config: Config = MinDegree_2  
    evaluating {
      factory(1, 2, 3~4) } should produce [MinDegreeException]
    val g = factory.empty[Int,UnDiEdge]
    evaluating { g + 1~2 } should produce [MinDegreeException]

    val g6 = g ++ List(1~2, 1~3, 2~3)
    g6 should have size (6)
    val g7 = g6 + 3~>1
    g7 should have size (7)
    evaluating { g6 +   4 } should produce [MinDegreeException]
    evaluating { g6 + 3~4 } should produce [MinDegreeException]
    g6 + 1~>2 should have ('graphSize (4))

    evaluating { g6 - 3   } should produce [MinDegreeException]
    evaluating { g6 - 2~3 } should produce [MinDegreeException]
    g7 - 3~>1 should have ('graphSize (3))

    evaluating { g6 -- List(2~3) } should produce [MinDegreeException]
    (g6 -- List(1, 2, 3)) should be ('empty)
    (g7 -- List(3~>1))    should have ('graphSize (3))
  }
  def test_Op {
    import UserConstraints._
    { implicit val config: Config = EvenNode && EvenNode  
      val g1 = factory[Int,Nothing](2,4)
      g1 should have ('order (2), 'graphSize (0))
    }
    { implicit val config: Config = EvenNode && MinDegree_2  
      val g2 = factory.empty[Int,UnDiEdge]
      evaluating { g2 + 2 } should produce [MinDegreeException]
      g2 ++ List(0~2, 0~>2) should have size (4)
    }
  }
}
object UserConstraints {
  import PreCheckFollowUp._

  val checkAbort    = PreCheckResult(Abort)
  val postCheck     = PreCheckResult(PostCheck)
  val checkComplete = PreCheckResult(Complete)
  /* Constrains nodes to even numbers of type Int relying solely on pre-checks. */
  class EvenNode[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends Constraint[N,E] (self)
    with    ConstraintHandlerMethods[N,E]
  {
    def preAdd(node: N) = PreCheckResult.complete(
      node match { case i:Int => i % 2 == 0
                   case _     => false
      })
    def preAdd(edge: E[N]) = PreCheckResult.complete(
        edge forall { preAdd(_).noAbort }
    )
    def preSubtract(node: self.NodeT, forced: Boolean) = checkComplete
    def preSubtract(edge: self.EdgeT, simple: Boolean) = checkComplete
  }
  object EvenNode extends ConstraintCompanion[EvenNode] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new EvenNode[N,E] (self) 
  }

  /* Same as EvenNode but throws an exception on constraint violation. */
  class EvenNodeByException[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends EvenNode[N,E](self)
  {
    override def onAdditionRefused (refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) = {
      throw new IllegalArgumentException("Non-integer or uneven node found.")
    }
  }
  object EvenNodeByException extends ConstraintCompanion[EvenNode] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new EvenNodeByException[N,E] (self) 
  }

  /* Constrains the graph to nodes having a minimal degree of `min` 
   * utilizing pre- and post-checks. */
  abstract class MinDegree[N, E[X] <: EdgeLikeIn[X]] (override val self: Graph[N,E])
    extends Constraint[N,E] (self)
    with    ConstraintHandlerMethods[N,E]
  {
    // the required minimal degree
    val min: Int

    // difficult to say so postpone it until post-check
    override def preCreate(nodes: collection.Iterable[N],
                           edges: collection.Iterable[E[N]]) = postCheck
    // this would become an unconnected node with a degree of 0
    def preAdd(node: N) = checkAbort
    // edge ends not yet contained in the graph would have a degree of 1
    def preAdd(edge: E[N]) = PreCheckResult.postCheck(
      edge forall (self contains _)
    )
    // difficult to say so postpone it until post-check
    override def preAdd(elems: InParam[N,E]*) = postCheck
    // inspecting the would-be graph is much easier
    override def postAdd (newGraph: Graph[N,E],
                          passedNodes: Iterable[N],
                          passedEdges: Iterable[E[N]],
                          preCheck   : PreCheckResult) = 
    { allNodes(passedNodes, passedEdges) forall (
        n => (newGraph get n).degree >= min)
    }
    override def onAdditionRefused( refusedNodes: Iterable[N],
                                    refusedEdges: Iterable[E[N]],
                                    graph:        Graph[N,E]) =
    { throw new MinDegreeException("Addition refused: " +
                "nodes = " + refusedNodes + ", " +
                "edges = " + refusedEdges)
    }
    def preSubtract(node: self.NodeT, forced: Boolean) = PreCheckResult.complete(
      if (forced) node.neighbors forall (_.degree > min)
      else true
    )
    def preSubtract(edge: self.EdgeT, simple: Boolean) = PreCheckResult.complete(
      if (simple)
        edge.nodes forall (_.degree > min)
      else
        ((for (n <- edge) yield n.neighbors).flatten filterNot (edge contains _)
          ) forall (_.degree > min)
    )

    /** Sub-classed `PreCheckResult` to store `neighbors` which is calculated
     * in the bulk-subtraction pre-check and to be forwarded to `postSubtract`. */
    protected class Result(followUp:         PreCheckFollowUp,
                           val nodesToCheck: Set[self.NodeT])
      extends PreCheckResult(followUp)
    object Result extends PreCheckResultCompanion {
      def apply(followUp: PreCheckFollowUp) = new Result(followUp, Set.empty[self.NodeT])
      def apply(followUp:     PreCheckFollowUp,
                nodesToCheck: Set[self.NodeT] ) = new Result(followUp, nodesToCheck)
      def unapply(result: Result): Option[Set[self.NodeT]] = Some(result.nodesToCheck)
    }
    override def preSubtract (nodes: => Set[self.NodeT],
                              edges: => Set[self.EdgeT],
                              simple:   Boolean): PreCheckResult = Result(
      PostCheck,
      { val nodesToSubtract = 
          if (simple) nodes
          else        nodes ++ (edges.map(_.privateNodes).flatten)
        nodes.map(_.neighbors).flatten ++
        edges.map(_.nodes)    .flatten -- nodesToSubtract
      }
    )
    override def postSubtract(newGraph: Graph[N,E],
                              passedNodes: Iterable[N],
                              passedEdges: Iterable[E[N]],
                              preCheck   : PreCheckResult) = preCheck match {
      case Result(nodesToCheck) => nodesToCheck forall {
        n => newGraph.get(n).degree >= min
      }
    }
    override def onSubtractionRefused(refusedNodes: Iterable[Graph[N,E]#NodeT],
                                      refusedEdges: Iterable[Graph[N,E]#EdgeT],
                                      graph:        Graph[N,E]) =
    { throw new MinDegreeException("Subtraction refused: " +
                "nodes = " + refusedNodes + ", " +
                "edges = " + refusedEdges)
    }
  }
  object MinDegree_2 extends ConstraintCompanion[MinDegree] {
    def apply [N, E[X] <: EdgeLikeIn[X]] (self: Graph[N,E]) =
      new MinDegree[N,E] (self) {
        val min = 2
      }
  }
  class MinDegreeException(msg: String) extends IllegalArgumentException(msg)
}