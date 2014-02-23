package scalax.collection

import language.{higherKinds, postfixOps}
import collection.Set
import collection.immutable.{Range, SortedSet}
import collection.mutable.{Set => MutableSet}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import edge._, edge.WBase._, edge.LBase._, edge.WLBase._
import io._

import org.scalatest.Suite
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TStreamRootTest
  extends Suites(
      new TStream[immutable.Graph](immutable.Graph),
      new TStream[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  // ---------------------------------------- mutable tests
}
/**	This class contains tests for implementing node/edge input streams
 *  and using them for graph creation.
 */
class TStream[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC])
	extends	Suite
	with	ShouldMatchers
{
  /** Emulates filling a graph with edges coming from a single `edgeStream`.
   *  @return Set of outer edges
   */ 
  protected def fill[N,
                     E[N] <: EdgeLike[N],
                     L,
                     C[  EE[N]<:E[N]]       <: EdgeCompanionBase[EE],
                     A[N,EE[N]<:E[N],AL<:L] <: EdgeAdapterBase[N,EE,AL]]
     (edgeStream: EdgeInputStream[N,E,L,C,A]) =
  {
    val edges = MutableSet.empty[E[N]]
    edgeStream.factory match {
      case wlF: WLEdgeCompanion[E] =>
        for (a <- edgeStream) a match { case a: WLEdgeAdapter[N,E,L] =>
          edges += wlF.from[N,L](a.nodes)(a.weight, a.label) }
      case lF: LEdgeCompanion[E] =>
        for (a <- edgeStream) a match { case a: LEdgeAdapter[N,E,L] =>
          edges += lF.from[N,L](a.nodes)(a.label) }
      case wF: WEdgeCompanion[E] =>
        for (a <- edgeStream) a match { case a: WEdgeAdapter[N,E,L] =>
          edges += wF.from[N](a.nodes)(a.weight) }
    }
    //println(edges)
    edges
  }
  def test_nodeStream {
    // the following value represents raw data to flow into the stream
    val nodesData = Set[Long](7,77,777,888,999)

    class MyNodeStream extends NodeInputStream[Long] {
      val it = nodesData.iterator
      def hasNext = it.hasNext
      def next = it.next
    }
    def check(nodes: Set[Long]) {
      nodes should have size (nodesData.size)
      nodesData forall {nodes contains _}
    }
    check {val g = factory.fromStream[Long,Nothing](
                           nodeStreams = Seq(new MyNodeStream),
                           edgeStreams = Seq.empty[GenEdgeInputStream[Long,Nothing]])
           g.nodes map (_.value)
          }
  }
  def test_WLEdgeStream {
    // the following values represent raw data to flow into the stream
    val weights = (0 to 6) 
    val labels  = weights map (i => ('A' + i).toChar.toString) 
    val sourceNodes = weights map (_ + 10)
    val targetNodes = sourceNodes reverse

    class WLDiEdgeStream (override val factory: WLEdgeCompanion[WLDiEdge])
      extends EdgeInputStream[Int,WLDiEdge,String,WLEdgeCompanion,WLEdgeAdapter](factory)
      with    WLEdgeAdapter  [Int,WLDiEdge,String]
    {
      /** emulates a cursor - similar to an iterator - moving in a data set */
      class Cursor (var source: Seq[Int],
                    var target: Seq[Int],
                    var weight: Range,
                    var label:  Seq[String])
      {
        var first = true // flag allows to avoid saving results
        def hasNext = if (first) weight.nonEmpty
                      else       weight.tail.nonEmpty
        def read {
          if (first) first = false
          else {
            source = source.tail
            target = target.tail
            weight = weight.tail
            label  = label .tail
          }
        }
      }
      val cursor = new Cursor(sourceNodes, targetNodes, weights, labels)
      override def nodes   = Tuple2(cursor.source.head, cursor.target.head) 
      override def weight  = cursor.weight.head
      override def label   = cursor.label .head
      override def hasNext = cursor.hasNext
      override def next    = { cursor.read; this }
    }
    check {fill[Int,WLDiEdge,String,WLEdgeCompanion,WLEdgeAdapter](
                new WLDiEdgeStream(WLDiEdge))
          }
    check {val g = factory.fromStream[Int,WLDiEdge](
                                      edgeStreams = Seq(new WLDiEdgeStream(WLDiEdge)))
           g.edges map (_.toEdgeIn)
          }
    def check(edges: Set[WLDiEdge[Int]]) {
      edges should have size (weights.size)
      edges.map(_.weight).sum should be (weights.sum)
      SortedSet(edges.map(_.label.asInstanceOf[String]).toSeq: _*).
        sameElements(labels) should be (true)
      sourceNodes forall {s => edges.count(_._1 == s) == 1} should be (true)
      targetNodes forall {s => edges.count(_._2 == s) == 1} should be (true)
      edges.count(e => e._1 == e._2) should be (edges.size % 2)
    }
  }
  def test_MyWEdgeStream {
    // the following values represent raw data to flow into the stream
    val weights = (0 to 6) 
    val sourceNodes = weights map (i => ('A' + i).toChar.toString)
    val targetNodes = sourceNodes reverse
    
    // a sample stream implementation
    class MyWEdgeStream (override val factory: WEdgeCompanion[MyWEdge])
      extends EdgeInputStream[String,MyWEdge,Nothing,WEdgeCompanion,WEdgeAdapter](factory)
      with    WEdgeAdapter   [String,MyWEdge,Nothing]
    {
      /** emulates a cursor - similar to an iterator - moving in a data set */
      class Cursor (var source: Seq[String],
                    var target: Seq[String],
                    var weight: Range)
      {
        var first = true // flag allows to avoid saving results
        def hasNext = if (first) weight.nonEmpty
                      else       weight.tail.nonEmpty
        def read {
          if (first) first = false
          else {
            source = source.tail
            target = target.tail
            weight = weight.tail
          }
        }
      }
      val cursor = new Cursor(sourceNodes, targetNodes, weights)
    
      override def nodes   = Tuple2(cursor.source.head, cursor.target.head) 
      override def weight  = cursor.weight.head
      override def hasNext = cursor.hasNext
      override def next    = { cursor.read; this }
    }
    check {fill[String,MyWEdge,Nothing,WEdgeCompanion,WEdgeAdapter](
                new MyWEdgeStream(MyWEdge))
          }
    check {val g = factory.fromStream[String,MyWEdge](
                           edgeStreams = Seq(new MyWEdgeStream(MyWEdge)))
           g.edges map (_.toEdgeIn)
          }
    def check(edges: Set[MyWEdge[String]]) {
      edges should have size (weights.size)
      edges.map(_.weight).sum should be (weights.sum)
      sourceNodes forall {s => edges.count(_._1 == s) == 1} should be (true)
      targetNodes forall {s => edges.count(_._2 == s) == 1} should be (true)
      edges.count(e => e._1 == e._2) should be (edges.size % 2)
    }
  }
  def test_nodeEdgeStream {
  }
}
/** a sample custom edge */
class MyWEdge[N] (nodes: Product, weight: Long)
  extends WDiEdge[N](nodes, weight)
  with    EdgeCopy[MyWEdge]
  with    OuterEdge[N,MyWEdge]
  with    WEdge[N]
  with    Serializable
{
  override protected[collection] def copy[NN](newNodes: Product) =
    new MyWEdge[NN](newNodes, weight)
}
object MyWEdge extends WEdgeCompanion[MyWEdge] {
  @SerialVersionUID(97585564L) override
  def newEdge[N](nodes: Product, weight: Long) =
    new  MyWEdge[N](nodes, weight)
    with EdgeCopy [MyWEdge] { 
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
}
