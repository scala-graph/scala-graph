package scalax.collection

import language.{higherKinds, postfixOps}
import collection.{SortedMap, SortedSet}
import collection.mutable.{Map => MutableMap}

import GraphPredef.EdgeLikeIn

/** A trait for graph degree calculations.
  *
  * @tparam N the user type of the nodes (vertices) in this graph.
  * @tparam E the kind of the edges (links) in this graph.
  *
  * @author Peter Empen
  * @define DEGREEFUNCTION the degree function to apply
  *         to the nodes defaulting to `Degree`. Non-default predefined
  *         degree functions are `InDegree` and `OutDegree`.
  * @define DEGREEFILTER selects nodes to be included by their degree.
  */
trait GraphDegree[N, E[+X] <: EdgeLikeIn[X]] { this: GraphBase[N, E] =>

  /** Decreasing ordering of nodes with respect to their degree.
    */
  final class DegreeOrdering(val f: DegreeFunction) extends Ordering[NodeT] {
    def compare(n1: NodeT, n2: NodeT) = n1.degree compare n2.degree
  }
  object DegreeOrdering {
    @inline final def apply(f: DegreeFunction) = new DegreeOrdering(f)
  }

  /** Decreasing ordering of integers.
    */
  object IntReverseOrdering extends Ordering[Int] {
    def compare(d1: Int, d2: Int) = d2 compare d1
  }

  trait DegreeFunction extends Function1[NodeT, Int]
  object Degree        extends DegreeFunction { def apply(n: NodeT) = n.degree }
  object InDegree      extends DegreeFunction { def apply(n: NodeT) = n.inDegree }
  object OutDegree     extends DegreeFunction { def apply(n: NodeT) = n.outDegree }

  trait Filter[T] extends Function1[T, Boolean]
//  trait NodeFilter extends Filter[NodeT]
//  implicit object AnyNode extends NodeFilter { def apply = (n: NodeT) => true }
  /** The total degree of this graph equaling to the sum
    * of the degrees over all nodes or `0` if this graph is empty.
    *
    * @param nodeDegree $DEGREEFUNCTION
    * @param degreeFilter $DEGREEFILTER
    */
  def totalDegree(implicit nodeDegree: DegreeFunction = Degree, degreeFilter: Int => Boolean = AnyDegree) =
    if (edges.maxArity <= 2 && degreeFilter == AnyDegree) edges.size * 2
    else {
      var deg = 0
      nodes foreach { n =>
        val nodeD = nodeDegree(n)
        if (degreeFilter(nodeD)) deg += nodeD
      }
      deg
    }

  /** The degree of the node having the least degree or `0` if
    * this graph is empty.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def minDegree(implicit nodeDegree: DegreeFunction = Degree, degreeFilter: Int => Boolean = AnyDegree): Int =
    if (order == 0) 0
    else if (degreeFilter == AnyDegree)
      nodeDegree(nodes min DegreeOrdering(nodeDegree))
    else
      nodes.toList.view map nodeDegree filter degreeFilter min

  /** The degree of the node having the highest degree or `0` if
    * this graph is empty.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def maxDegree(implicit nodeDegree: DegreeFunction = Degree, degreeFilter: Int => Boolean = AnyDegree): Int =
    if (order == 0) 0
    else if (degreeFilter == AnyDegree)
      nodeDegree(nodes max DegreeOrdering(nodeDegree))
    else
      nodes.toList.view map nodeDegree filter degreeFilter max

  /** The degree sequence of this graph, that is the non-increasing
    * sequence of degrees over all nodes.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def degreeSeq(implicit nodeDegree: DegreeFunction = Degree, degreeFilter: Int => Boolean = AnyDegree): Seq[Int] = {
    val v = nodes.toList.view map nodeDegree sorted IntReverseOrdering
    if (degreeFilter == AnyDegree) v.toSeq
    else (v filter degreeFilter).toSeq
  }

  /** The degree set of this graph, that is the decreasing
    * set of unique degrees over all nodes. Same as degreeSeq without duplicates.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def degreeSet(implicit nodeDegree: DegreeFunction = Degree,
                degreeFilter: Int => Boolean = AnyDegree): SortedSet[Int] =
    SortedSet[Int]()(IntReverseOrdering) ++ (if (degreeFilter == AnyDegree) nodes map nodeDegree
                                             else nodes.view map nodeDegree filter degreeFilter)

  /** Type alias for entries in degree maps returned by `degreeSeqMap`.
    */
  type DegreeNodeSeqEntry = (Int, NodeT)

  /** The degree sequence of this graph projected onto a sequence of tuples.
    * The first elements of the tuples are the degrees in non-increasing order
    * while the second elements are the corresponding inner nodes.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def degreeNodeSeq(implicit nodeDegree: DegreeFunction = Degree,
                    degreeFilter: Int => Boolean = AnyDegree): Seq[DegreeNodeSeqEntry] = {
    val r = (nodes.toList map (n => (nodeDegree(n), n))) sorted
      (new Ordering[DegreeNodeSeqEntry] {
        def compare(a: DegreeNodeSeqEntry, b: DegreeNodeSeqEntry) = b._1 compare a._1
      })
    if (degreeFilter == AnyDegree) r
    else r filter (t => degreeFilter(t._1))
  }

  /** The degree set of this graph projected onto a map.
    * The keys of the map are the degrees in decreasing order
    * while the values are sets of the corresponding inner nodes.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def degreeNodesMap(implicit nodeDegree: DegreeFunction = Degree,
                     degreeFilter: Int => Boolean = AnyDegree): SortedMap[Int, AnySet[NodeT]] = {
    val r: SortedMap[Int, AnySet[NodeT]] =
      SortedMap[Int, AnySet[NodeT]]()(IntReverseOrdering) ++ (nodes groupBy nodeDegree)
    if (degreeFilter == AnyDegree) r
    else r filter (t => degreeFilter(t._1))
  }

  /** The degree set of this graph projected onto a map.
    * The keys of the map are the degrees in decreasing order
    * while the values are the number of inner nodes having
    * the degree of the corresponding key.
    *
    * @param $DEGREEFUNCTION
    * @param $DEGREEFILTER
    */
  def degreeCount(implicit nodeDegree: DegreeFunction = Degree,
                  degreeFilter: Int => Boolean = AnyDegree): SortedMap[Int, Int] =
    SortedMap[Int, Int]()(IntReverseOrdering) ++ {
      val m = MutableMap[Int, Int]()
      nodes foreach { n =>
        val degree = nodeDegree(n)
        if (degreeFilter(degree))
          m += degree -> (m.getOrElse(degree, 0) + 1)
      }
      m
    }
}
