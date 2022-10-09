package scalax.collection
package generator

import scala.collection.mutable.{ArrayBuffer, Set => MSet}
import scala.util.Random
import scala.reflect.ClassTag

import scalax.collection.edges._
import scalax.collection.generic._
/* TODO L
import edge.WBase.{WEdgeCompanion, WHyperEdgeCompanion}
import edge.LBase.{LEdgeCompanion, LHyperEdgeCompanion}
import edge.WLBase.{WLEdgeCompanion, WLHyperEdgeCompanion}
 */

/** Supports random graph creation for graphs of any type with variable metrics.
  *
  * @tparam N Type of the nodes the generated will contain.
  * @tparam E Kind of type of the edges the generated will contain.
  * @tparam G Kind of type of the graph to be generated.
  * @param graphCompanion The graph companion the factory method `from`of which is
  *        to be called to create random graphs.
  * @param order The total number of nodes the generated random graph should contain.
  * @param nodeFactory The function responsible for generating nodes.
  * @param nodeDegree The characteristics of node degrees such as the span of degrees
  *        for the graph to be generated.
  * @param edgeCompanions The edge types to be used in the generated graph.
  * @param connected Whether the generated graph should be connected.
  *
  * @author Peter Empen
  */
class RandomGraph[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
    val graphCompanion: GenericGraphFactory[G],
    val order: Int,
    nodeFactory: => N,
    nodeDegree: NodeDegreeRange,
    edgeCompanions: Set[EdgeCompanionBase],
    connected: Boolean,
    weightFactory: Option[() => Long] = None,
    labelFactory: Option[() => Any] = None
)(implicit nodeTag: ClassTag[N]) {
  require(order > 0)
  if (connected) require(nodeDegree.min >= 2)

  implicit val graphConfig = graphCompanion.defaultConfig

  protected val doTrace                       = false
  protected def trace(str: => String): Unit   = if (doTrace) print(str)
  protected def traceln(str: => String): Unit = if (doTrace) println(str)

  final protected[RandomGraph] class DefaultWeightFactory {
    private[this] var weightCount = 0L
    def apply: () => Long = { () =>
      weightCount += 1
      weightCount
    }
    def reset(): Unit = weightCount = 0
  }

  /** A stateful String generator with deterministic results.
    */
  final protected[RandomGraph] class DefaultLabelFactory(startChar: Char = 'A', endChar: Char = 'Z') {
    private[this] var labelBuffer = Array[Char]((startChar.toInt - 1).toChar)

    /** Given `startChar = 'A'`, consecutive calls return the character combinations
      *  `A, B, ..., AA, AB, ..., BA, BB, ...`.
      */
    def apply: () => Any = { () =>
      val len = labelBuffer.length
      def loop(i: Int): Array[Char] = {
        val c = labelBuffer(i)
        if (c == endChar) {
          if (i > 0) {
            labelBuffer(i) = startChar
            loop(i - 1)
          } else
            Array.fill[Char](len + 1)(startChar)
        } else {
          labelBuffer(i) = (c.toInt + 1).toChar
          labelBuffer
        }
      }
      labelBuffer = loop(len - 1)
      labelBuffer.mkString
    }
  }

  private def addExact[A](nrToAdd: Int, add: => Boolean, infiniteMsg: String, gentle: Boolean): Boolean = {
    val checkInfiniteAt = math.pow(math.log(nrToAdd), 2).ceil.toInt + 10
    val infiniteFactor  = 5

    var added, lastAdded = 0
    var trials           = 0
    while (added < nrToAdd) {
      if (add) added += 1

      trials += 1
      if (trials == checkInfiniteAt) {
        if ((added - lastAdded) * infiniteFactor < trials) {
          traceln(s"gentle=$gentle trials=$trials, lastAdded=$lastAdded, added=$added")

          if (gentle) return false
          else throw new IllegalArgumentException(infiniteMsg)
        }
        trials = 0
        lastAdded = added
      }
    }
    true
  }

  final protected[RandomGraph] class Degrees {
    /* Unique nodes inserted by `add`. */
    private[this] val nodes = new Array[N](order)

    /* Counts the degree to be reached for each node matched by the array index.
     * The initial value is inserted by `add`. On each edge creation the counts of the
     * nodes incident with the new edge are decremented. */
    private[this] val degrees = new Array[Int](order)

    /* Count of nodes with a positive degree count. */
    private[this] var active = 0

    /* When looking for a node to be incident with a new edge, this array is searched
     * for a positive element starting by a random index.
     * To avoid lengthy searches this array, initially equaling to `degrees`, will be replaced
     * by a new array as soon as it gets sparse.
     * The replacement array contains indexes each referencing the position of a positive
     * element in `degrees` plus 1. */
    private[this] var compact = degrees
    /* Array of compacted degree reference arrays. */
    private[this] val compacts = new Array[Array[Int]](8)
    /* The currently active degrees array in `compacts` or -1 if there is no compacted one. */
    private[this] var activeCompact = -1
    /* Whether a compaction has already taken place. */
    private[this] var isCompact = false

    def sliding2(f: (N, N) => Unit): Unit = {
      nodes sliding 2 foreach { a: Array[N] =>
        f(a(0), a(1))
      }
      f(nodes(order - 1), nodes(0))
    }

    def mayFinish: Boolean = active.toFloat / order < 0.5

    private[this] var idx = 0
    def add(node: N, degree: Int): Unit = {
      nodes(idx) = node
      degrees(idx) = degree
      idx += 1
      if (degree > 0) active += 1
    }

    /** A random node with back references to the degree arrays.
      *
      * @param index Index of `degrees` if `compactIndex < 0`;
      *        otherwise index of `compacts(compactIndex)`.
      * @param compactIndex Index of `compacts`or -1.
      */
    final class Drawn(val node: N, index: Int, compactIndex: Int) {

      def isDefined = index != Drawn.emptyIdx

      def setUsed: Unit = {
        val drawnCompact = compactIndex >= 0
        val degreeIdx =
          if (drawnCompact) compacts(compactIndex)(index) - 1
          else index
        if (degreeIdx >= 0) {
          val newDegree = degrees(degreeIdx) - 1
          degrees(degreeIdx) = newDegree
          if (newDegree == 0) {
            active -= 1
            if (drawnCompact) compacts(compactIndex)(index) = 0

            val len = compact.length
            if (len > 500 && active < len / 25) {
              traceln(s"New compact at len=$len, active=$active")

              val newCompact = new Array[Int](active)
              var i, j       = 0
              while (i < len) {
                val c = compact(i)
                if (c > 0 && (!isCompact || degrees(c - 1) > 0)) {
                  newCompact(j) = if (isCompact) c else i + 1
                  j += 1
                }
                i += 1
              }
              activeCompact += 1
              compacts(activeCompact) = newCompact
              compact = newCompact
              isCompact = true
            }
          }
        }
      }
    }
    object Drawn {
      private val emptyIdx = Integer.MIN_VALUE
      val empty            = new Drawn(0.asInstanceOf[N], emptyIdx, -1)
    }

    private val r = new Random
    def draw: Drawn =
      if (active <= 0) Drawn.empty
      else {
        val len = compact.length
        var i   = r.nextInt(len)
        while (compact(i) <= 0) {
          i += 1
          if (i == len) i = 0
        }
        new Drawn(nodes(if (isCompact) compact(i) - 1 else i), i, activeCompact)
      }

    def draw(count: Int): ArrayBuffer[Drawn] = {
      val all = new ArrayBuffer[Drawn](count)
      var i   = 0
      while (i < count) {
        val one = draw
        if (one.isDefined) {
          all += one
          i += 1
        } else i = count + 1
      }
      if (i > count) ArrayBuffer.empty else all
    }
  }

  final protected[RandomGraph] class OuterNodes {
    val nodes   = MSet.empty[N]
    val degrees = new Degrees

    addExact[N](
      nrToAdd = order,
      add = add(nodeFactory),
      infiniteMsg = "'nodeFactory' yields too many duplicates.",
      false
    )

    private def add(n: N): Boolean =
      if (nodes.add(n)) {
        def degree: Int = {
          val d = nodeDegree.draw
          if (connected) d - 2
          else d
        }
        degrees.add(n, degree)
        true
      } else false

    def sliding2(f: (N, N) => Unit) = degrees.sliding2(f)
  }

  protected[RandomGraph] class RandomEdge(weightFactory: () => Long, labelFactory: () => Any)(implicit val d: Degrees) {
    private[this] val c = RandomEdge.drawCompanion
    val degrees = d.draw(
      c match {
        case _: EdgeCompanion[_] => 2
        case _                   => 2 + RandomEdge.r.nextInt(5) // TODO use EdgeArityRange instead
      }
    )

    final def isDefined = !degrees.isEmpty

    def draw: E = {
      val it = degrees.iterator
      draw(it.next().node, it.next().node, it.map(_.node).toSeq: _*)
    }

    def draw(n1: N, n2: N): E =
      (c match {
        /* TODO L
        case c: WLEdgeCompanion[E]    => c.from(n1, n2)(weightFactory(), labelFactory())
        case c: WEdgeCompanion[E]     => c.from(n1, n2)(weightFactory())
        case c: LEdgeCompanion[E]     => c.from(n1, n2)(labelFactory())
         */
        case c: EdgeCompanion[_]      => c(n1, n2)
        case c: HyperEdgeCompanion[_] => c(n1, n2) // TODO Hyper
        case x                        => RandomEdge.throwUnsupportedEdgeCompanionException(x)
      }).asInstanceOf[E]

    def draw(n1: N, n2: N, n: N*): E =
      (c match {
        /* TODO L
        case c: WLEdgeCompanion[E]      => c(n1, n2)(weightFactory(), labelFactory())
        case c: WLHyperEdgeCompanion[E] => c(n1, n2, n: _*)(weightFactory(), labelFactory())
        case c: WEdgeCompanion[E]       => c(n1, n2)(weightFactory())
        case c: WHyperEdgeCompanion[E]  => c(n1, n2, n: _*)(weightFactory())
        case c: LEdgeCompanion[E]       => c(n1, n2)(labelFactory())
        case c: LHyperEdgeCompanion[E]  => c(n1, n2, n: _*)(labelFactory())
         */
        case c: EdgeCompanion[_]      => c(n1, n2)
        case c: HyperEdgeCompanion[_] => c(n1, n2, n: _*) // TODO Hyper
        case x                        => RandomEdge.throwUnsupportedEdgeCompanionException(x)
      }).asInstanceOf[E]

    def setUsed: Boolean = {
      degrees foreach (_.setUsed)
      true
    }
  }

  protected[RandomGraph] object RandomEdge {
    val companions     = edgeCompanions.toArray
    val nrOfCompanions = companions.size

    val r             = Random
    def drawCompanion = companions(r.nextInt(nrOfCompanions))

    def throwUnsupportedEdgeCompanionException(c: EdgeCompanionBase) =
      throw new IllegalArgumentException(s"The edge companion '$c' not supported.")
  }

  final protected[RandomGraph] class OuterElems(weightFactory: () => Long, labelFactory: () => Any) {
    traceln("Creating OuterNodes...")
    private val nodes = new OuterNodes
    traceln("OuterNodes created.")

    def outerNodes = nodes.nodes

    private val edges = MSet.empty[E]
    def outerEdges    = edges

    implicit private val degrees = nodes.degrees

    if (connected)
      nodes.sliding2 { (n1, n2) =>
        edges += new RandomEdge(weightFactory, labelFactory).draw(n1, n2)
      }

    var edge  = new RandomEdge(weightFactory, labelFactory)
    var added = false
    do {
      val mayFinish = degrees.mayFinish
      added = addExact[E](
        1,
        if (edge.isDefined && edges.add(edge.draw)) {
          edge.setUsed
        } else {
          edge = new RandomEdge(weightFactory, labelFactory)
          false
        },
        "An 'edgeCompanion' returns too many duplicates or the requested node degree is too high.",
        mayFinish
      )
      if (added) edge = new RandomEdge(weightFactory, labelFactory)
    } while (added && edge.isDefined)
  }

  /** Returns a random graph of the specified type and with the metrics passed to the enclosing class.
    */
  def draw: G[N, E] = {
    val elems = new OuterElems(
      weightFactory getOrElse (new DefaultWeightFactory).apply,
      labelFactory getOrElse (new DefaultLabelFactory).apply
    )
    graphCompanion.from(elems.outerNodes, elems.outerEdges)
  }

  /** Returns an empty graph of the type as specified for the enclosing class.
    */
  def empty: G[N, E] = graphCompanion.empty
}

/** Provides convenience metrics and methods for the generation of random graphs.
  *
  * @define COMPANION The graph companion object such as `scalax.collection.Graph` to be used to generate graphs.
  * @define METRICS   The `Metrics` to be applied to the generated graph.
  */
object RandomGraph {

  def apply[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G],
      order: Int,
      nodeFactory: => N,
      nodeDegree: NodeDegreeRange,
      edgeCompanions: Set[EdgeCompanionBase],
      connected: Boolean,
      weightFactory: Option[() => Long],
      labelFactory: Option[() => Any]
  )(implicit nodeTag: ClassTag[N]): RandomGraph[N, E, G] =
    new RandomGraph[N, E, G](
      graphCompanion,
      order,
      nodeFactory,
      nodeDegree,
      edgeCompanions,
      connected,
      weightFactory,
      labelFactory
    )

  def fromMetrics[N, E <: Edge[N], G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G],
      metrics: Metrics[N],
      edgeCompanions: Set[EdgeCompanionBase]
  )(implicit nodeTag: ClassTag[N]): RandomGraph[N, E, G] =
    new RandomGraph[N, E, G](
      graphCompanion,
      metrics.order,
      metrics.nodeGen,
      metrics.nodeDegrees,
      edgeCompanions,
      metrics.connected
    )

  /** Template for `Metrics` with `connected` set to `true`
    *  and some lazy values useful for checking the metrics of generated graphs.
    */
  trait MetricsBase[N] {
    def order: Int
    def nodeDegrees: NodeDegreeRange
    def connected             = true
    protected def minMax: Int = 10 * order

    lazy val isDense: Boolean = nodeDegrees.mean / order match {
      case x if x > 0.7f => true
      case x if x > 0.5f => nodeDegrees.max >= order - 2
      case _             => false
    }
    lazy val expectedTotalDegree: Int = (order * nodeDegrees.mean).toInt
    lazy val divisor: Int = {
      val d = if (isDense) 8 else 22
      if (order > 50) d else d / 6
    }
    lazy val maxDegreeDeviation: Int = expectedTotalDegree / divisor
  }
  trait Metrics[N] extends MetricsBase[N] {
    def nodeGen: N
  }
  trait IntFactory extends Metrics[Int] {
    private val r    = new Random
    def nodeGen: Int = r.nextInt(10 * order)
  }

  /** Predefined metrics of a 'tiny' graph with the node type of `Int`,
    *  an order of `5` and a node degree range of `2` to `4`.
    */
  object TinyInt extends IntFactory {
    val order       = 5
    val nodeDegrees = NodeDegreeRange(2, 4)
  }

  /** Predefined metrics of a 'small' graph with the node type of `Int`,
    *  an order of `20` and a node degree range of `2` to `5`.
    */
  object SmallInt extends IntFactory {
    val order       = 20
    val nodeDegrees = NodeDegreeRange(2, 5)
  }

  /** Returns a generator for tiny, connected, non-labeled directed graphs with the metrics defined by `TinyInt`.
    *
    *  @param graphCompanion $COMPANION
    */
  def tinyConnectedIntDi[G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G]
  ): RandomGraph[Int, DiEdge[Int], G] =
    RandomGraph.fromMetrics[Int, DiEdge[Int], G](graphCompanion, TinyInt, Set(DiEdge))

  /** Returns a generator for small, connected, non-labeled directed graphs with the metrics defined by `SmallInt`.
    *
    *  @param graphCompanion $COMPANION
    */
  def smallConnectedIntDi[N, G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G]
  ): RandomGraph[Int, DiEdge[Int], G] =
    RandomGraph.fromMetrics[Int, DiEdge[Int], G](graphCompanion, SmallInt, Set(DiEdge))

  /** Returns a generator for non-labeled directed graphs of any metrics and any type.
    *
    * @param graphCompanion $COMPANION
    * @param metrics $METRICS
    */
  def diGraph[N, G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G],
      metrics: Metrics[N]
  )(implicit nodeTag: ClassTag[N]): RandomGraph[N, DiEdge[N], G] =
    RandomGraph.fromMetrics[N, DiEdge[N], G](graphCompanion, metrics, Set(DiEdge))

  /** Returns a generator for non-labeled undirected graphs of any metrics and any type.
    *
    * @param graphCompanion $COMPANION
    * @param metrics $METRICS
    */
  def unDiGraph[N, G[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, G]](
      graphCompanion: GenericGraphFactory[G],
      metrics: Metrics[N]
  )(implicit nodeTag: ClassTag[N]): RandomGraph[N, UnDiEdge[N], G] =
    RandomGraph.fromMetrics[N, UnDiEdge[N], G](graphCompanion, metrics, Set(UnDiEdge))

}
