package scalax.collection
package generator

import scala.language.higherKinds
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Set => MSet}
import scala.math.pow
import scala.util.Random
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

import GraphPredef._, GraphEdge._
import generic.GraphCompanion

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
abstract class RandomGraph[N,
                           E[X] <: EdgeLikeIn[X],
                           G[X,Y[Z] <: EdgeLikeIn[Z]] <: Graph[X,Y] with GraphLike[X,Y,G]]
   (val graphCompanion: GraphCompanion[G],
    val order:          Int,
    nodeFactory:        => N,
    nodeDegree:         NodeDegreeRange,
    edgeCompanions:     Set[EdgeCompanionBase[E]],
    connected:          Boolean)
   (implicit edgeTag: TypeTag[E[N]],
    nodeTag: ClassTag[N])
{
  require(order > 0)
  if (connected) require(nodeDegree.min >= 2)
  
  implicit val graphConfig: graphCompanion.Config
  
  protected val doTrace = false
  protected def trace  (str: => String) { if (doTrace) print  (str) }
  protected def traceln(str: => String) { if (doTrace) println(str) }

  private def addExact[A](nrToAdd:     Int,
                          add:         => Boolean,
                          infiniteMsg: String,
                          gentle:      Boolean): Boolean = {
    val checkInfiniteAt = math.pow(math.log(nrToAdd), 2).ceil.toInt + 10
    val infiniteFactor = 5
    
    var added, lastAdded = 0
    var trials = 0
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
  
  protected[RandomGraph] final class Degrees {
    /* Unique nodes inserted by `add`. */
    private[this] val nodes   = new Array[N]  (order)
    
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
    
    def sliding2(f: (N, N) => Unit) = {
      nodes sliding 2 foreach { a: Array[N] => f(a(0), a(1)) }
      f(nodes(order - 1), nodes(0))
    }
    
    def mayFinish: Boolean = active.toFloat / order < 0.5 

    private[this] var idx = 0
    def add(node: N, degree: Int) {
      nodes  (idx) = node
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
        val degreeIdx = if (drawnCompact) compacts(compactIndex)(index) - 1
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
              var i, j = 0
              while (i < len) {
                val c = compact(i)
                if (c > 0 && (! isCompact || degrees(c - 1) > 0)) {
                  newCompact(j) = if(isCompact) c else i + 1
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
      val empty = new Drawn(0.asInstanceOf[N], emptyIdx, -1)
    }
    
    private val r = new Random
    def draw: Drawn =
      if (active <= 0) Drawn.empty
      else {
        val len = compact.length
        var i = r.nextInt(len)
        while (compact(i) <= 0) {
          i += 1
          if (i == len) i = 0
        }
        new Drawn(nodes(if (isCompact) compact(i) - 1 else i), i, activeCompact)
      }

    def draw(count: Int): ArrayBuffer[Drawn] = {
      val all = new ArrayBuffer[Drawn](count)
      var i = 0
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
  
  protected[RandomGraph] final class OuterNodes {
    val nodes = MSet.empty[N]
    val degrees = new Degrees
    
    addExact[N](
        nrToAdd     = order,
        add         = add(nodeFactory),
        infiniteMsg = "'nodeFactory' yields too many duplicates.",
        false)
   
    private def add(n: N): Boolean = {
      if (nodes.add(n)) {
        def degree: Int = {
          val d = nodeDegree.draw
          if (connected) d - 2
          else d
        }
        degrees.add(n, degree)
        true
      } else false
    }
    
    def sliding2(f: (N, N) => Unit) = degrees.sliding2(f)
  }

  protected[RandomGraph] class RandomEdge2(implicit val d: Degrees) {
    protected val degrees = d.draw(2)
    
    final def isDefined = ! degrees.isEmpty

    final protected def _1: N = degrees(0).node
    final protected def _2: N = degrees(1).node
    
    def apply(): E[N] = RandomEdge.draw(_1, _2) 
  }
  
  protected[RandomGraph] final class RandomEdge(implicit override val d: Degrees)
      extends RandomEdge2 {
    import RandomEdge._
    private[this] val c = drawCompanion
    override val degrees = d.draw(
        if (c.isInstanceOf[EdgeCompanion[E]]) 2
        else 2 + r.nextInt(5)) // TODO use EdgeArityRange instead
        
    private def _n = degrees.drop(2) map (_.node)
    
    override def apply(): E[N] = RandomEdge.draw(c, _1, _2, _n: _*)
    
    def setUsed: Boolean = {
      degrees foreach (_.setUsed)
      true
    }
  }
  protected[RandomGraph] object RandomEdge {
    val companions = edgeCompanions.toArray
    val nrOfCompanions = companions.size
    
    val r = Random
    def drawCompanion = companions(r.nextInt(nrOfCompanions)).
        asInstanceOf[EdgeCompanionBase[HyperEdge]]

    def errMsg(c: EdgeCompanionBase[HyperEdge]) = s"The edge companion '$c' not supported."

    def draw(n1: N, n2: N): E[N] =
      (drawCompanion match {
        case c:      EdgeCompanion[E] => c(n1, n2)
        case c: HyperEdgeCompanion[E] => c(n1, n2)
        case x                        => throw new IllegalArgumentException(errMsg(x))
      }).asInstanceOf[E[N]]
    
    def draw(companion: EdgeCompanionBase[HyperEdge], n1: N, n2: N, n: N*): E[N] =
      (companion match {
        case c:      EdgeCompanion[E] => c(n1, n2)
        case c: HyperEdgeCompanion[E] => c(n1, n2, n: _*)
        case x                        => throw new IllegalArgumentException(errMsg(x))
      }).asInstanceOf[E[N]]
  }

  protected[RandomGraph] final class OuterElems {
    traceln("Creating OuterNodes...")
    private val nodes = new OuterNodes
    traceln("OuterNodes created.")

    def outerNodes = nodes.nodes

    private val edges = MSet.empty[E[N]]
    def outerEdges = edges
    
    private implicit val degrees = nodes.degrees
    
    if (connected)
      nodes.sliding2 { (n1, n2) =>
        edges += RandomEdge.draw(n1, n2)
      }
    
    var edge = new RandomEdge
    var added = false
    do {
      val mayFinish = degrees.mayFinish
      added = addExact[E[N]](
          1,
          if (edge.isDefined && edges.add(edge())) {
            edge.setUsed
          } else {
            edge = new RandomEdge
            false
          },
          "An 'edgeCompanion' returns too many duplicates or the requested node degree is too high.",
          mayFinish
      )
      if (added) edge = new RandomEdge 
    } while (added && edge.isDefined)
  }

  /** Returns a random graph of the specified type and with the metrics passed to the enclosing class. 
   */
  def draw: G[N,E] = {
    val elems = new OuterElems
    graphCompanion.from(elems.outerNodes, elems.outerEdges)
  }

  /** Returns an empty graph of the type as specified for the enclosing class. 
   */
  def empty: G[N,E] = graphCompanion.empty
}
/** Provides templates for random graph metrics including shortcuts for metrics
 *  of small random graphs with a node type of `Int`.  
 */
object RandomGraph {
  
  trait MetricsBase[N] {
    def order: Int
    def nodeDegrees: NodeDegreeRange
    def maxEdgeArity = 2
    def connected = true
    protected def minMax = 10 * order

    lazy val isDense = nodeDegrees.mean / order match {
      case x if x > 0.7f => true
      case x if x > 0.5f => nodeDegrees.max >= order - 2
      case _             => false
    }
    lazy val expectedTotalDegree = (order * nodeDegrees.mean).toInt
    lazy val devisor = {
      val d = if (isDense) 15 else 30
      if (order > 50) d else d / 6 
    }
    lazy val maxDegreeDeviation = (expectedTotalDegree / devisor).toInt
  }
  trait Metrics[N] extends MetricsBase[N] {
    def nodeGen: N
  }
  trait IntFactory extends Metrics[Int] {
    private val r = new Random
    def nodeGen = r.nextInt(10 * order)
  }

  object TinyInt extends IntFactory {
    val order = 5
    val nodeDegrees = NodeDegreeRange(2,4)
  }
  object SmallInt extends IntFactory {
    val order = 20
    val nodeDegrees = NodeDegreeRange(2,5)
  }
}