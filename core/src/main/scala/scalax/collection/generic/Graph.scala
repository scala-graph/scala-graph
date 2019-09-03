package scalax.collection
package generic

import language.higherKinds
import collection.generic.CanBuildFrom
import collection.mutable.Builder
import scala.reflect.ClassTag

import GraphPredef.{EdgeLikeIn, Param}
import config.{CoreConfig, GraphConfig}
import mutable.GraphBuilder

/** Methods common to `Graph` companion objects in the core module.
  *
  * @tparam CC the kind of type of the graph that is to become the companion class/trait
  *         of the object extending this trait.
  * @define DUPLEXCL Duplicate exclusion takes place on the basis of values
  *         returned by `hashCode` of the supplied nodes and edges. The hash-code
  *         value of an edge is determined by its ends and optionally by other
  *         edge components such as `weight` or `label`. To include non-node edge
  *         components in the hash-code of an edge make use of any of the predefined
  *         key-weighted/key-labeled edges or mix `ExtendedKey` into your custom
  *         edge class.
  * @define EDGES all edges to be included in the edge set of the graph to be
  *         created. Edge ends will be added to the node set automatically.
  * @define INNODES The isolated (and optionally any other) outer nodes that the node set of
  *         this graph is to be populated with. This parameter may be used as an alternative
  *         or in addition to `nodeStreams`.
  * @define INEDGES The outer edges that the edge set of this graph is to be populated with.
  *         Nodes being the end of any of these edges will be added to the node set.
  *         This parameter is meant be used as an alternative or in addition to `edgeStreams`.
  * @author Peter Empen
  */
trait GraphCompanion[+CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]] {

  /** Type of configuration required for a specific `Graph` companion. */
  type Config <: GraphConfig

  /** The default configuration to be used in absence of a user-supplied configuration. */
  def defaultConfig: Config

  protected[this] type Coll = CC[_, Nothing]

  /** Creates an empty `Graph` instance. */
  def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config): CC[N, E]

  /** Creates a `Graph` with a node set built from all nodes in `elems` including
    * edge ends and with an edge set containing all edges in `elems`.
    * $DUPLEXCL
    *
    * @param   elems sequence of nodes and/or edges in an arbitrary order
    * @return  A new graph instance containing the nodes and edges derived from `elems`.
    */
  def apply[N, E[+X] <: EdgeLikeIn[X]](elems: Param[N, E]*)(implicit edgeT: ClassTag[E[N]], config: Config): CC[N, E] =
    (newBuilder[N, E] ++= elems).result

  /** Produces a graph with a node set containing all `nodes` and edge ends in `edges`
    * and with an edge set containing all `edges` but duplicates.
    * $DUPLEXCL
    *
    * @param nodes the isolated and optionally any other non-isolated nodes to
    *        be included in the node set of the graph to be created.
    * @param edges $EDGES
    * @return  A new graph instance containing `nodes` and all edge ends
    *          and `edges`.
    */
  def from[N, E[+X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil,
                                      edges: Traversable[E[N]])(implicit edgeT: ClassTag[E[N]], config: Config): CC[N, E]

  /** Produces a graph containing the results of some element computation a number of times.
    * $DUPLEXCL
    *
    * @param   nr  the number of elements to be contained in the graph.
    * @param   elem the element computation returning nodes or edges `nr` times.
    * @return  A graph that contains the results of `nr` evaluations of `elem`.
    */
  def fill[N, E[+X] <: EdgeLikeIn[X]](nr: Int)(elem: => Param[N, E])(implicit edgeT: ClassTag[E[N]],
                                                                     config: Config): CC[N, E] = {
    val gB = newBuilder[N, E].asInstanceOf[GraphBuilder[N, E, CC]]
    gB.sizeHint(nr)
    var i = 0
    while (i < nr) {
      gB += elem
      i += 1
    }
    gB.result
  }
  def newBuilder[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                           config: Config): Builder[Param[N, E], CC[N, E]] =
    new GraphBuilder[N, E, CC](this)
  /* TODO build from
  class GraphCanBuildFrom[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config)
      extends CanBuildFrom[Coll, Param[N, E], CC[N, E]] {
    def apply(from: Coll) = newBuilder[N, E]
    def apply()           = newBuilder[N, E]
    override def fromSpecific(from: Coll)(it: IterableOnce[Param[N, E]]) = ???
    override def newBuilder(from: Coll) = ???
  } */
}

/** `GraphCompanion` extended to work with `CoreConfig`. */
trait GraphCoreCompanion[+CC[N, E[+X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]]
    extends GraphCompanion[CC] {
  type Config = CoreConfig
  def defaultConfig = CoreConfig()
  def empty[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]], config: Config = defaultConfig): CC[N, E]
  override def apply[N, E[+X] <: EdgeLikeIn[X]](elems: Param[N, E]*)(implicit edgeT: ClassTag[E[N]],
                                                                     config: Config = defaultConfig): CC[N, E] =
    super.apply(elems: _*)(edgeT, config)
  def from[N, E[+X] <: EdgeLikeIn[X]](nodes: Iterable[N] = Nil, edges: Iterable[E[N]])(
      implicit edgeT: ClassTag[E[N]],
      config: Config = defaultConfig): CC[N, E]
  override def fill[N, E[+X] <: EdgeLikeIn[X]](nr: Int)(elem: => Param[N, E])(implicit edgeT: ClassTag[E[N]],
                                                                              config: Config = defaultConfig): CC[N, E] =
    super.fill(nr)(elem)(edgeT, config)
}

trait ImmutableGraphCompanion[+CC[N, E[+X] <: EdgeLikeIn[X]] <: immutable.Graph[N, E] with GraphLike[N, E, CC]]
    extends GraphCoreCompanion[CC]

trait MutableGraphCompanion[+CC[N, E[+X] <: EdgeLikeIn[X]] <: mutable.Graph[N, E] with mutable.GraphLike[N, E, CC]]
    extends GraphCoreCompanion[CC] {
  override def newBuilder[N, E[+X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                                     config: Config): Builder[Param[N, E], CC[N, E]] =
    new GraphBuilder[N, E, CC](this)(edgeT, config)
}
