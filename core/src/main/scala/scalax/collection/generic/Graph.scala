package scalax.collection
package generic

import scalax.collection.config.{AdjacencyListArrayConfig, CoreConfig, GraphConfig}
import scalax.collection.mutable.Builder

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
sealed trait Factory[+CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] {

  /** The default configuration to be used in absence of a user-supplied configuration. */
  def defaultConfig: GraphConfig

  // protected[this] type Coll = CC[_, Nothing]

  def newBuilder[N, E <: Edge[N]](implicit config: GraphConfig = defaultConfig) = new Builder[N, E, CC](this)

  protected[collection] def fromSpecific[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: GraphConfig
  ): CC[N, E] =
    this match {
      case gC: GenericGraphFactory[CC] => gC.from[N, E](nodes, edges)(config)
      case tC: generic.TypedGraphFactory[N @unchecked, E @unchecked, CC @unchecked] => tC.from(nodes, edges)(config)
    }

  final protected def coreConfig(config: GraphConfig): GraphConfig with AdjacencyListArrayConfig =
    config match {
      case c: AdjacencyListArrayConfig => c
      case _                           => CoreConfig()
    }
}

trait GenericGraphFactory[+CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] extends Factory[CC] {

  /** Creates an empty `Graph` instance. */
  def empty[N, E <: Edge[N]](implicit config: GraphConfig = defaultConfig): CC[N, E]

  /** Creates a `Graph` with a node set built from all nodes in `elems` including
    * edge ends and with an edge set containing all edges in `elems`.
    * $DUPLEXCL
    *
    * @param   elems sequence of nodes and/or edges in an arbitrary order
    * @return  A new graph instance containing the nodes and edges derived from `elems`.
    */
  def apply[N, E[X] <: Edge[X]](elems: OuterElem[N, E[N]]*)(implicit config: GraphConfig = defaultConfig): CC[N, E[N]] =
    (newBuilder[N, E[N]] ++= elems).result

  def from[N, E[X] <: Edge[X]](edges: Iterable[E[N]]): CC[N, E[N]]

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
  def from[N, E <: Edge[N]](nodes: Iterable[N], edges: Iterable[E])(implicit
      config: GraphConfig = defaultConfig
  ): CC[N, E]

  /** Produces a graph containing the results of some element computation a number of times.
    * $DUPLEXCL
    *
    * @param   nr  the number of elements to be contained in the graph.
    * @param   elem the element computation returning nodes or edges `nr` times.
    * @return  A graph that contains the results of `nr` evaluations of `elem`.
    */
  def fill[N, E <: Edge[N]](
      nr: Int
  )(elem: => OuterElem[N, E])(implicit config: GraphConfig = defaultConfig): CC[N, E] = {
    val gB = newBuilder[N, E]
    // TODO gB.sizeHint(nr)
    var i = 0
    while (i < nr) {
      gB addOuter elem
      i += 1
    }
    gB.result
  }
}

trait TypedGraphFactory[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, CC]]
    extends Factory[CC] {

  def empty(implicit config: GraphConfig = defaultConfig): CC[N, E]

  def apply[NN <: N, EE <: E with Edge[NN]](elems: OuterElem[NN, EE]*)(implicit
      config: GraphConfig = defaultConfig
  ): CC[NN, EE] =
    (newBuilder[NN, EE] ++= elems).result

  def from(edges: Iterable[E]): CC[N, E]

  def from(nodes: Iterable[N], edges: Iterable[E])(implicit config: GraphConfig = defaultConfig): CC[N, E]
}

protected trait DefaultConfig[+CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]] { this: Factory[CC] =>
  def defaultConfig = CoreConfig()
}

trait GenericGraphCoreFactory[+CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]]
    extends GenericGraphFactory[CC]
    with DefaultConfig[CC]

trait TypedGraphCoreFactory[N, E <: Edge[N], +CC[X, Y <: Edge[X]] <: AnyGraph[X, Y] with GraphLike[X, Y, CC]]
    extends TypedGraphFactory[N, E, CC]
    with DefaultConfig[CC]

trait ImmutableFactory[+CC[N, E <: Edge[N]] <: immutable.Graph[N, E] with GraphLike[N, E, CC]]
    extends GenericGraphCoreFactory[CC]

trait MutableFactory[+CC[N, E <: Edge[N]] <: mutable.Graph[N, E] with mutable.GraphLike[N, E, CC]]
    extends GenericGraphCoreFactory[CC] {

  override def newBuilder[N, E <: Edge[N]](implicit config: GraphConfig) =
    new Builder[N, E, CC](this)(config)
}
