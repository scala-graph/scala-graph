package custom

import scala.collection.generic.CanBuildFrom

import scalax.collection._
import scalax.collection.GraphPredef.{EdgeLikeIn, GraphParam, GraphParamIn} 
import scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCompanion
import scalax.collection.immutable.AdjacencyListBase
import scalax.collection.mutable.ArraySet
import config.AdjacencyListArrayConfig
import scalax.collection.io._

// here goes the real extension -----------------------------------------------

/**	This trait extends the inner node type of Graph by inheritance.
 *  Inner nodes will have an additional method `helloAdjacents`.
 */
trait MyExtGraphLike[N,
                     E[X]  <: EdgeLikeIn[X],
                     +This[X, Y[X]<:EdgeLikeIn[X]]
                           <: MyExtGraphLike[X,Y,This] with Set[GraphParam[X,Y]] with Graph[X,Y]]
  extends GraphLike[N,E,This]
{
  trait InnerNodeLike extends super.InnerNodeLike {
    this: NodeT =>
    def helloSuccessors = "Hello " + (sorted(diSuccessors) mkString ",") + "!"

    /* we need the following sorting for test purposes only;
     * it ensures that the elements in the returned string
     * are in the same order as any expected test result.
     */
    protected def sorted(nodes: collection.Set[NodeT]) =
      (nodes map (_.toString) toList).sorted
  }
}

/* the rest is a lot but almost just copy & paste -----------------------------
 * Here the components must be assembled anew to include the extension.
 * I'm going to elaborate a more elegant alternative...
 */
package immutable {
  import scalax.collection.immutable.{Graph => ImmutableGraph, AdjacencyListGraph}
  import scalax.collection.generic.ImmutableGraphCompanion

//  abstract class ExtendableNode[N,
//                                E[X] <: EdgeLikeIn[X],
//                               +This[X, Y[X]<:EdgeLikeIn[X]] <: AdjacencyListGraph[X,Y,This] with ImmutableGraph[X,Y]]
//    extends ImmutableGraph[N,E]
//    with    AdjacencyListGraph[N,E,This]
//    with    GraphTraversalImpl[N,E]
//    with    Serializable
//  {
//    val graphCompanion = MyExtGraph
//    protected type Config = MyExtGraph.Config
//    override final def config = _config.asInstanceOf[graphCompanion.Config with Config]
//
//    protected val _nodes = new NodeSet 
//    protected val _edges = EdgeSet
//    initialize(iniNodes, iniEdges)
//  
//    @inline final override def empty = MyExtGraph.empty[N,E]
//    @inline final override def clone = MyExtGraph.from [N,E](_nodes.toNodeInSet,
//                                                             _edges.toEdgeInSet)
//    @inline final override def copy(nodes: Iterable[N],
//                                    edges: Iterable[E[N]])=
//      MyExtGraph.from[N,E](nodes, edges)
//    final protected class NodeBase(value: N, hints: ArraySet.Hints)
//      extends InnerNodeImpl(value, hints)
//      with    InnerNodeLike // inner class of  extension trait
//      with    InnerNodeTraversalImpl
//    type NodeT = NodeBase
//    @inline final protected def newNode(n: N) = new NodeT(n, config.adjacencyListHints)
//    @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
//  }
  /** Custom immutable Graph based on the default implementation
   *  and extended by `MyExtGraphLike`.
   *  You just need to replace the occurrences of `MyExtGraphLike`.  
   */
  class MyExtGraph[N, E[X] <: EdgeLikeIn[X]]
      ( iniNodes: Iterable[N]    = Set[N](),
        iniEdges: Iterable[E[N]] = Set[E[N]]() )
      ( implicit override val edgeManifest: Manifest[E[N]],
        val _config: MyExtGraph.Config with AdjacencyListArrayConfig)
    extends ImmutableGraph[N,E]
    with    AdjacencyListGraph[N,E,MyExtGraph]
    with    GraphTraversalImpl[N,E]
    with    Serializable
    with    MyExtGraphLike[N,E,MyExtGraph] // extension trait
  {
    val graphCompanion = MyExtGraph
    protected type Config = MyExtGraph.Config
    override final def config = _config.asInstanceOf[graphCompanion.Config with Config]

    private def this(that: AdjacencyListBase[N,E,MyExtGraph])
                    (delNodes: Iterable[AdjacencyListBase[N,E,MyExtGraph]#NodeT],
                     delEdges: Iterable[AdjacencyListBase[N,E,MyExtGraph]#EdgeT],
                     ripple:   Boolean,
                     addNodes: Iterable[N],
                     addEdges: Iterable[E[N]])
                    (implicit edgeManifest: Manifest[E[N]],
                     config: MyExtGraph.Config with AdjacencyListArrayConfig) = {
      this()
      from(that)(delNodes, delEdges, ripple, addNodes, addEdges)
    }
    protected final def alteredClone
        (delNodes: Iterable[NodeT],
         delEdges: Iterable[EdgeT],
         ripple:   Boolean,
         addNodes: Iterable[N],
         addEdges: Iterable[E[N]]) = {
      new MyExtGraph(this)(
          delNodes.asInstanceOf[Iterable[AdjacencyListBase[N,E,MyExtGraph]#NodeT]],
          delEdges.asInstanceOf[Iterable[AdjacencyListBase[N,E,MyExtGraph]#EdgeT]],
          ripple, addNodes, addEdges)
    }

    @inline final def newNodeSet: NodeSetT = new NodeSet
    override final val nodes = newNodeSet 
    override final val edges = new EdgeSet
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](nodes.toNodeInSet,
                                                             edges.toEdgeInSet)
    @inline final override def copy(nodes: Iterable[N],
                                    edges: Iterable[E[N]])=
      MyExtGraph.from[N,E](nodes, edges)
    final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with    InnerNodeLike // inner class of  extension trait
      with    InnerNodeTraversalImpl
    type NodeT = NodeBase
    @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
  }
  object MyExtGraph extends ImmutableGraphCompanion[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                        config: Config) = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                                 edges: Iterable[E[N]])
                                                (implicit edgeManifest: Manifest[E[N]],
                                                 config: Config) =
      new MyExtGraph[N,E](nodes, edges)
    override def fromStream[N, E[X] <: EdgeLikeIn[X]]
       (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
        nodes:       Iterable[N]                  = Seq.empty[N],
        edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
        edges:       Iterable[E[N]]               = Seq.empty[E[N]])
       (implicit edgeManifest: Manifest[E[N]],
        config: Config = defaultConfig): MyExtGraph[N,E] =
      throw new IllegalAccessError("'fromStream' not implemented.")
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](
        implicit edgeManifest: Manifest[E[N]],
        config: Config): CanBuildFrom[Coll, GraphParamIn[N,E], MyExtGraph[N,E]] =
      new GraphCanBuildFrom[N,E]
  }
}
package mutable {
  import scalax.collection.mutable.{Graph => MutableGraph, AdjacencyListGraph} 
  import scalax.collection.generic.MutableGraphCompanion

  /** The mutable variant of `MyExtGraph`. 
   *  You just need to replace the occurrences of `MyMutableExtGraph`.  
   */
  class MyExtGraph[N, E[X] <: EdgeLikeIn[X]]
      ( iniNodes: Iterable[N]    = Set[N](),
        iniEdges: Iterable[E[N]] = Set[E[N]]() )
      ( implicit override val edgeManifest: Manifest[E[N]],
        val _config: MyExtGraph.Config with AdjacencyListArrayConfig)
    extends MutableGraph[N,E]
    with    AdjacencyListGraph[N,E,MyExtGraph]
    with    GraphTraversalImpl[N,E]
    with    Serializable
    with    MyExtGraphLike[N,E,MyExtGraph] // extension trait
  {
    override val graphCompanion = MyExtGraph
    protected type Config = MyExtGraph.Config
    override final def config = _config.asInstanceOf[graphCompanion.Config with Config]

    private def this(that: AdjacencyListBase[N,E,MyExtGraph])
                    (delNodes: Iterable[AdjacencyListBase[N,E,MyExtGraph]#NodeT],
                     delEdges: Iterable[AdjacencyListBase[N,E,MyExtGraph]#EdgeT],
                     ripple:   Boolean,
                     addNodes: Iterable[N],
                     addEdges: Iterable[E[N]])
                    (implicit edgeManifest: Manifest[E[N]],
                     config: MyExtGraph.Config with AdjacencyListArrayConfig) = {
      this()
      from(that)(delNodes, delEdges, ripple, addNodes, addEdges)
    }
    protected final def alteredClone
        (delNodes: Iterable[NodeT],
         delEdges: Iterable[EdgeT],
         ripple:   Boolean,
         addNodes: Iterable[N],
         addEdges: Iterable[E[N]]) = {
      new MyExtGraph(this)(
          delNodes.asInstanceOf[Iterable[AdjacencyListBase[N,E,MyExtGraph]#NodeT]],
          delEdges.asInstanceOf[Iterable[AdjacencyListBase[N,E,MyExtGraph]#EdgeT]],
          ripple, addNodes, addEdges)
    }

    @inline final def newNodeSet: NodeSetT = new NodeSet
    override final val nodes = newNodeSet 
    override final val edges = new EdgeSet
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](nodes.toNodeInSet,
                                                             edges.toEdgeInSet)
    final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with    InnerNodeLike // inner class of  extension trait
      with    InnerNodeTraversalImpl
    type NodeT = NodeBase
    @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)
  }
  object MyExtGraph extends MutableGraphCompanion[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeManifest: Manifest[E[N]],
                                        config: Config) = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Iterable[N],
                                                 edges: Iterable[E[N]])
                                                (implicit edgeManifest: Manifest[E[N]],
                                                 config: Config) =
      new MyExtGraph[N,E](nodes, edges)
    override def fromStream[N, E[X] <: EdgeLikeIn[X]]
       (nodeStreams: Iterable[NodeInputStream[N]] = Seq.empty[NodeInputStream[N]],
        nodes:       Iterable[N]                  = Seq.empty[N],
        edgeStreams: Iterable[GenEdgeInputStream[N,E]] = Seq.empty[GenEdgeInputStream[N,E]],
        edges:       Iterable[E[N]]               = Seq.empty[E[N]])
       (implicit edgeManifest: Manifest[E[N]],
        config: Config = defaultConfig): MyExtGraph[N,E] =
      throw new IllegalAccessError("'fromStream' not implemented.")
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](
        implicit edgeManifest: Manifest[E[N]],
        config: Config): CanBuildFrom[Coll, GraphParamIn[N,E], MyExtGraph[N,E]] =
      new GraphCanBuildFrom[N,E]
  }
}
