package custom

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.{Set => AnySet}
import scala.reflect.ClassTag

import scalax.collection._
import scalax.collection.GraphPredef.{EdgeLikeIn, Param, InParam} 
import scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCompanion
import scalax.collection.immutable.AdjacencyListBase
import scalax.collection.mutable.ArraySet
import config.AdjacencyListArrayConfig

// here goes the real extension -----------------------------------------------

/**	This trait extends the inner node type of Graph by inheritance.
 *  Inner nodes will have an additional method `helloAdjacents`.
 */
trait MyExtGraphLike[N,
                     E[X]  <: EdgeLikeIn[X],
                     +This[X, Y[X]<:EdgeLikeIn[X]]
                           <: MyExtGraphLike[X,Y,This] with AnySet[Param[X,Y]] with Graph[X,Y]]
  extends GraphLike[N,E,This]
{ this: This[N,E] =>
  trait InnerNode extends super.InnerNode {
    this: NodeT =>
    def helloSuccessors = "Hello " + (sorted(diSuccessors) mkString ",") + "!"

    /* we need the following sorting for test purposes only;
     * it ensures that the elements in the returned string
     * are in the same order as any expected test result.
     */
    protected def sorted(nodes: collection.Set[NodeT]) =
      (nodes.map(_.toString).toList).sorted
  }
}

/* the rest is a lot but almost just copy & paste -----------------------------
 * Here the components must be assembled anew to include the extension.
 * I'm going to elaborate a more elegant alternative...
 */
package immutable {
  import scalax.collection.immutable.{Graph => ImmutableGraph, AdjacencyListGraph}
  import scalax.collection.generic.ImmutableGraphCompanion

  /** Custom immutable Graph based on the default implementation
   *  and extended by `MyExtGraphLike`.
   *  You just need to replace the occurrences of `MyExtGraphLike`.  
   */
  class MyExtGraph[N, E[X] <: EdgeLikeIn[X]]
      ( iniNodes: Traversable[N]    = Nil,
        iniEdges: Traversable[E[N]] = Nil)
      ( implicit override val edgeT: ClassTag[E[N]],
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

    @inline final protected def newNodeSet: NodeSetT = new NodeSet
    @transient private[this] var _nodes: NodeSetT = newNodeSet
    @inline override final def nodes = _nodes
    
    @transient private[this] var _edges: EdgeSetT = new EdgeSet
    @inline override final def edges = _edges
    
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](nodes.toOuter,
                                                             edges.toOuter)
    @inline final override def copy(nodes: Traversable[N],
                                    edges: Traversable[E[N]])= MyExtGraph.from[N,E](nodes, edges)
                                    
    final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with    InnerNode // inner class of  extension trait
      with    InnerNodeTraversalImpl
      
    type NodeT = NodeBase
    
    @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)

    private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)
  
    private def readObject(in: ObjectInputStream): Unit = {
      _nodes = newNodeSet
      _edges = new EdgeSet
      initializeFrom(in, _nodes, _edges)
    }
  }
  object MyExtGraph extends ImmutableGraphCompanion[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                        config: Config) = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N] = Nil,
                                                 edges: Traversable[E[N]])
                                                (implicit edgeT: ClassTag[E[N]],
                                                 config: Config) =
      new MyExtGraph[N,E](nodes, edges)
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](
        implicit edgeT: ClassTag[E[N]],
        config: Config): CanBuildFrom[Coll, InParam[N,E], MyExtGraph[N,E]] =
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
      ( iniNodes: Traversable[N]    = Set[N](),
        iniEdges: Traversable[E[N]] = Set[E[N]]() )
      ( implicit override val edgeT: ClassTag[E[N]],
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

    @inline final protected def newNodeSet: NodeSetT = new NodeSet
    @transient private[this] var _nodes: NodeSetT = newNodeSet
    @inline override final def nodes = _nodes
    
    @transient private[this] var _edges: EdgeSetT = new EdgeSet
    @inline override final def edges = _edges
    
    initialize(iniNodes, iniEdges)
  
    @inline final override def empty = MyExtGraph.empty[N,E]
    @inline final override def clone = MyExtGraph.from [N,E](nodes.toOuter,
                                                             edges.toOuter)
    final protected class NodeBase(value: N, hints: ArraySet.Hints)
      extends InnerNodeImpl(value, hints)
      with    InnerNode // inner class of  extension trait
      with    InnerNodeTraversalImpl
      
    type NodeT = NodeBase
    
    @inline final protected def newNodeWithHints(n: N, h: ArraySet.Hints) = new NodeT(n, h)

    private def writeObject(out: ObjectOutputStream): Unit = serializeTo(out)
  
    private def readObject(in: ObjectInputStream): Unit = {
      _nodes = newNodeSet
      _edges = new EdgeSet
      initializeFrom(in, _nodes, _edges)
    }
  }
  object MyExtGraph extends MutableGraphCompanion[MyExtGraph]
  {
    def empty[N, E[X] <: EdgeLikeIn[X]](implicit edgeT: ClassTag[E[N]],
                                        config: Config) = new MyExtGraph[N,E]
    override def from [N, E[X] <: EdgeLikeIn[X]](nodes: Traversable[N],
                                                 edges: Traversable[E[N]])
                                                (implicit edgeT: ClassTag[E[N]],
                                                 config: Config) = new MyExtGraph[N,E](nodes, edges)
    implicit def canBuildFrom[N, E[X] <: EdgeLikeIn[X]](
        implicit edgeT: ClassTag[E[N]],
        config: Config): CanBuildFrom[Coll, InParam[N,E], MyExtGraph[N,E]] =
      new GraphCanBuildFrom[N,E]
  }
}
