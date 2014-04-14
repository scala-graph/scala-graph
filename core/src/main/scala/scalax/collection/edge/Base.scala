package scalax.collection.edge

import language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

import scalax.collection.GraphEdge._,
       scalax.collection.GraphPredef._
import scalax.collection.{GraphBase, Graph}

/**
 * Base traits for weighted edges.
 * 
 * @author Peter Empen
 */
object WBase {
  trait WEdge[N]
  { this: EdgeLike[N] =>
    override protected def attributesToString = WEdge.wPrefix + weight.toString
  }
  object WEdge {
    val wPrefix = " %"
  }
  type  WHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = HyperEdge[N] with WEdge[N] with Serializable 
  trait WEdgeCompanionBase[E[X] <: EdgeLikeIn[X] with WHyperEdgeBound[_,E]]
    extends EdgeCompanionBase
  {
    /** @param nodes must be of `arity >= 2` for hyperedges and of `arity == 2` for edges. */
    @inline final protected[collection]
    def from [N](nodes: Product)(weight: Long): E[N] = newEdge[N](nodes, weight)
    protected
    def newEdge[N](nodes: Product, weight: Long): E[N] with EdgeCopy[E]
  }
  /** Everything common to weighted hyperedge companion objects. */
  trait WHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WHyperEdgeBound[_,E]]
    extends WEdgeCompanionBase[E]
  {
    @inline final
    def apply[N](node_1: N, node_2: N,
                 nodes: N*)         (weight: Long): E[N] = newEdge[N](NodeProduct(node_1, node_2, nodes: _*), weight)
    @inline final
    def apply[N](nodes: Iterable[N])(weight: Long): E[N] = newEdge[N](NodeProduct(nodes), weight)
    @inline final
    def unapply[N](e: E[N]): Option[(Product,Long)] = Some(e.nodes, e.weight)
  }
  type  WEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WHyperEdgeBound[N,E] with UnDiEdge[N] 
  /** Everything common to weighted edge companion objects. */
  trait WEdgeCompanion[E[X] <: EdgeLikeIn[X] with WEdgeBound[_,E]]
    extends WEdgeCompanionBase[E] 
  {
    @inline final
    def apply[N](node_1: N, node_2: N)(weight: Long): E[N] = newEdge[N](NodeProduct(node_1, node_2), weight)
    @inline final
    def apply[N](nodes: Tuple2[N,N]  )(weight: Long) = newEdge[N](nodes, weight)
    @inline final
    def unapply[N](e: E[N]): Option[(N,N,Long)] = Some(e._1, e._2, e.weight)
  }
}
/** Base traits for key-weighted edges. */
object WkBase {
  import WBase._
  trait WkEdge[N]
      extends WEdge[N]
      with    Keyed {
    this: EdgeLike[N] with Eq =>
    override protected def equals(other: EdgeLike[_]) =
      this.weight == other.weight && (other match {
        case wkEdge: WkEdge[_] => baseEquals(wkEdge)   
        case _ => false
      })
    override def hashCode = baseHashCode ^ weight.##  
  }
  type  WkHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WHyperEdgeBound[N,E] with WkEdge[N] 
  /** Everything common to key-weighted hyperedge companion objects. */
  trait WkHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkHyperEdgeBound[_,E]]
    extends WHyperEdgeCompanion[E] 
  type  WkEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WkHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to key-weighted edge companion objects. */
  trait WkEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkEdgeBound[_,E]]
    extends WEdgeCompanion[E] 
}
/** Base traits for labeled edges. */
object LBase {
  trait LEdge[N]
  { this: EdgeLike[N] =>
    type L1
    override def label: L1 = throw new IllegalArgumentException
    override protected def attributesToString = LEdge.lPrefix + label 
  }
  object LEdge
  {
    val lPrefix  = " '"
    val lkPrefix = " `"
  }
  type  LHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = HyperEdge[N] with LEdge[N] with Serializable 
  trait LEdgeCompanionBase[E[X] <: EdgeLikeIn[X] with LHyperEdgeBound[_,E]]
    extends EdgeCompanionBase
  {
    /** @param nodes must be of `arity >= 2` for hyperedges and of `arity == 2` for edges. */
    @inline final protected[collection]
    def from [N,L]  (nodes: Product)(label: L) = newEdge[N,L](nodes, label)
    @inline final def unapply[N]  (e: E[N]): Option[(N,N,E[N]#L1)] =
      if (e eq null) None else Some((e._1, e._2, e.label))

    def newEdge[N,L](nodes: Product, label_1: L): E[N] with EdgeCopy[E]{type L1 = L}
  }
  /** Everything common to labeled hyperedge companion objects. */
  trait LHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with LHyperEdgeBound[_,E]] 
    extends LEdgeCompanionBase[E] 
  {
    final def apply[N,L](node_1: N, node_2: N, nodes: N*)
                        (label: L) = newEdge[N,L](NodeProduct(node_1, node_2, nodes: _*), label)
    final def apply[N,L](nodes: Iterable[N])
                        (label: L) = newEdge[N,L](NodeProduct(nodes), label)
  }
  type  LEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = LHyperEdgeBound[N,E] with UnDiEdge[N] 
  /** Everything common to predefined edge labeled edge companion objects. */
  trait LEdgeCompanion[E[X] <: EdgeLikeIn[X] with LEdgeBound[_,E]]
    extends LEdgeCompanionBase[E] 
  {
    final def apply[N,L](node_1: N, node_2: N)
                        (label: L) = newEdge[N,L](NodeProduct(node_1, node_2), label)
    final def apply[N,L](nodes: Product)
                        (label: L) = newEdge[N,L](nodes, label)
  }
  /** Implicit conversions from an outer labeled edge to its label.
   * 
   * @tparam UL type of the user label.
   */
  abstract class OuterLEdgeImplicits[UL: ClassTag] {
    /** Lets implicitly convert a labeled outer edge to its label:
      {{{ 
      case class MyLabel(val i: Int)
      val eOuter = LUnDiEdge(1,3)(MyLabel(4))

      object MyLabelConversion extends OuterLEdgeImplicits[MyLabel]
      import MyLabelConversion._
      val four = eOuter.i
      }}}  
     */
    implicit def outerLEdge2UserLabel[N](edge: LEdge[N]{type L1 = UL}): UL = edge.label

    /** Lets implicitly convert a label to its user type:
      {{{
      case class MyLabel(val i: Int)
      val eOuter = LUnDiEdge(1,3)(MyLabel(4))

      object MyLabelConversion extends OuterLEdgeImplicits[MyLabel]
      import MyLabelConversion._
      val label: MyLabel = eOuter.label
      }}}
      As this conversion is not type safe, the user has to ensure that `label`
      is of the type `UL`.  
     */
    implicit def toUserLabel[N, E[X] <: LEdge[X]](label: E[N]#L1): UL =
      try label.asInstanceOf[UL]
      catch {
        case e: ClassCastException => handle(label)
      }

    protected def handle(label: Any) = throw new IllegalArgumentException(
      s"Expected label type: ${
        implicitly[ClassTag[UL]].runtimeClass.getName
      }, found: ${
        label match {
          case r: AnyRef => r.getClass.getName
          case a         => a.toString
        }
      }.")
  }
  /** Implicit conversions from an inner or outer labeled edge to its label.
   *  In case of inner edges, this trait works for [[scalax.collection.Graph]] only.
   *  For other graph types see [[scalax.collection.edge.LBase.TypedLEdgeImplicits]].
   * 
   * @tparam UL type of the user label.
   */
  abstract class LEdgeImplicits[UL: ClassTag]
      extends OuterLEdgeImplicits[UL] {
    /** Lets implicitly convert a labeled inner edge to its label:
      {{{ 
      case class MyLabel(val i: Int)
      val g = Graph(LUnDiEdge(1,3)(MyLabel(4)))
      val eInner = g.edges.head

      object MyLabelConversion extends LEdgeImplicits[MyLabel]
      import MyLabelConversion._
      val four_2 = eInner.i 
      }}}
      As this conversion is not type safe, the user has to ensure that `innerEdge`
      is of appropriate type.  
     */
    implicit def innerEdge2UserLabel[N, E[X] <: EdgeLikeIn[X]]
                                    (innerEdge: Graph[N,E]#EdgeT): UL =
      try innerEdge.edge.label.asInstanceOf[UL]
      catch {
        case e: ClassCastException => handle(innerEdge)
      }
  }
  /** Implicit conversions from an inner or outer labeled edge to its label
   *  for any graph type.
   *
   * @tparam G kind of type of graph.
   * @tparam UL type of the user label.
   */
  abstract class TypedLEdgeImplicits[G[N, E[X] <: EdgeLikeIn[X]] <: GraphBase[N,E], UL: ClassTag]
      extends OuterLEdgeImplicits[UL] {
    /** Lets implicitly convert a labeled inner edge to its label:
      {{{
      import scalax.collection.mutable.{Graph => MGraph}

      case class MyLabel(val i: Int)
      val g = MGraph(LUnDiEdge(1,3)(MyLabel(4)))
      val eInner = g.edges.head

      object MyLabelConversion extends TypedLEdgeImplicits[MGraph, MyLabel]
      import MyLabelConversion._
      val four_2 = eInner.i 
      }}}
      As this conversion is not type safe, the user has to ensure that `innerEdge`
      is of appropriate type.  
     */
    implicit def innerEdge2UserLabel[N, E[X] <: EdgeLikeIn[X]]
                                    (innerEdge: G[N,E]#EdgeT): UL = {
      try innerEdge.edge.label.asInstanceOf[UL]
      catch {
        case e: ClassCastException => handle(innerEdge)
      }
    }
  }
}
/** Base traits for key-labeled edges. */
object LkBase {
  import LBase._
  /** Everything common to all predefined labeled edge classes with a label being a key attribute.
   *  Such labeled and keyed edge classes are named `Lk<EdgeType>Edge` by convention.
   */
  trait LkEdge[N]
      extends LEdge[N]
      with    Keyed {
    this: EdgeLike[N] with Eq =>
    override protected def equals(other: EdgeLike[_]) = other match {
      case that: LkEdge[_] => this.label == that.label &&
                              baseEquals(that)  
      case _ => false
    }
    override def hashCode = baseHashCode ^ label.##  
  }
  type  LkHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = LHyperEdgeBound[N,E] with LkEdge[N] 
  /** Everything common to key-weighted hyperedge companion objects. */
  trait LkHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with LkHyperEdgeBound[_,E]]
    extends LHyperEdgeCompanion[E] 
  type  LkEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = LkHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to key-weighted edge companion objects. */
  trait LkEdgeCompanion[E[X] <: EdgeLikeIn[X] with LkEdgeBound[_,E]]
    extends LEdgeCompanion[E] 
}
/** Base traits for weighted and labeled edges. */
object WLBase {
  import WBase._, LBase._
  /** Everything common to predefined weighted and labeled edge classes.
   *  Such edge classes are prefixed with  `WL` by convention.
   */
  trait WLEdge[N] extends WEdge[N] with LEdge[N]
  { this: EdgeLike[N] =>
    override protected def attributesToString = WEdge.wPrefix + weight.toString +
                                                LEdge.lPrefix + label.toString
  }
  type  WLHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = HyperEdge[N] with WLEdge[N] with Serializable 
  /** Everything common to predefined weighted and labeled hyperedge companion objects. */
  trait WLEdgeCompanionBase[E[X] <: EdgeLikeIn[X] with WLHyperEdgeBound[_,E]]
    extends EdgeCompanionBase
  {
    /** @param nodes must be of `arity >= 2` for hyperedges and of `arity == 2` for edges. */
    @inline final protected[collection]
    def from [N,L](nodes: Product)(weight: Long, label: L): E[N] =
      newEdge[N,L](nodes, weight, label)
    @inline final def unapply[N]  (e: E[N]): Option[(N,N,Long,E[N]#L1)] =
      if (e eq null) None else Some((e._1, e._2, e.weight, e.label))
    protected
    def newEdge[N,L](nodes: Product, weight: Long, label_1: L): E[N] with EdgeCopy[E]
  }
  trait WLHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WLHyperEdgeBound[_,E]]
    extends WLEdgeCompanionBase[E] 
  {
    final def apply[N,L](node_1: N, node_2: N, nodes: N*)(weight: Long, label: L) = newEdge[N,L](NodeProduct(node_1, node_2, nodes: _*), weight, label)
    final def apply[N,L](nodes: Iterable[N])             (weight: Long, label: L) = newEdge[N,L](NodeProduct(nodes), weight, label)
  }
  type  WLEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WLHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to predefined weighted and labeled edge companion objects. */
  trait WLEdgeCompanion[E[X] <: EdgeLikeIn[X] with WLEdgeBound[_,E]]
    extends WLEdgeCompanionBase[E] 
  {
    final def apply[N,L](node_1: N, node_2: N)
                        (weight: Long, label: L) = newEdge[N,L](NodeProduct(node_1, node_2), weight, label)
    final def apply[N,L](nodes: Tuple2[N,N]  )
                        (weight: Long, label: L) = newEdge[N,L](nodes, weight, label)
  }
}
/** Base traits for key-weighted and labeled edges. */
object WkLBase {
  import WkBase._, WLBase._
  type  WkLHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WLHyperEdgeBound[N,E] with WLEdge[N] with WkEdge[N] 
  /** Everything common to key-weighted, labeled hyperedge companion objects. */
  trait WkLHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkLHyperEdgeBound[_,E]]
    extends WLHyperEdgeCompanion[E] 
  type  WkLEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WkLHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to key-weighted, labeled edge companion objects. */
  trait WkLEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkLEdgeBound[_,E]]
    extends WLEdgeCompanion[E] 
}
/** Base traits for weighted and key-labeled edges. */
object WLkBase {
  import WBase._, LBase._, LkBase._, WLBase._
  trait WLkEdge[N] extends WEdge[N] with LkEdge[N]
  { this: EdgeLike[N] with Eq =>
    override protected def attributesToString = WEdge.wPrefix + weight.toString +
                                                LEdge.lkPrefix + label.toString
  }
  type  WLkHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WLHyperEdgeBound[N,E] with WLkEdge[N] 
  /** Everything common to weighted, key-labeled hyperedge companion objects. */
  trait WLkHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WLkHyperEdgeBound[_,E]]
    extends WLHyperEdgeCompanion[E] 
  type  WLkEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WLkHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to weighted, key-labeled edge companion objects. */
  trait WLkEdgeCompanion[E[X] <: EdgeLikeIn[X] with WLkEdgeBound[_,E]]
    extends WLEdgeCompanion[E] 
}
/** Base traits for key-weighted and key-labeled edges. */
object WkLkBase {
  import WLBase._, WkBase._, LkBase._
  trait WkLkEdge[N] extends WLEdge[N] 
  { this: EdgeLike[N] with Eq =>
    override protected def equals(other: EdgeLike[_]) = other match {
      case that: WkLkEdge[_] => this.weight == that.weight &&
                                this.label == that.label &&
                                baseEquals(that)
      case _ => false
    }
    override def hashCode =
      baseHashCode ^ (weight.hashCode) ^ label.hashCode
  }
  type  WkLkHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WLHyperEdgeBound[N,E] with WkLkEdge[N] 
  /** Everything common to weighted, key-labeled hyperedge companion objects. */
  trait WkLkHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkLkHyperEdgeBound[_,E]]
    extends WLHyperEdgeCompanion[E] 
  type  WkLkEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = WkLkHyperEdgeBound[N,E] with UnDiEdge[N]  
  /** Everything common to weighted, key-labeled edge companion objects. */
  trait WkLkEdgeCompanion[E[X] <: EdgeLikeIn[X] with WkLkEdgeBound[_,E]]
    extends WLEdgeCompanion[E] 
}
/** Base traits for custom edges - hence the `C` prefix. Note that custom edges need only
 *  to mix in `CBase.Attributes` when importing by `fromJson`.
 */
object CBase {
  /** To be mixed in by the custom edge class. */
  trait Attributes[N]
  { this: EdgeLike[N] =>
    /** To be set to `<companion object>.P`. */
    type P <: Product
    /** Must return an instance of `Tuple<n>` with `n` being the number of
     *  custom attributes. Custom attributes are defined by the constructor parameters
     *  excluding those representing nodes. */
    def attributes: P
  }
  type  CHyperEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = HyperEdge[N] with Attributes[N] with Serializable 
  trait CEdgeCompanionBase[E[X] <: EdgeLikeIn[X] with CHyperEdgeBound[_,E]]
    extends EdgeCompanionBase
  {
    protected[collection] type P <: Product
    @inline final def from [N](nodes: Product, attr: P): E[N] = newEdge[N](nodes, attr)
    /** Must return an instance of the custom edge which is achieved by projecting the elements
     *  of `attr` and passing them as single arguments to the custom edge constructor. */
    protected def newEdge[N](nodes: Product, attr: P): E[N] with EdgeCopy[E]
  }
  /** To be mixed in by custom hyperedge companion objects. */
  trait CHyperEdgeCompanion[E[X] <: EdgeLikeIn[X] with CHyperEdgeBound[_,E]]
    extends CEdgeCompanionBase[E]
  {
    @inline final def apply[N](node_1: N, node_2: N, nodes: N*)(attr: P): E[N] =
      newEdge[N](NodeProduct(node_1, node_2, nodes: _*), attr)
    @inline final def apply[N](nodes: Iterable[N], attr: P): E[N] =
      newEdge[N](NodeProduct(nodes), attr)
  }
  type  CEdgeBound[N, +E[X<:N] <: EdgeLikeIn[X]] = CHyperEdgeBound[N,E] with UnDiEdge[N] 
  /** To be mixed in by custom edge companion objects. */
  trait CEdgeCompanion[E[X] <: EdgeLikeIn[X] with CEdgeBound[_,E]]
    extends CEdgeCompanionBase[E] 
  {
    @inline final def apply[N](node_1: N, node_2: N, attr: P): E[N] =
      newEdge[N](NodeProduct(node_1, node_2), attr)
    @inline final def apply[N,A <: Product](nodes: Tuple2[N,N], attr: P) =
      newEdge[N](nodes, attr)
  }
}