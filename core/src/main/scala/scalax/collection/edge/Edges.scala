package scalax.collection.edge

import scalax.collection.GraphEdge._,
       scalax.collection.GraphPredef._
import scalax.collection.Graph
import WBase._, LBase._

// ------------------------------------------------------------------------- W*
/** weighted undirected edge. */
@SerialVersionUID(72L)
class WUnDiEdge[N](nodes: Product,
                   override val weight: Long)
  extends UnDiEdge[N](nodes)
  with    WEdge   [N]
  with    EdgeCopy[WUnDiEdge]
  with    EdgeIn  [N,WUnDiEdge]
{
  override protected[collection]
  def copy[NN](newNodes: Product) = new WUnDiEdge[NN](newNodes, weight)
}
object WUnDiEdge extends WEdgeCompanion[WUnDiEdge] {
  override def newEdge[N](nodes: Product, weight: Long) = new WUnDiEdge[N](nodes, weight)
}
/** weighted directed edge. */
@SerialVersionUID(73L)
class WDiEdge[N](nodes: Product,
                 weight: Long)
  extends WUnDiEdge [N](nodes, weight)
  with    DiEdgeLike[N]
  with    EdgeCopy  [WDiEdge]
  with    EdgeIn    [N,WDiEdge]
{
  override protected[collection]
  def copy[NN](newNodes: Product) = new WDiEdge[NN](newNodes, weight)
}
object WDiEdge extends WEdgeCompanion[WDiEdge] {
  override def newEdge[N](nodes: Product, weight: Long) = new WDiEdge[N](nodes, weight)
}
// ------------------------------------------------------------------------ Wk*
import WkBase._
/** key-weighted undirected edge. */
abstract class WkUnDiEdge[N](nodes: Product, weight: Long)
  extends WUnDiEdge[N](nodes, weight)
  with    EdgeIn   [N,WkUnDiEdge]
  with    WkEdge   [N]
object WkUnDiEdge extends WkEdgeCompanion[WkUnDiEdge] {
  @SerialVersionUID(975L) override
  def newEdge[N](nodes: Product, weight: Long) =
    new  WkUnDiEdge[N](nodes, weight)
    with EdgeCopy [WkUnDiEdge] { 
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
}
/** key-weighted directed edge. */
abstract class WkDiEdge[N](nodes: Product, weight: Long)
  extends WkUnDiEdge[N](nodes, weight)
  with    DiEdgeLike[N]
  with    EdgeIn    [N,WkDiEdge]
object WkDiEdge extends WkEdgeCompanion[WkDiEdge] {
  @SerialVersionUID(976L) override
  def newEdge[N](nodes: Product, weight: Long) =
    new  WkDiEdge[N](nodes, weight)
    with EdgeCopy [WkDiEdge] { 
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
}
// ------------------------------------------------------------------------- L*
/** labeled undirected edge. */
abstract class LUnDiEdge[N](nodes: Product)
  extends UnDiEdge[N](nodes)
  with    EdgeIn  [N,LUnDiEdge]
  with    LEdge   [N]
object LUnDiEdge extends LEdgeCompanion[LUnDiEdge] {
  @SerialVersionUID(977L) override
  def newEdge[N,L](nodes: Product, pLabel: L) =
    new  LUnDiEdge[N](nodes)
    with EdgeCopy [LUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
    }
}
/** labeled directed edge. */
abstract class LDiEdge[N](nodes: Product)
  extends LUnDiEdge [N](nodes) 
  with    DiEdgeLike[N]
  with    EdgeIn    [N,LDiEdge]
object LDiEdge extends LEdgeCompanion[LDiEdge] {
  @SerialVersionUID(978L) override
  def newEdge[N,L](nodes: Product, pLabel: L) =
    new  LDiEdge [N](nodes)
    with EdgeCopy[LDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
    }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._
/** key-labeled undirected edge. */
abstract class LkUnDiEdge[N](nodes: Product)
  extends LUnDiEdge[N](nodes) 
  with    EdgeIn   [N,LkUnDiEdge]
  with    LkEdge   [N]
object LkUnDiEdge extends LkEdgeCompanion[LkUnDiEdge] {
  @SerialVersionUID(979L) override
  def newEdge[N,L](nodes: Product, pLabel: L) =
    new  LkUnDiEdge[N](nodes)
    with EdgeCopy  [LkUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
    }
}
/** key-labeled directed edge. */
abstract class LkDiEdge[N](nodes: Product)
  extends LDiEdge[N](nodes) 
  with    EdgeIn [N,LkDiEdge]
  with    LkEdge [N]
object LkDiEdge extends LkEdgeCompanion[LkDiEdge] {
  @SerialVersionUID(980L) override
  def newEdge[N,L](nodes: Product, pLabel: L) =
    new  LkDiEdge[N](nodes)
    with EdgeCopy[LkDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, pLabel)
    }
}
// ------------------------------------------------------------------------ WL*
import WLBase._
/** weighted, labeled undirected edge. */
abstract class WLUnDiEdge[N](nodes: Product, weight: Long)
  extends WUnDiEdge[N](nodes, weight)
  with    EdgeIn  [N,WLUnDiEdge]
  with    LEdge   [N]
  with    WLEdge  [N]
object WLUnDiEdge extends WLEdgeCompanion[WLUnDiEdge] {
  @SerialVersionUID(981L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WLUnDiEdge[N](nodes, weight)
    with EdgeCopy [WLUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
/** weighted, labeled directed edge. */
abstract class WLDiEdge[N](nodes: Product, weight: Long)
  extends WLUnDiEdge[N](nodes, weight) 
  with    DiEdgeLike[N]
  with    EdgeIn    [N,WLDiEdge]
object WLDiEdge extends WLEdgeCompanion[WLDiEdge] {
  @SerialVersionUID(982L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WLDiEdge [N](nodes, weight)
    with EdgeCopy[WLDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
// ----------------------------------------------------------------------- WkL*
import WkLBase._
/** key-weighted, labeled undirected edge. */
abstract class WkLUnDiEdge[N](nodes: Product, weight: Long)
  extends WLUnDiEdge[N](nodes, weight)
  with    WkEdge    [N]
  with    EdgeIn    [N,WkLUnDiEdge]
object WkLUnDiEdge extends WkLEdgeCompanion[WkLUnDiEdge] {
  @SerialVersionUID(983L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WkLUnDiEdge[N](nodes, weight)
    with EdgeCopy [WkLUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
/** key-weighted, labeled directed edge. */
abstract class WkLDiEdge[N](nodes: Product, weight: Long)
  extends WkLUnDiEdge[N](nodes, weight) 
  with    DiEdgeLike [N]
  with    EdgeIn     [N,WkLDiEdge]
object WkLDiEdge extends WkLEdgeCompanion[WkLDiEdge] {
  @SerialVersionUID(984L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WkLDiEdge [N](nodes, weight)
    with EdgeCopy[WkLDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
// ----------------------------------------------------------------------- WLk*
import WLkBase._
/** weighted, key-labeled undirected edge. */
abstract class WLkUnDiEdge[N](nodes: Product, weight: Long)
  extends WLUnDiEdge[N](nodes, weight) 
  with    EdgeIn   [N,WLkUnDiEdge]
  with    LkEdge   [N]
  with    WLkEdge  [N]
object WLkUnDiEdge extends WLkEdgeCompanion[WLkUnDiEdge] {
  @SerialVersionUID(985L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WLkUnDiEdge[N](nodes, weight)
    with EdgeCopy  [WLkUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
/** weighted, key-labeled directed edge. */
abstract class WLkDiEdge[N](nodes: Product, weight: Long)
  extends WLkUnDiEdge[N](nodes, weight) 
  with    DiEdgeLike [N]
  with    EdgeIn     [N,WLkDiEdge]
object WLkDiEdge extends WLkEdgeCompanion[WLkDiEdge] {
  @SerialVersionUID(986L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WLkDiEdge[N](nodes, weight)
    with EdgeCopy[WLkDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
// ---------------------------------------------------------------------- WkLk*
import WkLkBase._
/** key-weighted, key-labeled undirected edge. */
abstract class WkLkUnDiEdge[N](nodes: Product, weight: Long)
  extends WLUnDiEdge[N](nodes, weight) 
  with    EdgeIn    [N,WkLkUnDiEdge]
  with    WkLkEdge  [N]
object WkLkUnDiEdge extends WkLkEdgeCompanion[WkLkUnDiEdge] {
  @SerialVersionUID(987L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WkLkUnDiEdge[N](nodes, weight)
    with EdgeCopy  [WkLkUnDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
/** key-weighted, key-labeled directed edge. */
abstract class WkLkDiEdge[N](nodes: Product, weight: Long)
  extends WkLkUnDiEdge[N](nodes, weight) 
  with    DiEdgeLike  [N]
  with    EdgeIn      [N,WkLkDiEdge]
object WkLkDiEdge extends WkLkEdgeCompanion[WkLkDiEdge] {
  @SerialVersionUID(988L) override
  def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
    new  WkLkDiEdge[N](nodes, weight)
    with EdgeCopy[WkLkDiEdge] { 
      type L1 = L
      override val label = pLabel
      override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
    }
}
