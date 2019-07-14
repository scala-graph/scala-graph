package scalax.collection.edge

import scalax.collection.GraphEdge._, scalax.collection.GraphPredef._
import WBase._, LBase._

// ------------------------------------------------------------------------- W*
/** weighted undirected edge. */
@SerialVersionUID(72L)
class WUnDiEdge[+N](nodes: Product, override val weight: Double)
    extends UnDiEdge[N](nodes)
    with WEdge[N]
    with EdgeCopy[WUnDiEdge]
    with OuterEdge[N, WUnDiEdge] {
  override protected[collection] def copy[NN](newNodes: Product) = new WUnDiEdge[NN](newNodes, weight)
}
object WUnDiEdge extends WEdgeCompanion[WUnDiEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double) = new WUnDiEdge[N](nodes, weight)
}

/** weighted directed edge. */
@SerialVersionUID(73L)
class WDiEdge[+N](nodes: Product, weight: Double)
    extends WUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with EdgeCopy[WDiEdge]
    with OuterEdge[N, WDiEdge] {
  override protected[collection] def copy[NN](newNodes: Product) = new WDiEdge[NN](newNodes, weight)
}
object WDiEdge extends WEdgeCompanion[WDiEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double) = new WDiEdge[N](nodes, weight)
}
// ------------------------------------------------------------------------ Wk*
import WkBase._

/** key-weighted undirected edge. */
abstract class WkUnDiEdge[+N](nodes: Product, weight: Double)
    extends WUnDiEdge[N](nodes, weight)
    with OuterEdge[N, WkUnDiEdge]
    with WkEdge[N]
    with EqUnDi
object WkUnDiEdge extends WkEdgeCompanion[WkUnDiEdge] {
  @SerialVersionUID(975L) override protected def newEdge[N](nodes: Product, weight: Double) =
    new WkUnDiEdge[N](nodes, weight) with EdgeCopy[WkUnDiEdge] {
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
}

/** key-weighted directed edge. */
abstract class WkDiEdge[+N](nodes: Product, weight: Double)
    extends WkUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with OuterEdge[N, WkDiEdge]
    with EqDi
object WkDiEdge extends WkEdgeCompanion[WkDiEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double) =
    new WkDiEdge[N](nodes, weight) with EdgeCopy[WkDiEdge] {
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
}
// ------------------------------------------------------------------------- L*
/** labeled undirected edge. */
abstract class LUnDiEdge[+N](nodes: Product) extends UnDiEdge[N](nodes) with OuterEdge[N, LUnDiEdge] with LEdge[N]
object LUnDiEdge extends LEdgeCompanion[LUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L) =
    new LUnDiEdge[N](nodes) with EdgeCopy[LUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
}

/** labeled directed edge. */
abstract class LDiEdge[+N](nodes: Product) extends LUnDiEdge[N](nodes) with DiEdgeLike[N] with OuterEdge[N, LDiEdge]
object LDiEdge extends LEdgeCompanion[LDiEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L) =
    new LDiEdge[N](nodes) with EdgeCopy[LDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._

/** key-labeled undirected edge. */
abstract class LkUnDiEdge[+N](nodes: Product)
    extends LUnDiEdge[N](nodes)
    with OuterEdge[N, LkUnDiEdge]
    with LkEdge[N]
    with EqUnDi
object LkUnDiEdge extends LkEdgeCompanion[LkUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L) =
    new LkUnDiEdge[N](nodes) with EdgeCopy[LkUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
}

/** key-labeled directed edge. */
abstract class LkDiEdge[+N](nodes: Product)
    extends LDiEdge[N](nodes)
    with OuterEdge[N, LkDiEdge]
    with LkEdge[N]
    with EqDi
object LkDiEdge extends LkEdgeCompanion[LkDiEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L) =
    new LkDiEdge[N](nodes) with EdgeCopy[LkDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
}
// ------------------------------------------------------------------------ WL*
import WLBase._

/** weighted, labeled undirected edge. */
abstract class WLUnDiEdge[+N](nodes: Product, weight: Double)
    extends WUnDiEdge[N](nodes, weight)
    with OuterEdge[N, WLUnDiEdge]
    with LEdge[N]
    with WLEdge[N]
object WLUnDiEdge extends WLEdgeCompanion[WLUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WLUnDiEdge[N](nodes, weight) with EdgeCopy[WLUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}

/** weighted, labeled directed edge. */
abstract class WLDiEdge[+N](nodes: Product, weight: Double)
    extends WLUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with OuterEdge[N, WLDiEdge]
object WLDiEdge extends WLEdgeCompanion[WLDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WLDiEdge[N](nodes, weight) with EdgeCopy[WLDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}
// ----------------------------------------------------------------------- WkL*
import WkLBase._

/** key-weighted, labeled undirected edge. */
abstract class WkLUnDiEdge[+N](nodes: Product, weight: Double)
    extends WLUnDiEdge[N](nodes, weight)
    with WkEdge[N]
    with OuterEdge[N, WkLUnDiEdge]
    with EqUnDi
object WkLUnDiEdge extends WkLEdgeCompanion[WkLUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WkLUnDiEdge[N](nodes, weight) with EdgeCopy[WkLUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}

/** key-weighted, labeled directed edge. */
abstract class WkLDiEdge[+N](nodes: Product, weight: Double)
    extends WkLUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with OuterEdge[N, WkLDiEdge]
    with EqDi
object WkLDiEdge extends WkLEdgeCompanion[WkLDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WkLDiEdge[N](nodes, weight) with EdgeCopy[WkLDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}
// ----------------------------------------------------------------------- WLk*
import WLkBase._

/** weighted, key-labeled undirected edge. */
abstract class WLkUnDiEdge[+N](nodes: Product, weight: Double)
    extends WLUnDiEdge[N](nodes, weight)
    with OuterEdge[N, WLkUnDiEdge]
    with LkEdge[N]
    with WLkEdge[N]
    with EqUnDi
object WLkUnDiEdge extends WLkEdgeCompanion[WLkUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WLkUnDiEdge[N](nodes, weight) with EdgeCopy[WLkUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}

/** weighted, key-labeled directed edge. */
abstract class WLkDiEdge[+N](nodes: Product, weight: Double)
    extends WLkUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with OuterEdge[N, WLkDiEdge]
    with EqDi
object WLkDiEdge extends WLkEdgeCompanion[WLkDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WLkDiEdge[N](nodes, weight) with EdgeCopy[WLkDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}
// ---------------------------------------------------------------------- WkLk*
import WkLkBase._

/** key-weighted, key-labeled undirected edge. */
abstract class WkLkUnDiEdge[+N](nodes: Product, weight: Double)
    extends WLUnDiEdge[N](nodes, weight)
    with OuterEdge[N, WkLkUnDiEdge]
    with WkLkEdge[N]
    with EqUnDi
object WkLkUnDiEdge extends WkLkEdgeCompanion[WkLkUnDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WkLkUnDiEdge[N](nodes, weight) with EdgeCopy[WkLkUnDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}

/** key-weighted, key-labeled directed edge. */
abstract class WkLkDiEdge[+N](nodes: Product, weight: Double)
    extends WkLkUnDiEdge[N](nodes, weight)
    with DiEdgeLike[N]
    with OuterEdge[N, WkLkDiEdge]
    with EqDi
object WkLkDiEdge extends WkLkEdgeCompanion[WkLkDiEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L) =
    new WkLkDiEdge[N](nodes, weight) with EdgeCopy[WkLkDiEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
}
