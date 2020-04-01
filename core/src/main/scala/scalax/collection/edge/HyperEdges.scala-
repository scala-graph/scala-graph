package scalax.collection.edge

import scalax.collection.GraphEdge._, scalax.collection.GraphPredef._
import WBase._, LBase._

// ------------------------------------------------------------------------- W*
/** weighted, undirected hyperedge. */
@SerialVersionUID(70L)
class WHyperEdge[+N](nodes: Product, override val weight: Double)
    extends HyperEdge[N](nodes)
    with WEdge[N]
    with EdgeCopy[WHyperEdge]
    with OuterEdge[N, WHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Product) =
    WHyperEdge.newEdge[NN](newNodes, weight)(CollectionKind.from(this))
}
object WHyperEdge extends WHyperEdgeCompanion[WHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double)(
      implicit endpointsKind: CollectionKind): WHyperEdge[N] =
    if (endpointsKind.orderSignificant) new WHyperEdge[N](nodes, weight) with OrderedEndpoints
    else new WHyperEdge[N](nodes, weight)
}

/** weighted directed hyperedge. */
@SerialVersionUID(71L)
class WDiHyperEdge[+N](nodes: Product, override val weight: Double)
    extends DiHyperEdge[N](nodes)
    with WEdge[N]
    with EdgeCopy[WDiHyperEdge]
    with OuterEdge[N, WDiHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Product) =
    WDiHyperEdge.newEdge[NN](newNodes, weight)(CollectionKind.from(this))
}
object WDiHyperEdge extends WHyperEdgeCompanion[WDiHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double)(
      implicit endpointsKind: CollectionKind): WDiHyperEdge[N] =
    if (endpointsKind.orderSignificant) new WDiHyperEdge[N](nodes, weight) with OrderedEndpoints
    else new WDiHyperEdge[N](nodes, weight)
}
// ------------------------------------------------------------------------ Wk*
import WkBase._

/** key-weighted undirected hyperedge. */
abstract class WkHyperEdge[+N](nodes: Product, weight: Double)
    extends WHyperEdge[N](nodes, weight)
    with OuterEdge[N, WkHyperEdge]
    with WkEdge[N]
object WkHyperEdge extends WkHyperEdgeCompanion[WkHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double)(
      implicit endpointsKind: CollectionKind): WkHyperEdge[N] with EdgeCopy[WkHyperEdge] = {
    class Wk[N](nodes: Product, weight: Double)(implicit endpointsKind: CollectionKind)
        extends WkHyperEdge[N](nodes, weight)
        with EdgeCopy[WkHyperEdge] {
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
    if (endpointsKind.orderSignificant) new Wk[N](nodes, weight) with OrderedEndpoints
    else new Wk[N](nodes, weight)
  }
}

/** key-weighted directed hyperedge. */
abstract class WkDiHyperEdge[+N](nodes: Product, weight: Double)
    extends WkHyperEdge[N](nodes, weight)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, WkDiHyperEdge]
object WkDiHyperEdge extends WkHyperEdgeCompanion[WkDiHyperEdge] {
  override protected def newEdge[N](nodes: Product, weight: Double)(
      implicit endpointsKind: CollectionKind): WkDiHyperEdge[N] with EdgeCopy[WkDiHyperEdge] = {
    class WkDi[N](nodes: Product, weight: Double)(implicit endpointsKind: CollectionKind)
        extends WkDiHyperEdge[N](nodes, weight)
        with EdgeCopy[WkDiHyperEdge] {
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
    }
    if (endpointsKind.orderSignificant) new WkDi[N](nodes, weight) with OrderedEndpoints
    else new WkDi[N](nodes, weight)
  }
}
// ------------------------------------------------------------------------- L*
/** labeled undirected hyperedge. */
abstract class LHyperEdge[+N](nodes: Product) extends HyperEdge[N](nodes) with OuterEdge[N, LHyperEdge] with LEdge[N]
object LHyperEdge extends LHyperEdgeCompanion[LHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L)(
      implicit endpointsKind: CollectionKind): LHyperEdge[N] with EdgeCopy[LHyperEdge] { type L1 = L } = {
    class LH[N](nodes: Product) extends LHyperEdge[N](nodes) with EdgeCopy[LHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
    if (endpointsKind.orderSignificant) new LH[N](nodes) with OrderedEndpoints
    else new LH[N](nodes)
  }
}

/** Labeled directed hyperedge. */
abstract class LDiHyperEdge[+N](nodes: Product)
    extends LHyperEdge[N](nodes)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, LDiHyperEdge]
object LDiHyperEdge extends LHyperEdgeCompanion[LDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L)(
      implicit endpointsKind: CollectionKind): LDiHyperEdge[N] with EdgeCopy[LDiHyperEdge] { type L1 = L } = {
    class LDi[N](nodes: Product) extends LDiHyperEdge[N](nodes) with EdgeCopy[LDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
    if (endpointsKind.orderSignificant) new LDi[N](nodes) with OrderedEndpoints
    else new LDi[N](nodes)
  }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._

/** key-labeled undirected hyperedge. */
abstract class LkHyperEdge[+N](nodes: Product)
    extends LHyperEdge[N](nodes)
    with OuterEdge[N, LkHyperEdge]
    with LkEdge[N]
object LkHyperEdge extends LkHyperEdgeCompanion[LkHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L)(
      implicit endpointsKind: CollectionKind): LkHyperEdge[N] with EdgeCopy[LkHyperEdge] { type L1 = L } = {
    class Lk[N](nodes: Product) extends LkHyperEdge[N](nodes) with EdgeCopy[LkHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
    if (endpointsKind.orderSignificant) new Lk[N](nodes) with OrderedEndpoints
    else new Lk[N](nodes)
  }
}

/** key-labeled directed hyperedge. */
abstract class LkDiHyperEdge[+N](nodes: Product)
    extends LDiHyperEdge[N](nodes)
    with OuterEdge[N, LkDiHyperEdge]
    with LkEdge[N]
object LkDiHyperEdge extends LkHyperEdgeCompanion[LkDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, pLabel: L)(
      implicit endpointsKind: CollectionKind): LkDiHyperEdge[N] with EdgeCopy[LkDiHyperEdge] { type L1 = L } = {
    class LkDi[N](nodes: Product) extends LkDiHyperEdge[N](nodes) with EdgeCopy[LkDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, pLabel)
    }
    if (endpointsKind.orderSignificant) new LkDi[N](nodes) with OrderedEndpoints
    else new LkDi[N](nodes)
  }
}
// ------------------------------------------------------------------------ WL*
import WLBase._

/** weighted, labeled undirected hyperedge. */
abstract class WLHyperEdge[+N](nodes: Product, weight: Double)
    extends WHyperEdge[N](nodes, weight)
    with OuterEdge[N, WLHyperEdge]
    with LEdge[N]
    with WLEdge[N]
object WLHyperEdge extends WLHyperEdgeCompanion[WLHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WLHyperEdge[N] with EdgeCopy[WLHyperEdge] = {
    class WL[N](nodes: Product, weight: Double) extends WLHyperEdge[N](nodes, weight) with EdgeCopy[WLHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WL[N](nodes, weight) with OrderedEndpoints
    else new WL[N](nodes, weight)
  }
}

/** weighted, labeled directed hyperedge. */
abstract class WLDiHyperEdge[+N](nodes: Product, weight: Double)
    extends WLHyperEdge[N](nodes, weight)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, WLDiHyperEdge]
object WLDiHyperEdge extends WLHyperEdgeCompanion[WLDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WLDiHyperEdge[N] with EdgeCopy[WLDiHyperEdge] = {
    class WLDi[N](nodes: Product, weight: Double) extends WLDiHyperEdge[N](nodes, weight) with EdgeCopy[WLDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WLDi[N](nodes, weight) with OrderedEndpoints
    else new WLDi[N](nodes, weight)
  }
}
// ----------------------------------------------------------------------- WkL*
import WkLBase._

/** key-weighted, labeled undirected hyperedge. */
abstract class WkLHyperEdge[+N](nodes: Product, weight: Double)
    extends WLHyperEdge[N](nodes, weight)
    with WkEdge[N]
    with OuterEdge[N, WkLHyperEdge]
object WkLHyperEdge extends WkLHyperEdgeCompanion[WkLHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WkLHyperEdge[N] with EdgeCopy[WkLHyperEdge] = {
    class WkL[N](nodes: Product, weight: Double) extends WkLHyperEdge[N](nodes, weight) with EdgeCopy[WkLHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WkL[N](nodes, weight) with OrderedEndpoints
    else new WkL[N](nodes, weight)
  }
}

/** key-weighted, labeled directed hyperedge. */
abstract class WkLDiHyperEdge[+N](nodes: Product, weight: Double)
    extends WkLHyperEdge[N](nodes, weight)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, WkLDiHyperEdge]
object WkLDiHyperEdge extends WkLHyperEdgeCompanion[WkLDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WkLDiHyperEdge[N] with EdgeCopy[WkLDiHyperEdge] = {
    class WkLDi[N](nodes: Product, weight: Double)
        extends WkLDiHyperEdge[N](nodes, weight)
        with EdgeCopy[WkLDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WkLDi[N](nodes, weight) with OrderedEndpoints
    else new WkLDi[N](nodes, weight)
  }
}
// ----------------------------------------------------------------------- WLk*
import WLkBase._

/** weighted, key-labeled undirected hyperedge. */
abstract class WLkHyperEdge[+N](nodes: Product, weight: Double)
    extends WLHyperEdge[N](nodes, weight)
    with OuterEdge[N, WLkHyperEdge]
    with LkEdge[N]
    with WLkEdge[N]
object WLkHyperEdge extends WLkHyperEdgeCompanion[WLkHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WLkHyperEdge[N] with EdgeCopy[WLkHyperEdge] = {
    class WLk[N](nodes: Product, weight: Double) extends WLkHyperEdge[N](nodes, weight) with EdgeCopy[WLkHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WLk[N](nodes, weight) with OrderedEndpoints
    else new WLk[N](nodes, weight)
  }
}

/** weighted, key-labeled directed hyperedge. */
abstract class WLkDiHyperEdge[+N](nodes: Product, weight: Double)
    extends WLkHyperEdge[N](nodes, weight)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, WLkDiHyperEdge]
object WLkDiHyperEdge extends WLkHyperEdgeCompanion[WLkDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WLkDiHyperEdge[N] with EdgeCopy[WLkDiHyperEdge] = {
    class WLkDi[N](nodes: Product, weight: Double)
        extends WLkDiHyperEdge[N](nodes, weight)
        with EdgeCopy[WLkDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WLkDi[N](nodes, weight) with OrderedEndpoints
    else new WLkDi[N](nodes, weight)
  }
}
// ---------------------------------------------------------------------- WkLk*
import WkLkBase._

/** key-weighted, key-labeled undirected hyperedge. */
abstract class WkLkHyperEdge[+N](nodes: Product, weight: Double)
    extends WLHyperEdge[N](nodes, weight)
    with OuterEdge[N, WkLkHyperEdge]
    with WkLkEdge[N]
object WkLkHyperEdge extends WkLkHyperEdgeCompanion[WkLkHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WkLkHyperEdge[N] with EdgeCopy[WkLkHyperEdge] = {
    class WkLk[N](nodes: Product, weight: Double) extends WkLkHyperEdge[N](nodes, weight) with EdgeCopy[WkLkHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WkLk[N](nodes, weight) with OrderedEndpoints
    else new WkLk[N](nodes, weight)
  }
}

/** key-weighted, key-labeled directed hyperedge. */
abstract class WkLkDiHyperEdge[+N](nodes: Product, weight: Double)
    extends WkLkHyperEdge[N](nodes, weight)
    with DiHyperEdgeLike[N]
    with OuterEdge[N, WkLkDiHyperEdge]
object WkLkDiHyperEdge extends WkLkHyperEdgeCompanion[WkLkDiHyperEdge] {
  override protected def newEdge[N, L](nodes: Product, weight: Double, pLabel: L)(
      implicit endpointsKind: CollectionKind): WkLkDiHyperEdge[N] with EdgeCopy[WkLkDiHyperEdge] = {
    class WkLkDi[N](nodes: Product, weight: Double)
        extends WkLkDiHyperEdge[N](nodes, weight)
        with EdgeCopy[WkLkDiHyperEdge] {
      type L1 = L
      override val label                                             = pLabel
      override protected[collection] def copy[NN](newNodes: Product) = newEdge[NN, L](newNodes, weight, pLabel)
    }
    if (endpointsKind.orderSignificant) new WkLkDi[N](nodes, weight) with OrderedEndpoints
    else new WkLkDi[N](nodes, weight)
  }
}
