package scalax.collection.edge

import scalax.collection.GraphEdge._

object Implicits {
  // ------------------------------------------------------------------------- W*
  final class WUnDiEdgeAssoc[N](e: UnDiEdge[N]) {
    def % (weight: Long) = new WUnDiEdge[N](e.nodes, weight)
  }
  implicit def edge2WUnDiEdgeAssoc[N](e: UnDiEdge[N]) = new WUnDiEdgeAssoc[N](e)

  final class WDiEdgeAssoc[N](e: DiEdge[N]) {
    def % (weight: Long) = new WDiEdge[N](e.nodes, weight)
  }
  implicit def edge2WDiEdgeAssoc[N](e: DiEdge[N]) = new WDiEdgeAssoc[N](e)

  final class WHyperEdgeAssoc[N](e: HyperEdge[N]) {
    def % (weight: Long) = new WHyperEdge[N](e.nodes, weight)
  }
  implicit def edge2WHyperEdgeAssoc[N](e: HyperEdge[N]) = new WHyperEdgeAssoc[N](e)

  final class WDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) {
    def % (weight: Long) = new WDiHyperEdge[N](e.nodes, weight)
  }
  implicit def edge2WDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) = new WDiHyperEdgeAssoc[N](e)

  // ------------------------------------------------------------------------- L*
  final class LUnDiEdgeAssoc[N](e: UnDiEdge[N]) {
    def + [L](label: L) = LUnDiEdge[N,L](e.nodes)(label)
  }
  implicit def edge2LUnDiEdgeAssoc[N](e: UnDiEdge[N]) = new LUnDiEdgeAssoc[N](e)

  final class LDiEdgeAssoc[N](e: DiEdge[N]) {
    def + [L](label: L) = LDiEdge[N,L](e.nodes)(label)
  }
  implicit def edge2LDiEdgeAssoc[N](e: DiEdge[N]) = new LDiEdgeAssoc[N](e)

  final class LHyperEdgeAssoc[N](e: HyperEdge[N]) {
    def + [L](label: L) = LHyperEdge.from[N,L](e.nodes)(label)
  }
  implicit def edge2LHyperEdgeAssoc[N](e: HyperEdge[N]) = new LHyperEdgeAssoc[N](e)

  final class LDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) {
    def + [L](label: L) = LDiHyperEdge.from[N,L](e.nodes)(label)
  }
  implicit def edge2LDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) = new LDiHyperEdgeAssoc[N](e)

  // ----------------------------------------------------------------------- Edge
  final class XEdgeAssoc[N1](n1: N1) {
    def ~%    [N >: N1, N2 <: N](n2: N2)(w: Long) = new WUnDiEdge[N](Tuple2[N,N](n1, n2), w)
    def ~%#   [N >: N1, N2 <: N](n2: N2)(w: Long) =    WkUnDiEdge[N](Tuple2[N,N](n1, n2))(w)
    def ~+    [N >: N1, N2 <: N, L](n2: N2)(l: L) =     LUnDiEdge[N,L](Tuple2[N,N](n1, n2))(l)
    def ~+#   [N >: N1, N2 <: N, L](n2: N2)(l: L) =    LkUnDiEdge[N,L](Tuple2[N,N](n1, n2))(l)
    def ~%+   [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =   WLUnDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%#+  [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =  WkLUnDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%+#  [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =  WLkUnDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%#+# [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) = WkLkUnDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    
    def ~%>    [N >: N1, N2 <: N](n2: N2)(w: Long) = new WDiEdge[N](Tuple2[N,N](n1, n2), w)
    def ~%#>   [N >: N1, N2 <: N](n2: N2)(w: Long) =    WkDiEdge[N](Tuple2[N,N](n1, n2))(w)
    def ~+>    [N >: N1, N2 <: N, L](n2: N2)(l: L) =     LDiEdge[N,L](Tuple2[N,N](n1, n2))(l)
    def ~+#>   [N >: N1, N2 <: N, L](n2: N2)(l: L) =    LkDiEdge[N,L](Tuple2[N,N](n1, n2))(l)
    def ~%+>   [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =   WLDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%#+>  [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =  WkLDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%+#>  [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) =  WLkDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
    def ~%#+#> [N >: N1, N2 <: N, L](n2: N2)(w: Long, l: L) = WkLkDiEdge[N,L](Tuple2[N,N](n1, n2))(w, l)
  }
  implicit def any2XEdgeAssoc[N1](n: N1) = new XEdgeAssoc(n)
  
  // ------------------------------------------------------------------ HyperEdge
  final class XHyperEdgeAssoc[NOld](e: EdgeLike[NOld]) {
    def ~%    [N >: NOld, NX <: N](n: NX)(w: Long) = new WHyperEdge[N](NodeProduct(e.iterator.toBuffer[N] += n), w)
    def ~%#   [N >: NOld, NX <: N](n: NX)(w: Long) =    WkHyperEdge[N](e.iterator.toBuffer[N] += n)(w)
    def ~+    [N >: NOld, NX <: N, L](n: NX)(l: L) =     LHyperEdge[N,L](e.iterator.toBuffer[N] += n)(l)
    def ~+#   [N >: NOld, NX <: N, L](n: NX)(l: L) =    LkHyperEdge[N,L](e.iterator.toBuffer[N] += n)(l)
    def ~%+   [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =   WLHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%#+  [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =  WkLHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%+#  [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =  WLkHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%#+# [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) = WkLkHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    
    def ~%>    [N >: NOld, NX <: N](n: NX)(w: Long) = new WDiHyperEdge[N](NodeProduct(e.iterator.toBuffer[N] += n), w)
    def ~%#>   [N >: NOld, NX <: N](n: NX)(w: Long) =    WkDiHyperEdge[N](e.iterator.toBuffer[N] += n)(w)
    def ~+>    [N >: NOld, NX <: N, L](n: NX)(l: L) =     LDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(l)
    def ~+#>   [N >: NOld, NX <: N, L](n: NX)(l: L) =    LkDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(l)
    def ~%+>   [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =   WLDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%#+>  [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =  WkLDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%+#>  [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) =  WLkDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
    def ~%#+#> [N >: NOld, NX <: N, L](n: NX)(w: Long, l: L) = WkLkDiHyperEdge[N,L](e.iterator.toBuffer[N] += n)(w, l)
  }
  implicit def edge2XHyperEdgeAssoc[NOld](e: EdgeLike[NOld]) = new XHyperEdgeAssoc(e)
}