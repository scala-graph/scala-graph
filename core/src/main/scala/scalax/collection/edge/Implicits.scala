package scalax.collection.edge

import language.implicitConversions

import scalax.collection.GraphEdge._

object Implicits {
  // ------------------------------------------------------------------------- W*
  final class WUnDiEdgeAssoc[N](e: UnDiEdge[N]) {
    def %(weight: Double) = new WUnDiEdge[N](e.nodes, weight)
  }
  implicit def edge2WUnDiEdgeAssoc[N](e: UnDiEdge[N]) = new WUnDiEdgeAssoc[N](e)

  final class WDiEdgeAssoc[N](e: DiEdgeLike[N]) {
    def %(weight: Double) = new WDiEdge[N](e.nodes, weight)
  }
  implicit def edge2WDiEdgeAssoc[N](e: DiEdge[N]) = new WDiEdgeAssoc[N](e)
  // Overload resolution should choose this type instead of the UnDiEdge conversion because it's narrower
  implicit def wEdge2WDiEdgeAssoc[N](e: WDiEdge[N]) = new WDiEdgeAssoc[N](e)

  final class WHyperEdgeAssoc[N](e: HyperEdge[N]) {
    def %(weight: Double) = new WHyperEdge[N](e.nodes, weight)
  }
  implicit def edge2WHyperEdgeAssoc[N](e: HyperEdge[N]) = new WHyperEdgeAssoc[N](e)

  final class WDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) {
    def %(weight: Double) = new WDiHyperEdge[N](e.nodes, weight)
  }
  implicit def edge2WDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) = new WDiHyperEdgeAssoc[N](e)

  // ------------------------------------------------------------------------- L*
  final class LUnDiEdgeAssoc[N](e: UnDiEdge[N]) {
    def +[L](label: L) = LUnDiEdge.from[N, L](e.nodes)(label)
  }
  implicit def edge2LUnDiEdgeAssoc[N](e: UnDiEdge[N]) = new LUnDiEdgeAssoc[N](e)

  final class LDiEdgeAssoc[N](e: DiEdgeLike[N]) {
    def +[L](label: L) = LDiEdge.from[N, L](e.nodes)(label)
  }
  implicit def edge2LDiEdgeAssoc[N](e: DiEdge[N]) = new LDiEdgeAssoc[N](e)
  // Overload resolution should choose this type instead of the UnDiEdge conversion because it's narrower
  implicit def lEdge2LDiEdgeAssoc[N](e: LDiEdge[N]) = new LDiEdgeAssoc[N](e)

  final class LHyperEdgeAssoc[N](e: HyperEdge[N])(implicit kind: CollectionKind = Bag) {
    def +[L](label: L) = LHyperEdge.from[N, L](e.nodes)(label)
  }
  implicit def edge2LHyperEdgeAssoc[N](e: HyperEdge[N]) = new LHyperEdgeAssoc[N](e)

  final class LDiHyperEdgeAssoc[N](e: DiHyperEdge[N])(implicit kind: CollectionKind = Bag) {
    def +[L](label: L) = LDiHyperEdge.from[N, L](e.nodes)(label)
  }
  implicit def edge2LDiHyperEdgeAssoc[N](e: DiHyperEdge[N]) = new LDiHyperEdgeAssoc[N](e)

  // ----------------------------------------------------------------------- Edge
  final class XEdgeAssoc[N1](n1: N1) {
    def ~%[N >: N1, N2 <: N](n2: N2)(w: Double)             = new WUnDiEdge[N](Tuple2[N, N](n1, n2), w)
    def ~%#[N >: N1, N2 <: N](n2: N2)(w: Double)            = WkUnDiEdge[N](Tuple2[N, N](n1, n2))(w)
    def ~+[N >: N1, N2 <: N, L](n2: N2)(l: L)               = LUnDiEdge[N, L](Tuple2[N, N](n1, n2))(l)
    def ~+#[N >: N1, N2 <: N, L](n2: N2)(l: L)              = LkUnDiEdge[N, L](Tuple2[N, N](n1, n2))(l)
    def ~%+[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)   = WLUnDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%#+[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)  = WkLUnDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%+#[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)  = WLkUnDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%#+#[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L) = WkLkUnDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)

    def ~%>[N >: N1, N2 <: N](n2: N2)(w: Double)             = new WDiEdge[N](Tuple2[N, N](n1, n2), w)
    def ~%#>[N >: N1, N2 <: N](n2: N2)(w: Double)            = WkDiEdge[N](Tuple2[N, N](n1, n2))(w)
    def ~+>[N >: N1, N2 <: N, L](n2: N2)(l: L)               = LDiEdge[N, L](Tuple2[N, N](n1, n2))(l)
    def ~+#>[N >: N1, N2 <: N, L](n2: N2)(l: L)              = LkDiEdge[N, L](Tuple2[N, N](n1, n2))(l)
    def ~%+>[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)   = WLDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%#+>[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)  = WkLDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%+#>[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L)  = WLkDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
    def ~%#+#>[N >: N1, N2 <: N, L](n2: N2)(w: Double, l: L) = WkLkDiEdge[N, L](Tuple2[N, N](n1, n2))(w, l)
  }
  implicit def any2XEdgeAssoc[N1](n: N1) = new XEdgeAssoc(n)

  // ------------------------------------------------------------------ HyperEdge
  final class XHyperEdgeAssoc[NOld](e: EdgeLike[NOld]) {
    final private def product[N >: NOld](e: EdgeLike[NOld], n: N): Product = NodeProduct(e.iterator.toBuffer[N] += n)

    def ~%[N >: NOld](n: N)(w: Double)(implicit endpointsKind: CollectionKind = Bag) =
      WHyperEdge.from[N](product(e, n))(w)
    def ~%#[N >: NOld](n: N)(w: Double)(implicit endpointsKind: CollectionKind = Bag) =
      WkHyperEdge.from[N](product(e, n))(w)
    def ~+[N >: NOld, L](n: N)(l: L)(implicit endpointsKind: CollectionKind = Bag) =
      LHyperEdge.from[N, L](product(e, n))(l)
    def ~+#[N >: NOld, L](n: N)(l: L)(implicit endpointsKind: CollectionKind = Bag) =
      LkHyperEdge.from[N, L](product(e, n))(l)
    def ~%+[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WLHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%#+[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WkLHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%+#[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WLkHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%#+#[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WkLkHyperEdge.from[N, L](product(e, n))(w, l)

    def ~%>[N >: NOld](n: N)(w: Double)(implicit targetsKind: CollectionKind = Bag) =
      WDiHyperEdge.from[N](product(e, n))(w)
    def ~%#>[N >: NOld](n: N)(w: Double)(implicit targetsKind: CollectionKind = Bag) =
      WkDiHyperEdge.from[N](product(e, n))(w)
    def ~+>[N >: NOld, L](n: N)(l: L)(implicit endpointsKind: CollectionKind = Bag) =
      LDiHyperEdge.from[N, L](product(e, n))(l)
    def ~+#>[N >: NOld, L](n: N)(l: L)(implicit endpointsKind: CollectionKind = Bag) =
      LkDiHyperEdge.from[N, L](product(e, n))(l)
    def ~%+>[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WLDiHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%#+>[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WkLDiHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%+#>[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WLkDiHyperEdge.from[N, L](product(e, n))(w, l)
    def ~%#+#>[N >: NOld, L](n: N)(w: Double, l: L)(implicit endpointsKind: CollectionKind = Bag) =
      WkLkDiHyperEdge.from[N, L](product(e, n))(w, l)
  }
  implicit def edge2XHyperEdgeAssoc[NOld](e: EdgeLike[NOld]) = new XHyperEdgeAssoc(e)

  // ------------------------------------------------------------------ extractors
  /** Extractors for weighted and/or labeled undirected edges.
      {{{
      object StringLabel extends LEdgeImplicits[String]
      import StringLabel._

      val lE = (n1 ~+ n2)(label)
      lE match { case LUnDiEdge(s, t, l) => f(s, t, l) } // constructor pattern
      lE match { case s :~ t + l         => f(s, t, l) } // infix op pattern

      val lkE = (n1 ~+# n2)(label)
      lkE match { case s :~ t + l => f(s, t, l) }
      }}}
    */
  object :~ {
    def unapply[N](e: WUnDiEdge[N]): Option[(N, (N, Double))] =
      if (e eq null) None else Some(e._1, (e._2, e.weight))

    def unapply[N](e: WkUnDiEdge[N]): Option[(N, (N, Double))] = unapply(e: WUnDiEdge[N])

    def unapply[N](e: LUnDiEdge[N]): Option[(N, (N, LUnDiEdge[N]#L1))] =
      if (e eq null) None else Some(e._1, (e._2, e.label))

    def unapply[N](e: LkUnDiEdge[N]): Option[(N, (N, LUnDiEdge[N]#L1))] = unapply(e: LUnDiEdge[N])

    def unapply[N](e: WLUnDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))] =
      if (e eq null) None else Some(e._1, (e._2, e.weight, e.label))

    def unapply[N](e: WLkUnDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))]  = unapply(e: WLUnDiEdge[N])
    def unapply[N](e: WkLUnDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))]  = unapply(e: WLUnDiEdge[N])
    def unapply[N](e: WkLkUnDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))] = unapply(e: WLUnDiEdge[N])
  }

  /** Extractors for weighted and/or labeled directed edges.
      {{{
      object StringLabel extends LEdgeImplicits[String]
      import StringLabel._

      val lDi = (n1 ~+> n2)(label)
      lDi match { case LDiEdge(s, t, l) => f(s, t, l) } // constructor pattern
      lDi match { case s :~> t + l      => f(s, t, l) } // infix op pattern

      val lkDi = (n1 ~+#> n2)(label)
      lkDi match { case s :~> t + l     => f(s, t, l) }
      }}}
    */
  object :~> {
    def unapply[N](e: WDiEdge[N]): Option[(N, (N, Double))]  = :~ unapply e
    def unapply[N](e: WkDiEdge[N]): Option[(N, (N, Double))] = :~ unapply (e: WkUnDiEdge[N])

    def unapply[N](e: LDiEdge[N]): Option[(N, (N, LUnDiEdge[N]#L1))]  = :~ unapply e
    def unapply[N](e: LkDiEdge[N]): Option[(N, (N, LUnDiEdge[N]#L1))] = :~ unapply (e: LUnDiEdge[N])

    def unapply[N](e: WLDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))] = :~ unapply e

    def unapply[N](e: WLkDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))]  = :~ unapply (e: WLUnDiEdge[N])
    def unapply[N](e: WkLDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))]  = :~ unapply (e: WLUnDiEdge[N])
    def unapply[N](e: WkLkDiEdge[N]): Option[(N, (N, Double, WLUnDiEdge[N]#L1))] = :~ unapply (e: WLUnDiEdge[N])
  }

  /** Weight extractor to be combined with `:~` or `:~>`. */
  object % {
    def unapply[N](nw: (N, Double)): Option[(N, Double)] =
      if (nw eq null) None else Some(nw._1, nw._2)
  }

  /** Label extractor to be combined with `:~` or `:~>`. */
  object + {
    def unapply[N, L](nl: (N, L)): Option[(N, L)] =
      if (nl eq null) None else Some(nl._1, nl._2)
  }

  /** Weight and label extractor to be combined with `:~` or `:~>`. */
  object %+ {
    def unapply[N, L](nwl: (N, Double, L)): Option[(N, Double, L)] =
      if (nwl eq null) None else Some(nwl._1, nwl._2, nwl._3)
  }
}
