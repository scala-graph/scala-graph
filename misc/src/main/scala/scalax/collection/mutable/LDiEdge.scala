package scalax.collection
package mutable

import GraphPredef._, GraphEdge._
import edge.LBase._
import edge.LUnDiEdge

/** Labeled directed edge with mutable label. */
abstract class LDiEdge[N](nodes: Product)
    extends LUnDiEdge [N](nodes) 
       with DiEdgeLike[N]
       with OuterEdge [N,LDiEdge] {

  def label_= (newLabel: L1): this.type  
}

object LDiEdge extends LEdgeCompanion[LDiEdge] {
  @SerialVersionUID(9780L)
  override  def newEdge[N,L](nodes: Product, pLabel: L) =
    new  LDiEdge [N](nodes)
    with EdgeCopy[LDiEdge] { 
      type L1 = L
      private[this] var _label = pLabel
      override final def label = _label
      override final def label_= (newLabel: L): this.type = { _label = newLabel; this }  
      final override protected[collection]
      def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, _label)
    }
  
  final implicit class XEdgeAssoc[N1](n1: N1) {
    def ~+> [N >: N1, N2 <: N, L](n2: N2)(l: L) = LDiEdge[N,L](Tuple2[N,N](n1, n2))(l)
  }
}

private object test {
  import LDiEdge._
  val e = (1~+>2)("A")
  e.label = "B"
}