package scalax.collection.edge.labeled.mutable

import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge.{DiEdge, EdgeCopy, NodeProduct}
import scalax.collection.Graph

/** Type-safe directed labeled edge with a mutable label.
 *  @author Peter Empen
 */
class LDiEdge[N,@specialized L](nodes: Product)(private var _label: L)
    extends DiEdge[N](nodes)
    with    EdgeCopy [  ({type λ[α] = LDiEdge[α,L]})#λ]
    with    OuterEdge[N,({type λ[α] = LDiEdge[α,L]})#λ] {

  override def label: L = _label
  def label_=(newLabel: L) = _label = newLabel
  override def copy[NN](newNodes: Product) = new LDiEdge[NN,L](newNodes)(_label)

  override protected def attributesToString = s" '$label" 
}

object LDiEdge {
  
  def apply[N,L](from: N, to: N, label: L) =
    new LDiEdge[N,L](NodeProduct(from, to))(label)
    
  def unapply[N,L](e: LDiEdge[N,L]) = Some(e)
  
  final implicit class Assoc[N](val n1: N) extends AnyVal {
    def ~+>[L](n2: N)(label: L) = new LDiEdge[N,L]((n1, n2))(label)
  }
}

object :~> {
  def unapply[N,L](e: LDiEdge[N,L]): Option[(N, (N,L))] =
    if (e eq null) None else Some(e._1, (e._2, e.label))
}
object + {
  def unapply[N,L](nl: (N, L)): Option[(N, L)] =
    if (nl eq null) None else Some(nl._1, nl._2)
}

object DemoLDiEdge extends App {
  import LDiEdge._
  val outer = (1~+>2)(None: Option[List[Int]])
  outer.label = None

  outer match {
    case n1 :~> n2 + label => 
  }
  
  import scalax.collection.Graph
  type E[X] = LDiEdge[X,Option[List[Int]]]

  val g = Graph[Int,E](outer)
  val inner = g.edges.head

  print(s"Let's mutate [$inner]")
  inner.label = Some(List(1,2))
  println(s" to [$inner]")

  inner.edge match {
    case _ :~> _ + label => println(s"matched label = $label")
  }
}