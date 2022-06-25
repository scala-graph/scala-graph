package scalax.collection.generic

// TODO discard or reuse
trait Label[L] { this: Edge[_] =>
  def isLabeled: Boolean = true
  def label: L
}

object LEdge {
  def unapply[L](lEdge: Label[L]): Option[L] = Some(lEdge.label)
}
