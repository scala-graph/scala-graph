package scalax.collection.io.dot

/** Types for record-based nodes as described on
    [[http://www.graphviz.org/doc/info/shapes.html]].
  */
object Record {
  sealed trait RLabel
  case class Field(name: String, port: Option[Id] = None) extends RLabel {
    override def toString = port map (p => s"<$p> $name") getOrElse name.toString
  }
  case class Horizontal(left: RLabel, right: RLabel) extends RLabel {
    override def toString: String = s"$left | $right"
  }
  case class Vertical(left: RLabel, right: RLabel) extends RLabel {
    override def toString: String = s"{$left | $right}"
  }
  case class Label(part: RLabel) extends RLabel {
    override def toString = s""""$part""""
  }

  case class Ports(port_1: String, port_2: String)
}
