package scalax.collection.io.dot

/** Predefined units of indentation. */
object Indent extends Enumeration {
  type Indent = Value
  val NoIndent, OneSpace, TwoSpaces, ThreeSpaces, FourSpaces, Tab = Value

  /** The String representation of a given `Indent`. */
  def apply(v: Value): String =
    if (v.id > 4) "\t"
    else new String(Array.fill[Char](v.id)(' '))
}

/** Template to define any DOT graph attribute separator. */
abstract class AttrSeparator(value: String) {
  override def toString = value
}

/** Holds predefined character sequences to separate specific DOT language elements. */
object AttrSeparator {

  /** The new line separator.*/
  object NewLine extends AttrSeparator(scala.sys.process.BasicIO.Newline)

  /** The separator `";"`.   */
  object Semicolon extends AttrSeparator(";")

  /** The separator `"; "`.  */
  object SemicolonSpace extends AttrSeparator("; ")
}

import Indent._, AttrSeparator._

/** @param indent unit of indentation to be used at the beginning of DOT graph lines.
  *        This unit is taken n times with n being the depths of the content represented
  *        on a given line. The depth increases with DOT subgraphs.
  * @param graphAttrSeparator the separator to be used for graph/subgraph level DOT graph
  *        language elements.
  * @param elemAttrSeparator the separator to be used for edge/node level DOT graph
  *        language elements.
  */
case class Spacing(indent: Indent = Tab,
                   graphAttrSeparator: AttrSeparator = NewLine,
                   elemAttrSeparator: AttrSeparator = SemicolonSpace)

/** [[scalax.collection.io.dot.Spacing]] with `indent` set to `Tab`,
  *  `graphAttrSeparator` set to `NewLine` and
  *  `elemAttrSeparator` set to `SemicolonSpace`.
  */
object DefaultSpacing extends Spacing(Tab, NewLine, SemicolonSpace)
