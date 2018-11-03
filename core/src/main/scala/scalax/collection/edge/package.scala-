package scalax.collection

/** '''Predefined edges'''.
  *
  * While basic edge types are defined in the object `GraphEdge`,
  * the predefined edges in this package cover the following categories
  * (prefixes, shortcuts):
  *
  * weighted (`W, %`), key-weighted (`Wk, %#`),
  * labeled (`L, +`), key-labeled (`Lk, +#`),
  * weighted and labeled (`WL, %+`), key-weighted and labeled (`WkL, %#+`),
  * weighted and key-labeled (`WLk, %+#`) and key-weighted and key-labeled (`WkLk, %#+#`).
  *
  * These predefined edges provide alternatives for any edge extension
  * taking the burden from the user to implement his/her custom edge class - but baring the
  * disadvantage that user edge attributes must be part of a label class as opposed to
  * being part of the edge class directly.
  * It may also serve as a source for looking up how to implement custom edge classes.
  *
  * @author Peter Empen
  */
package object edge {
  import GraphEdge._
  import edge.CBase.Attributes

  type CEdge[X]      = UnDiEdge[X] with Attributes[X]
  type CHyperEdge[X] = HyperEdge[X] with Attributes[X]
}
