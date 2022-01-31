package scalax.collection.rdf

sealed trait RdfNode
trait Subject   extends RdfNode
trait Predicate extends RdfNode
trait Object    extends RdfNode

case class IRI(iri: String)     extends Subject with Predicate with Object
case class BlankNode(i: Int)    extends Subject with Object
case class Label(label: String) extends Object
