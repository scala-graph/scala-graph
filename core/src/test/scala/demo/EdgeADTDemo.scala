package demo

import scalax.collection.{One, OneOrMore}
import scalax.collection.generic.{AbstractDiEdge, AbstractUnDiEdge, AnyEdge, MultiEdge}
import scalax.collection.immutable.{Graph, TypedGraphFactory}

/** Demonstrates a Graph with nodes representing Persons and edges representing Relations between Persons.
  */
object EdgeADTDemo {
  // node type, which could also be an ADT
  case class Person(name: String)

  // edge ADT
  sealed trait Relation extends AnyEdge[Person]

  /** `abstract class`es facilitating more concise case class definitions.
    * `ExtendedKey` is needed to allow for multiple Relations between the same Persons.
    */
  sealed abstract protected class DiRelation(from: Person, to: Person, discriminator: AnyRef)
      extends AbstractDiEdge(from, to)
      with MultiEdge
      with Relation {
    def extendKeyBy: OneOrMore[Any] = One(discriminator)
  }
  sealed abstract protected class UnDiRelation(one: Person, another: Person, discriminator: AnyRef)
      extends AbstractUnDiEdge(one, another)
      with MultiEdge
      with Relation {
    def extendKeyBy: OneOrMore[Any] = One(discriminator)
  }

  final case class Parent(offspring: Person, parent: Person) extends DiRelation(offspring, parent, Parent)
  final case class Siblings(one: Person, another: Person)    extends UnDiRelation(one, another, Siblings)
  final case class Friends(one: Person, another: Person)     extends UnDiRelation(one, another, Friends)

  val kate = Person("Kate")
  val john = Person("John")
  val mike = Person("Mike")

  type People = Graph[Person, Relation]
  object People extends TypedGraphFactory[Person, Relation]

  // create empty Graph
  val empty = People.empty

  // populate Graph by Iterable[Relation]
  val people: People = People.from(
    List(
      Parent(kate, mike),
      Friends(kate, john),
      Siblings(kate, john)
    )
  )

  val edgesInWords =
    people.edges.outerIterator map {
      case Friends(Person(name_1), Person(name_2))   => s"$name_1 and $name_2 are friends"
      case Siblings(Person(name_1), Person(name_2))  => s"$name_1 and $name_2 are siblings"
      case Parent(Person(offspring), Person(parent)) => s"$parent is parent of $offspring"
    }

  // populate Graph by Iterable[People] and Iterable[Relation]
  val p1 = People.from(
    nodes = List(kate, john, mike),
    edges = List(Parent(kate, mike), Friends(kate, john))
  )

  // populate Graph by repeated Peoples and Relations
  import People.OuterImplicits._
  val p2 = People(kate, Parent(kate, mike), Friends(kate, john), Siblings(kate, john))
}
