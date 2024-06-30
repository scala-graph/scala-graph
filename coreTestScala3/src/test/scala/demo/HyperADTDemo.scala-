package demo

import scalax.collection.OneOrMore
import scalax.collection.OneOrMore.one
import scalax.collection.generic.*
import scalax.collection.immutable.{Graph, TypedGraphFactory}

import scala.util.chaining.*

/** Demonstrates a largely typesafe Hypergraph for SQL schemas with
  *   - `Node`'s representing tables and columns and
  *   - `Connection`s representing
  *     - table-column relationships and
  *     - foreign keys.
  */
object HyperADTDemo extends App {
  sealed trait DataType
  case object StringType  extends DataType
  case object NumericType extends DataType

  sealed trait Node
  case class Table(name: String)                                  extends Node
  case class Column(table: Table, name: String, `type`: DataType) extends Node

  sealed trait Connection extends AnyDiHyperEdge[Node]

  /** Ensures by type safety that
    *   - columns cannot be connected to a table multiple times
    *   - primary key columns are an ordered subset of the table's columns
    *   - foreign key columns are an ordered subset of the table's columns
    *   - foreign keys are connected with the primary key of the child table
    * TODO try to also ensure that
    *   - only columns of the same table definition may be added to indexes/foreign keys
    *   - column names are unique with respect to the table
    *   - sets are not empty whenever appropriate
    *   - the # of foreign key columns must correspond to the # of primary key columns in the child table
    *   - `CS` of ForeignKey is different from `S`
    */
  final case class TableContext[S <: Set[Column] with Singleton](
      table: Table,
      columns: Superset[Column, S],
      primaryKeyColumns: OrderedSubset[Column, S]
  ) { outer =>

    case class TableColumn private[TableContext] (table: outer.table.type, column: Column)
        extends AbstractDiEdge(table: Table, column)
        with DiEdgeToString
        with Connection

    protected case class PrimaryKey private (table: outer.table.type, columns: OrderedSubset[Column, S])
        extends AbstractDiHyperEdge(one(table: Table), OneOrMore.fromUnsafe(columns))
        with MultiEdge
        with Connection {
      def extendKeyBy: OneOrMore[Any] = PrimaryKey.edgeKeyExtension
    }

    case object PrimaryKey {
      def apply(): PrimaryKey      = PrimaryKey(table, primaryKeyColumns)
      private val edgeKeyExtension = one(PrimaryKey.toString)
    }

    def columnEdges: List[TableColumn] =
      columns.set pipe (set =>
        set.iterator.map { column =>
          TableColumn(table, column)
        }.toList
      )

    def primaryKeyEdge: PrimaryKey = PrimaryKey()

    def requiredEdges: List[Connection] = primaryKeyEdge +: columnEdges

    case class Index[CS <: Set[Column] with Singleton] private (
        table: outer.table.type,
        columns: OrderedSubset[Column, S]
    ) extends AbstractDiHyperEdge(one(table: Table), OneOrMore.fromUnsafe(columns))
        with MultiEdge
        with Connection {
      def extendKeyBy: OneOrMore[Any] = Index.edgeKeyExtension
    }

    case object Index {
      def apply[CS <: Set[Column] with Singleton](
          columns: OrderedSubset[Column, S]
      ): Index[CS] =
        Index(table, columns)

      private val edgeKeyExtension = one(this.toString)
    }

    case class ForeignKey[CS <: Set[Column] with Singleton] private (
        table: outer.table.type,
        columns: OrderedSubset[Column, S],
        childTable: TableContext[CS]
    ) extends AbstractDiHyperEdge(one(table: Table), OneOrMore.fromUnsafe(columns))
        with MultiEdge
        with Connection {
      def extendKeyBy: OneOrMore[Any] = ForeignKey.edgeKeyExtension
    }

    case object ForeignKey {
      def apply[CS <: Set[Column] with Singleton](
          columns: OrderedSubset[Column, S],
          childTable: TableContext[CS]
      ): ForeignKey[CS] =
        ForeignKey(table, columns, childTable)

      private val edgeKeyExtension = one(this.toString)
    }
  }

  abstract class TableDef(val tableName: String) {
    final val table = Table(tableName)
    val columns: Set[Column]
    val primaryKeyColumns: OrderedSubset[Column, columns.type]

    final def column(name: String, `type`: DataType): Column = Column(table, name, `type`)
    final lazy val context: TableContext[columns.type]       = TableContext(table, Superset(columns), primaryKeyColumns)
    final def requiredEdges: List[Connection]                = context.requiredEdges

    final def index[C <: Set[Column] with Singleton](columns: Column*) =
      context.Index(OrderedSubset(this.columns)(columns: _*))

    final def foreignKey[C <: Set[Column] with Singleton](childTable: TableContext[C])(columns: Column*) =
      context.ForeignKey(OrderedSubset(this.columns)(columns: _*), childTable)
  }

  object Address extends TableDef("address") {
    val name              = column("name", StringType)
    val countryCode       = column("countryCode", StringType)
    val columns           = Set(name, countryCode)
    val primaryKeyColumns = OrderedSubset(columns)(name)
  }

  object Country extends TableDef("country") {
    val name              = column("name", StringType)
    val countryCode       = column("countryCode", StringType)
    val columns           = Set(name, countryCode)
    val primaryKeyColumns = OrderedSubset(columns)(countryCode)
  }

  type SqlSchema = Graph[Node, Connection]
  object SqlSchema extends TypedGraphFactory[Node, Connection]

  val schema = SqlSchema.from(
    Address.requiredEdges
      ++: Country.requiredEdges
      ++: List(
        Address.foreignKey(childTable = Country.context)(Address.countryCode),
        Country.index(Country.countryCode)
      )
  )

  // TODO some tasks based on `schema`
}
