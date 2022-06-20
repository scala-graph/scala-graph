package scalax.collection.generic

sealed protected[collection] trait Eq { this: Edge[_] =>
  protected def baseEquals(other: Edge[_]): Boolean
  protected def baseHashCode: Int

  override def equals(other: Any): Boolean = other match {
    case that: Edge[_] =>
      (this eq that) ||
      (that canEqual this) &&
      this.isDirected == that.isDirected &&
      this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed] &&
      equals(that)
    case _ => false
  }

  /** Preconditions:
    *  `this.directed == that.directed &&`
    *  `this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed]`
    */
  protected def equals(other: Edge[_]): Boolean = baseEquals(other)

  override def hashCode: Int = baseHashCode

}

protected[collection] object Eq {
  private def nrEqualingNodes(itA: Iterator[_], itB: Iterable[_]): Int = {
    var nr   = 0
    val bLen = itB.size
    val used = new Array[Boolean](bLen)
    for (a <- itA) {
      val bs = itB.iterator
      var j  = 0
      while (j < bLen) {
        val b = bs.next()
        if (!used(j) && a == b) {
          nr += 1
          used(j) = true
          j = bLen
        }
        j += 1
      }
    }
    nr
  }

  def equalEnds(
      left: Edge[_],
      leftEnds: Iterable[_],
      right: Edge[_],
      rightEnds: Iterable[_]
  ): Boolean = {
    val thisOrdered = left.isInstanceOf[OrderedEndpoints]
    val thatOrdered = right.isInstanceOf[OrderedEndpoints]
    thisOrdered == thatOrdered && (
      if (thisOrdered) leftEnds.toSeq == rightEnds.toSeq
      else Eq.nrEqualingNodes(leftEnds.iterator, rightEnds) == rightEnds.size
    )
  }
}

protected[collection] trait EqHyper extends Eq {
  this: AnyHyperEdge[_] =>

  override protected def baseEquals(other: Edge[_]): Boolean = {
    val (thisArity, thatArity) = (arity, other.arity)
    if (thisArity == thatArity)
      Eq.equalEnds(this, this.ends, other, other.ends)
    else false
  }

  override protected def baseHashCode: Int = ends.foldLeft(0)(_ ^ _.hashCode)
}

/** Equality for targets handled as a $BAG.
  *  Targets are equal if they contain the same nodes irrespective of their position.
  */
protected[collection] trait EqDiHyper extends Eq {
  this: AnyDiHyperEdge[_] =>

  override protected def baseEquals(other: Edge[_]): Boolean = {
    val (thisArity, thatArity) = (arity, other.arity)
    if (thisArity == thatArity)
      if (thisArity == 2)
        this.sources.head == other.sources.head &&
        this.targets.head == other.targets.head
      else
        other match {
          case diHyper: AnyDiHyperEdge[_] =>
            Eq.equalEnds(this, this.sources, diHyper, diHyper.sources) &&
            Eq.equalEnds(this, this.targets, diHyper, diHyper.targets)
          case _ => false
        }
    else false
  }

  override protected def baseHashCode: Int = {
    var m = 4

    def mul(i: Int): Int = { m += 3; m * i }
    ends.foldLeft(0)((s: Int, n: Any) => s ^ mul(n.hashCode))
  }
}

protected[collection] trait EqUnDi[+N] extends Eq {
  this: AnyUnDiEdge[N] =>

  @inline final protected def unDiBaseEquals(n1: Any, n2: Any): Boolean =
    this._1 == n1 && this._2 == n2 ||
      this._1 == n2 && this._2 == n1

  override protected def baseEquals(other: Edge[_]): Boolean = other match {
    case edge: AnyEdge[_] => unDiBaseEquals(edge._1, edge._2)
    case hyper: AnyHyperEdge[_] if hyper.isUndirected && hyper.arity == 2 =>
      unDiBaseEquals(hyper._n(0), hyper._n(1))
    case _ => false
  }

  override protected def baseHashCode: Int = _1.## ^ _2.##
}

protected[collection] trait EqDi[+N] extends Eq {
  this: AnyDiEdge[N] =>

  @inline final protected def diBaseEquals(n1: Any, n2: Any): Boolean =
    this._1 == n1 &&
      this._2 == n2

  final override protected def baseEquals(other: Edge[_]): Boolean = other match {
    case edge: AnyDiEdge[_]                           => diBaseEquals(edge._1, edge._2)
    case hyper: AnyDiHyperEdge[_] if hyper.arity == 2 => diBaseEquals(hyper.sources.head, hyper.targets.head)
    case _                                            => false
  }

  override protected def baseHashCode: Int = 23 * _1.## ^ _2.##
}

/** Defines how to handle the ends of hyperedges, or the source/target ends of directed hyperedges,
  *  with respect to equality.
  */
sealed abstract class CollectionKind(val duplicatesAllowed: Boolean, val orderSignificant: Boolean)

object CollectionKind {
  protected[collection] def from(duplicatesAllowed: Boolean, orderSignificant: Boolean): CollectionKind =
    if (duplicatesAllowed)
      if (orderSignificant) Sequence else Bag
    else
      throw new IllegalArgumentException("'duplicatesAllowed == false' is not supported for endpoints kind.")

  protected[collection] def from(s: String): CollectionKind =
    if (s == Bag.toString) Bag
    else if (s == Sequence.toString) Sequence
    else throw new IllegalArgumentException(s"Unexpected representation of '$s' for endpoints kind.")

  protected[collection] def from(edge: Edge[_]): CollectionKind =
    CollectionKind.from(duplicatesAllowed = true, orderSignificant = edge.isInstanceOf[OrderedEndpoints])

  def unapply(kind: CollectionKind): Option[(Boolean, Boolean)] =
    Some((kind.duplicatesAllowed, kind.orderSignificant))
}

/** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
  *  as an unordered collection of nodes with duplicates allowed.
  */
case object Bag extends CollectionKind(true, false)

/** Marks a hyperedge, $ORDIHYPER, to handle the endpoints
  *  as an ordered collection of nodes with duplicates allowed.
  */
case object Sequence extends CollectionKind(true, true)

/** Marks (directed) hyperedge endpoints to have a significant order. */
protected[collection] trait OrderedEndpoints

protected[collection] trait Keyed // TODO check usage

/** Mixin for multi-edge support.
  *
  * As a default, hashCode/equality of edges is determined by their participating nodes.
  * We also say that nodes are always part of the edge key.
  *
  * Whenever your custom edge needs be a multi-edge, meaning that your graph is allowed to have more than one edge
  * between the same nodes, the default equality needs be extended by adding further class members to the edge key.
  *
  * For example edges representing flight connections between airports need be multi-edges to allow for different
  * flights between the same airports.
  * For this purpose, mix in this trait and add some class member like a flight number to the edge key.
  *
  * @author Peter Empen
  */
trait ExtendedKey { this: Edge[_] =>

  /** Each element in this sequence references a class member of the custom edge that will be added to the edge key.
    * The edge ends, like `source` and `target`, are always part of the key so don't add them here.
    * Once you add class members to the edge key they automatically become part of the edge's hashCode/equality.
    */
  def extendKeyBy: Seq[Any]

  override def equals(other: Any): Boolean =
    super.equals(other) && (other match {
      case that: ExtendedKey => this.extendKeyBy == that.extendKeyBy
      case _                 => false
    })

  override def hashCode: Int = super.hashCode + extendKeyBy.map(_.## * 41).sum

  override protected def toStringPostfix: String
}
object ExtendedKey {
  def unapply(e: ExtendedKey) = Some(e)
}
