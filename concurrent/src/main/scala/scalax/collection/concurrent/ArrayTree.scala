package scalax.collection.concurrent

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.collection.mutable.Stack
import scala.reflect.ClassTag
import scala.util.chaining.given

import scalax.util.primitives.*

/** Concurrent, growing only, compressed rose tree with leaves of type `Array[A]`.
  *
  * For best efficiency, try to minimize the number of nodes. A high number of nodes is only needed
  * if the expected maximum size of the collection is orders of magnitude greater than the expected minimum size.
  *
  * Examples:
  *   - given an expected size of 1,000 to 10,000 in most cases, you may opt for
  *     - `initialCapacity` = 1,000
  *     - `leafCapacity` = 1,000
  *     - `nodeCapacity` = 20
  *   - given an expected size of 1,000 to 1,000,000,000, a good choice is to set
  *     - `initialCapacity` = 5,000
  *     - `leafCapacity` = 10,000
  *     - `nodeCapacity` = 1,000.
  *
  * @param initialCapacity number of elements of type `A` to be allocated for the first leaf.
  *                        This should cover about 10th to 20th percentile.
  * @param leafCapacity number of elements of type `A` to be allocated for subsequent leaves.
  *                     For best efficiency, choose it to be large like in the thousands.
  *                     For small collections, at least 8 is recommended.
  * @param nodeCapacity number of elements to be allocated for nodes, 2 at least.
  *                     For best efficiency, choose a high capacity, 16 at least.
  *                     For small collections, at least 4 is recommended.
  * @tparam A type of elements
  */
final class ArrayTree[A: ClassTag](
    val initialCapacity: PositiveSize,
    val leafCapacity: PositiveSize,
    val nodeCapacity: PositiveSize
):
  import ArrayTree.*

  @volatile private var _tree: Tree[A] | Null = null
  private val _size                           = AtomicInteger(0)

  /** The last, so probably not yet exhausted `Leaf` or `null`.
    * When extending the tree structure and updating shared values,
    * we always start with checking that this value was not changed concurrently.
    */
  @volatile private var _activeLeaf: Leaf[A] | Null = null

  /** Number of elements of type `A` in the tree <b>excluding</b> those in `_activeLeaf`.
    */
  @volatile private var _closedSize = Size.zero

  @volatile protected[concurrent] def tree: Tree[A] | Null = _tree

  def size: Size = Size.trust(_size.get)

  def capacity: Size = _activeLeaf match
    case leaf: Leaf[A] => _closedSize + leaf.capacity
    case null          => Size.zero

  // TODO def apply(i: Index): A

  private val _collisions = AtomicLong(0)
  def collisions: Long    = _collisions.get

  private val treeSync = new ReentrantLock

  @tailrec infix def append(a: A): Index =
    treeSync.lock()
    val lastActiveLeaf = _activeLeaf
    val lastClosedSize = _closedSize
    treeSync.unlock()

    def updateState(activeLeaf: Leaf[A], tree: Option[Tree[A]], closedSize: Size): Boolean =
      treeSync.lock()
      try
        if _activeLeaf eq lastActiveLeaf then
          _activeLeaf = activeLeaf
          tree foreach ((t: Tree[A]) => _tree = t)
          _closedSize = closedSize
          if _size.incrementAndGet() > Size.upperLimit then throw LimitOverflowException
          true
        else
          _collisions.incrementAndGet()
          false
      finally
        treeSync.unlock()

    if lastActiveLeaf eq null then
      val leaf: Leaf[A] =
        if initialCapacity === 1 then SingleLeaf(a)
        else MultiLeaf(initialCapacity, parent = null)(a)
      if updateState(leaf, Some(leaf), Size.zero) then Index.zero
      else append(a)
    else
      lastActiveLeaf append a match
        case Exhausted =>
          lastActiveLeaf.extendTreeAndAppend(leafCapacity, nodeCapacity, _closedSize, updateState)(a) match
            case Collision                                     => append(a)
            case idx: Index @unchecked /* must be last case */ => idx
        case idx: IntIndex @unchecked /* must be last case */ =>
          if _size.incrementAndGet() > Size.upperLimit then throw LimitOverflowException
          lastClosedSize + idx
  end append

  protected[concurrent] def treeIterator: Iterator[Tree[A]] =
    treeIteratorWithLevel map (_._1)

  protected[concurrent] def treeIteratorWithLevel: Iterator[(Tree[A], Int)] = _tree match
    case tree: Tree[A] =>
      val lastSize = size
      new AbstractIterator[(Tree[A], Int)]:
        private var consumedElems = Size.zero
        private val stack         = Stack.empty[(Node[A], IntIndex)]

        def hasNext: Boolean = consumedElems < lastSize

        def next(): (Tree[A], Int) =
          stack.headOption match
            case Some(UpperNode(elems, _) -> i) =>
              val node = elems(i.value)
              stack push node -> IntIndex.zero
              node            -> (stack.size - 1)
            case Some(LeafParentNode(elems, _) -> i) =>
              val leaf  = elems(i.value)
              val level = stack.size
              consumedElems += leaf.size
              stack.popWhile { case node -> i =>
                i.incr == node.size
              }
              stack.headOption map { case node -> i =>
                stack.pop()
                stack push node -> i.incr
              }
              leaf -> level
            case None if hasNext => // first call of next()
              tree match
                case single: SingleLeaf[A] => consumedElems = consumedElems.incr; single -> 0
                case multi: MultiLeaf[A]   => consumedElems += multi.size; multi -> 0
                case node: Node[A]         => stack push node -> IntIndex.zero; node -> 0
            case None => throw new NoSuchElementException
    case null => Iterator.empty
  end treeIteratorWithLevel

  protected[concurrent] def prettifyTree(
      includeNodes: Boolean,
      marginSize: NonNegativeInt = NonNegativeInt(2),
      indentSize: Positive = Positive(2)
  ): String =
    val builder = new StringBuilder(8_192)
    val indent  = " ".repeat(indentSize.value)
    val margin  = " ".repeat(marginSize.value)

    infix def append(elem: String, level: Int): builder.type =
      builder append margin
      builder append indent.repeat(level)
      builder append elem
      builder append System.lineSeparator

    val it =
      if includeNodes then treeIteratorWithLevel
      else treeIteratorWithLevel.filter(_._1.isInstanceOf[Leaf[A]])

    it.foldLeft(new StringBuilder(8_192 /* TODO */ )) { case (buf, elem -> level) =>
      append(elem.toString, level)
    }.toString()

object ArrayTree:
  /** Indicate that `Size` and `Index` are not necessarily limited to Int. */
  type Size  = IntSize; private val Size = IntSize
  type Index = Size; private val Index   = Size

  def of[A: ClassTag](expectedMinElements: IntSize, expectedMaxElements: PositiveSize) =
    // TODO
    new ArrayTree[A](???, ???, ???)

  private type Exhausted = -1; private val Exhausted: Exhausted = -1
  private type Collision = -2; private val Collision: Collision = -2

  sealed protected[concurrent] trait Tree[A]:
    def capacity: IntSize
    def size: IntSize
    final def exhausted: Boolean = size == capacity

  sealed protected[concurrent] trait Leaf[A] extends Tree[A]:
    /** Appends `a` to `this` if there is free space.
      *
      * @return the index where `a` was inserted, or `Exhausted` if there was no free space.
      */
    protected[ArrayTree] infix def append(a: A): IntIndex | Exhausted

    /** To be called after `append` has reported `Exhausted`. */
    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: Size,
        updateState: (Leaf[A], Option[Tree[A]], Size) => Boolean
    )(a: A): Index | Collision

  final protected[concurrent] case class SingleLeaf[A: ClassTag](elem: A) extends Leaf[A]:
    def capacity: IntSize = IntSize(1)
    def size: IntSize     = IntSize(1)

    protected[ArrayTree] infix def append(a: A): IntIndex | Exhausted = Exhausted

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: Size,
        updateState: (Leaf[A], Option[Tree[A]], Size) => Boolean
    )(a: A): Index | Collision =
      val leaf = MultiLeaf(leafCapacity, parent = null)(elem)
      leaf append a
      if updateState(leaf, Some(leaf), Size.zero) then Index(1)
      else Collision

  sealed protected[concurrent] trait Many[A] extends Tree[A]:
    type E
    protected[concurrent] def elems: Array[E]
    protected[concurrent] var parent: Node[A] | Null

    final protected val _used = AtomicInteger(0)

    final def capacity: IntSize = IntSize.trust(elems.length)
    final def size: IntSize     = IntSize.trust(_used.get)

    /** To be overridden when comparing element by element. */
    protected def equalElems[B](that: Many[?]): Boolean = true

    /** Non-traversing equality. Specifically, node references are not compared. */
    final override def equals(other: Any): Boolean = other match
      case many: Many[?] =>
        this.getClass == many.getClass &&
        (this.parent eq null) == (many.parent eq null) &&
        this.size == many.size &&
        equalElems(many)
      case _ => false

    /** To be overridden when comparing element by element. */
    protected def elemsHashCode: Int = size.hashCode

    final override def hashCode: Int =
      elemsHashCode * (if parent eq null then 1 else 7)

    final protected def commonToString: String =
      s"${if parent eq null then "no" else "some"} parent, used $size of $capacity elems"

    /** To be overridden in order to include elements. */
    override def toString: String = s"${getClass.getSimpleName}($commonToString)"

    /** Appends `elem` to `this` if there is free space.
      *
      * @return the index where `elem` was inserted, or `Exhausted` if there was no free space.
      */
    @tailrec final protected[ArrayTree] infix def append(elem: E): IntIndex | Exhausted =
      val idx = _used.get
      if idx < elems.length then
        if _used.compareAndSet(idx, idx + 1) then
          elems(idx) = elem
          IntIndex.trust(idx)
        else append(elem)
      else Exhausted

  final protected[concurrent] case class MultiLeaf[A: ClassTag] private (
      protected[concurrent] val elems: Array[A],
      protected[concurrent] var parent: Node[A] | Null
  ) extends Leaf[A]
      with Many[A]:
    type E = A

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: Size,
        updateState: (Leaf[A], Option[Tree[A]], Size) => Boolean
    )(a: A): Index | Collision =
      def ensureNodeAndAppend(a: A): Index | Collision =
        /** Searches for an extendable predecessor of `exhausted`.
          * @return
          *   - either
          *     - `Right` containing an extendable predecessor `Node` or
          *     - `Left` containing the exhausted root
          *   - the distance of the above from `this` leaf.
          */
        @tailrec def findExtendable(exhausted: Many[A], depth: IntSize): (Either[Many[A], Node[A]], IntSize) =
          exhausted.parent match
            case n: Node[A] if n.exhausted => findExtendable(n, depth.incr)
            case n: Node[A]                => Right(n)        -> depth.incr
            case null                      => Left(exhausted) -> depth

        def newLeaf(a: A, parent: Node[A]) = MultiLeaf(leafCapacity, parent)(a)

        def newNode(upper: Boolean, parent: Node[A] | Null): Node[A] =
          if upper then UpperNode.empty[A](nodeCapacity, parent)
          else LeafParentNode.empty[A](nodeCapacity, parent)

        val newClosedSize = closedSize + size
        findExtendable(this, depth = IntSize(0)) match
          case Left(exhaustedRoot) -> distance =>
            val (newPath, pathLeaf): (Node[A], MultiLeaf[A]) =
              @tailrec def loop(i: IntSize, parent: Node[A]): MultiLeaf[A] =
                parent match
                  case upper: UpperNode[A] =>
                    require(i < distance)
                    val incr = i.incr
                    loop(incr, newNode(upper = incr < distance, upper) tap upper.append)
                  case leafParent: LeafParentNode[A] =>
                    require(i == distance)
                    newLeaf(a, leafParent) tap leafParent.append

              val root: Node[A] =
                exhaustedRoot match
                  case exhaustedNode: Node[A] =>
                    UpperNode.empty[A](nodeCapacity, null) tap (_ append exhaustedNode)
                  case exhaustedLeaf: MultiLeaf[A] =>
                    LeafParentNode.empty[A](nodeCapacity, null) tap (_ append exhaustedLeaf)

              root -> loop(IntSize(0), root)

            if updateState(pathLeaf, Some(newPath), newClosedSize) then
              exhaustedRoot.parent = newPath
              newClosedSize
            else Collision

          case Right(extendable) -> distance =>
            val (newPath, pathLeaf): (Many[A], MultiLeaf[A]) =
              @tailrec def loop(i: IntSize, parent: Node[A] | Null, root: Option[Node[A]]): (Many[A], MultiLeaf[A]) =
                parent match
                  case upper: UpperNode[A] =>
                    require(i < distance)
                    val incr = i.incr
                    val n    = newNode(upper = incr < distance, upper)
                    if root.isDefined then upper append n
                    loop(i.incr, n, root orElse Some(n))
                  case leafParent: LeafParentNode[A] =>
                    assert(i == distance)
                    val l = newLeaf(a, leafParent)
                    leafParent append l
                    (root getOrElse l) -> l

              loop(IntSize(1), extendable, None)

            if updateState(pathLeaf, None, newClosedSize) then
              (extendable, newPath) match
                case (upper: UpperNode[A], node: Node[A])                => upper append node
                case (leafParent: LeafParentNode[A], leaf: MultiLeaf[A]) => leafParent append leaf
                case _                                                   => assert(false, "unexpected type mismatch")
              newClosedSize
            else Collision
      end ensureNodeAndAppend

      ensureNodeAndAppend(a) match
        case Collision                                             => Collision
        case idx: Index @unchecked /* works only as second case */ => idx

    override protected def equalElems[B](that: Many[?]): Boolean =
      unsafeWrapArray(this.elems) == unsafeWrapArray(that.elems)

    override protected def elemsHashCode: Int = unsafeWrapArray(elems).hashCode

    override def toString: String =
      s"$MultiLeaf($commonToString: ${size.indexIterator.map(elems(_).toString) mkString ", "})"

  protected[concurrent] case object MultiLeaf:
    def empty[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null): MultiLeaf[A] =
      new MultiLeaf[A](new Array(capacity.value), parent)

    def apply[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null)(elems: A*): MultiLeaf[A] =
      empty[A](capacity, parent) tap (elems foreach _.append)

  sealed protected[concurrent] trait Node[A] extends Many[A]

  final protected[concurrent] case class UpperNode[A] private (
      protected[concurrent] val elems: Array[Node[A]],
      protected[concurrent] var parent: Node[A] | Null
  ) extends Node[A]:
    type E = Node[A]

  protected[concurrent] case object UpperNode:
    def empty[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null): UpperNode[A] =
      new UpperNode[A](new Array[Node[A]](capacity.value), parent)

    def apply[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null)(elems: Node[A]*): UpperNode[A] =
      empty[A](capacity, parent) tap (elems foreach _.append)

  final protected[concurrent] case class LeafParentNode[A] private (
      protected[concurrent] val elems: Array[MultiLeaf[A]],
      protected[concurrent] var parent: Node[A] | Null
  ) extends Node[A]:
    type E = MultiLeaf[A]

  protected[concurrent] case object LeafParentNode:
    def empty[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null): LeafParentNode[A] =
      new LeafParentNode[A](new Array[MultiLeaf[A]](capacity.value), parent)

    def apply[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null)(
        elems: MultiLeaf[A]*
    ): LeafParentNode[A] =
      empty[A](capacity, parent) tap (elems foreach _.append)
