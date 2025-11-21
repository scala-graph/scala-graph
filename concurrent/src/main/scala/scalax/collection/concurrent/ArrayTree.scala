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

/* TODO */
type LongSize = Size
val LongSize = Size
type LongIndex = Index
val LongIndex = Index

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

  @volatile private var _tree: Tree[A] | Null = _
  private val _size                           = AtomicLong(0)

  /** The last, so probably not yet exhausted `Leaf` or `null`.
    * When extending the tree structure and updating shared values,
    * we always start with checking that this value was not changed concurrently.
    */
  @volatile private var _activeLeaf: Leaf[A] | Null = _

  /** Number of elements of type `A` in the tree <b>excluding</b> those in `_activeLeaf`.
    */
  @volatile private var _closedSize = LongSize(0)

  @volatile protected[concurrent] def tree: Tree[A] | Null = _tree

  def size: LongSize = LongSize.unsafe(_size.get.toInt)

  def capacity: Size = _activeLeaf match
    case leaf: Leaf[A] => _closedSize + leaf.capacity
    case null          => Size(0)

  // TODO def apply(i: Index): A

  private val _collisions = AtomicLong(0)
  def collisions: Long    = _collisions.get

  private val treeSync = new ReentrantLock

  @tailrec def append(a: A): LongIndex =
    treeSync.lock()
    val lastActiveLeaf = _activeLeaf
    val lastClosedSize = _closedSize
    treeSync.unlock()

    def updateState(activeLeaf: Leaf[A], tree: Option[Tree[A]], closedSize: LongSize): Boolean =
      treeSync.lock()
      try
        if _activeLeaf eq lastActiveLeaf then
          _activeLeaf = activeLeaf
          tree foreach (_tree = _)
          _closedSize = closedSize
          _size.incrementAndGet()
          true
        else
          _collisions.incrementAndGet()
          false
      finally
        treeSync.unlock()

    if lastActiveLeaf eq null then
      val leaf: Leaf[A] =
        if initialCapacity === 1 then Single(a)
        else Multiple(initialCapacity, parent = null)(a)
      if updateState(leaf, Some(leaf), LongSize(0)) then LongIndex(0)
      else append(a)
    else
      lastActiveLeaf append a match
        case Exhausted =>
          lastActiveLeaf.extendTreeAndAppend(leafCapacity, nodeCapacity, _closedSize, updateState)(a) match
            case Collision                                         => append(a)
            case idx: LongIndex @unchecked /* must be last case */ => idx
        case idx: Index @unchecked /* must be last case */ =>
          _size.incrementAndGet()
          lastClosedSize + idx
  end append

  protected[concurrent] def treeIterator: Iterator[Tree[A]] = _tree match
    case tree: Tree[A] =>
      val lastSize = size
      new AbstractIterator[Tree[A]]:
        private var consumedElems = LongSize(0)
        private val stack         = Stack.empty[(Node[A], Index)]

        def hasNext: Boolean = consumedElems < lastSize

        def next(): Tree[A] =
          stack.headOption match
            case Some(Node(elems, _) -> i) =>
              elems(i.value) match
                case multi: Multiple[A] =>
                  consumedElems += multi.size
                  stack.popWhile { case node -> i =>
                    i.incr == node.size
                  }
                  stack.headOption map { case node -> i =>
                    stack.pop()
                    stack push node -> i.incr
                  }
                  multi
                case node: Node[A] =>
                  stack push node -> Index(0)
                  node
            case None if hasNext => // first call of next()
              tree match
                case single: Single[A]  => consumedElems = consumedElems.incr; single
                case multi: Multiple[A] => consumedElems += multi.size; multi
                case node: Node[A]      => stack push node -> Index(0); node
            case None => throw new NoSuchElementException
    case null => Iterator.empty
  end treeIterator

object ArrayTree:
  def of[A: ClassTag](expectedMinElements: Size, expectedMaxElements: PositiveSize) =
    // TODO
    new ArrayTree[A](???, ???, ???)

  private type Exhausted = -1; private val Exhausted: Exhausted = -1
  private type Collision = -2; private val Collision: Collision = -2

  sealed protected[concurrent] trait Tree[A]:
    def capacity: Size
    def size: Size
    final def exhausted: Boolean = size == capacity

  sealed protected[concurrent] trait Leaf[A] extends Tree[A]:
    /** Appends `a` to `this` if there is free space.
      *
      * @return the index where `a` was inserted, or `Exhausted` if there was no free space.
      */
    protected[ArrayTree] def append(a: A): Index | Exhausted

    /** To be called after `append` has reported `Exhausted`. */
    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: LongSize,
        updateState: (Leaf[A], Option[Tree[A]], LongSize) => Boolean
    )(a: A): LongIndex | Collision

  final protected[concurrent] case class Single[A: ClassTag](elem: A) extends Leaf[A]:
    def capacity: Size = Size(1)
    def size: Size     = Size(1)

    protected[ArrayTree] def append(a: A): Index | Exhausted = Exhausted

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: LongSize,
        updateState: (Leaf[A], Option[Tree[A]], LongSize) => Boolean
    )(a: A): LongIndex | Collision =
      val leaf = Multiple(leafCapacity, parent = null)(elem)
      leaf append a
      if updateState(leaf, Some(leaf), LongSize(0)) then LongIndex(1)
      else Collision

  sealed protected[concurrent] trait Many[A] extends Tree[A]:
    type E
    protected[concurrent] def elems: Array[E]
    protected[concurrent] var parent: Node[A] | Null

    final protected val _used = AtomicInteger(0)

    final def capacity: Size = Size.unsafe(elems.length)
    final def size: Size     = Size.unsafe(_used.get)

    protected def equalElems[B](that: Many[_]): Boolean

    final override def equals(other: Any): Boolean = other match
      case many: Many[_] =>
        (parent eq null) == (many.parent eq null) && equalElems(many)
      case _ => false

    protected def elemsHashCode: Int

    override def hashCode: Int =
      elemsHashCode * (if parent eq null then 1 else 7)

    final protected def commonToString: String =
      s"${if parent eq null then "no" else "some"} parent, used $size of $capacity elems"

    /** Appends `elem` to `this` if there is free space.
      *
      * @return the index where `elem` was inserted, or `Exhausted` if there was no free space.
      */
    @tailrec final protected[ArrayTree] def append(elem: E): Index | Exhausted =
      val idx = _used.get
      if idx < elems.length then
        if _used.compareAndSet(idx, idx + 1) then
          elems(idx) = elem
          Index.unsafe(idx)
        else append(elem)
      else Exhausted

  final protected[concurrent] case class Multiple[A: ClassTag] private (
      protected[concurrent] val elems: Array[A],
      protected[concurrent] var parent: Node[A] | Null
  ) extends Leaf[A]
      with Many[A]:
    type E = A

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: LongSize,
        updateState: (Leaf[A], Option[Tree[A]], LongSize) => Boolean
    )(a: A): LongIndex | Collision =
      def ensureNodeAndAppend(a: A): LongIndex | Collision =
        /** Searches for an extendable predecessor of `exhausted`.
          * @return
          *   - either
          *     - `Right` containing an extendable predecessor `Node` or
          *     - `Left` containing the exhausted root
          *   - the distance of the above from `this` leaf.
          */
        @tailrec def findExtendable(exhausted: Many[A], depth: Size): (Either[Many[A], Node[A]], Size) =
          exhausted.parent match
            case n: Node[A] if n.exhausted => findExtendable(n, depth.incr)
            case n: Node[A]                => Right(n)        -> depth.incr
            case null                      => Left(exhausted) -> depth

        def newLeaf(a: A, parent: Node[A]) = Multiple(leafCapacity, parent)(a)

        def newNode(parent: Node[A]) = Node.empty[A](nodeCapacity, parent)

        val newClosedSize = closedSize + size
        val newIndex      = LongIndex.unsafe(newClosedSize.value)
        findExtendable(this, depth = Size(0)) match
          case Left(exhaustedRoot) -> distance =>
            val (newPath, pathLeaf): (Node[A], Multiple[A]) =
              @tailrec def loop(i: Size, parent: Node[A] | Null): Multiple[A] =
                if i < distance then loop(i.incr, newNode(parent) tap parent.append)
                else newLeaf(a, parent) tap parent.append

              val root = newNode(null) tap (_ append exhaustedRoot)
              root -> loop(Size(0), root)

            if updateState(pathLeaf, Some(newPath), newClosedSize) then
              exhaustedRoot.parent = newPath
              newIndex
            else Collision

          case Right(extendable) -> distance =>
            val (newPath, pathLeaf): (Many[A], Multiple[A]) =
              @tailrec def loop(i: Size, parent: Node[A] | Null, root: Option[Node[A]]): (Many[A], Multiple[A]) =
                if i < distance then
                  val n = newNode(parent)
                  if root.isDefined then parent append n
                  loop(i.incr, n, root orElse Some(n))
                else
                  val l = newLeaf(a, parent)
                  parent append l
                  (root getOrElse l) -> l

              loop(Size(1), extendable, None)

            if updateState(pathLeaf, None, newClosedSize) then
              extendable append newPath
              newIndex
            else Collision
      end ensureNodeAndAppend

      ensureNodeAndAppend(a) match
        case Collision                                                 => Collision
        case idx: LongIndex @unchecked /* works only as second case */ => idx

    protected def equalElems[B](that: Many[_]): Boolean =
      unsafeWrapArray(this.elems) == unsafeWrapArray(that.elems)

    protected def elemsHashCode: Int = unsafeWrapArray(elems).hashCode

    override def toString: String =
      s"Multiple($commonToString: ${size.indexIterator.map(elems(_).toString) mkString ", "})"

  protected[concurrent] object Multiple:
    def empty[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null): Multiple[A] =
      new Multiple[A](new Array(capacity.value), parent)

    def apply[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null)(elems: A*): Multiple[A] =
      empty[A](capacity, parent) tap (elems foreach _.append)

  final protected[concurrent] case class Node[A] private (
      protected[concurrent] val elems: Array[Many[A]],
      protected[concurrent] var parent: Node[A] | Null
  ) extends Many[A]:
    type E = Many[A]

    protected def equalElems[B](that: Many[_]): Boolean =
      val _size = size

      def elemsOfSameType: Boolean =
        if _size === 0 then true
        else
          // provided elems of a given instance are always of the same class
          this.elems(0).getClass == that.elems(0).getClass

      _size == that.size && elemsOfSameType

    protected def elemsHashCode: Int =
      if size === 0 then 1
      else elems(0).getClass.getSimpleName.hashCode

    override def toString: String =
      val prefix = s"Node($commonToString)"
      if size === 0 then prefix
      else s"$prefix of type ${elems(0).getClass.getSimpleName}"

  protected[concurrent] object Node:
    def empty[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null): Node[A] =
      new Node[A](new Array[Many[A]](capacity.value), parent)

    def apply[A: ClassTag](capacity: PositiveSize, parent: Node[A] | Null)(elems: Many[A]*): Node[A] =
      empty[A](capacity, parent) tap (elems foreach _.append)
