package scalax.collection.concurrent

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.locks.ReentrantLock

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.reflect.ClassTag
import scala.util.{Success, Try}
import scala.util.chaining.given

import scalax.util.primitives.*
import scalax.util.primitives.PositiveLog2ValueOverNonNegative.*
import scalax.util.primitives.PositiveLog2ValueOverPositive.*
import ArrayTree.Config

/** Concurrent, growing only, compressed rose tree with leaves of type `Array[A]`.
  *
  * @param config allows for an optimal setup in terms of speed and memory usage.
  * @tparam A type of elements
  */
final class ArrayTree[A: ClassTag](config: Config):
  import config.*
  import ArrayTree.*

  @volatile private var _tree: Tree[A] | Null = null
  private val _size                           = AtomicInteger(0)

  /** The last, so probably not yet exhausted `Leaf` or `null`.
    * When extending the tree structure and updating shared values,
    * we always start with checking that this value was not changed concurrently.
    */
  @volatile private var _activeLeaf: Leaf[A] | Null = null

  /** Number of elements of type `A` in the tree <b>excluding</b> those in `_activeLeaf`. */
  @volatile private var _closedSize = TSize.zero

  /** Number of tree levels. `_tree`, the root node, is always at this level.
    * This redundant number, it could also be calculated as a function of `config` and `_size`,
    * is handy for index-based look-ups.
    */
  @volatile private var _levels = Level.zero

  private val _collisions = AtomicLong(0)

  private val treeSync = new ReentrantLock

  @volatile protected[concurrent] def tree: Tree[A] | Null = _tree

  def size: TSize = TSize.trust(_size.get)

  def collisions: Long = _collisions.get

  /** @throws `IndexOutOfBoundsException` if `index` is not less than `size`. */
  def apply(index: TIndex): A =
    if index < size then
      _tree match
        case upper: UpperNode[A] =>
          val (multi, subIndex, _) = leaf(upper, index)
          multi.elems(subIndex.value)
        case LeafParentNode(elems) =>
          val (slot, subIndex, _) = locate(Index.zero, index, leftSide = true)
          elems(slot.value).elems(subIndex.value)
        case MultiLeaf(elems, _) => elems(index.value)
        case SingleLeaf(elem)    => elem
    else throw new IndexOutOfBoundsException

  @tailrec infix def append(a: A): TIndex =
    treeSync.lock()
    val lastTree       = _tree
    val lastActiveLeaf = _activeLeaf
    val lastClosedSize = _closedSize
    treeSync.unlock()

    def updateState(activeLeaf: Leaf[A], tree: Option[Tree[A]], closedSize: TSize): Boolean =
      treeSync.lock()
      try
        if _activeLeaf eq lastActiveLeaf then
          _activeLeaf = activeLeaf
          tree foreach { (t: Tree[A]) =>
            _tree = t
            if t.isInstanceOf[Node[A]] then _levels = _levels.incr
          }
          _closedSize = closedSize
          if _size.incrementAndGet() > TSize.upperLimit then throw new LimitOverflowException
          true
        else
          _collisions.incrementAndGet()
          false
      finally
        treeSync.unlock()

    if lastActiveLeaf eq null then
      val leaf: Leaf[A] =
        if initialCap === 1 then SingleLeaf(a)
        else MultiLeaf(initialCap, leftNeighbor = null)(a)
      if updateState(leaf, Some(leaf), TSize.zero) then TIndex.zero
      else append(a)
    else
      lastActiveLeaf append a match
        case Exhausted =>
          lastActiveLeaf.extendTreeAndAppend(lastTree, leafCap, nodeCap, _closedSize, updateState)(a) match
            case Collision                                      => append(a)
            case idx: TIndex @unchecked /* must be last case */ => idx
        case idx: Index @unchecked /* must be last case */ =>
          if _size.incrementAndGet() > TSize.upperLimit then throw new LimitOverflowException
          lastClosedSize + idx
  end append

  def reverseIterator: Iterator[A] =
    _activeLeaf match
      case leaf: Leaf[A] if leaf.leftNeighbor eq null => leaf.reverseIterator
      case multi: MultiLeaf[A]                        => ReverseIterator(multi, _closedSize)
      case _                                          => Iterator.empty

  def reverseIteratorWithIndex: Iterator[(A, TIndex)] =
    _activeLeaf match
      case leaf: Leaf[A] if leaf.leftNeighbor eq null => leaf.reverseIteratorWithIndex
      case multi: MultiLeaf[A]                        => ReverseIteratorWithIndex(multi, _closedSize)
      case _                                          => Iterator.empty

  private def reverseIteratorImpl[B](
      from: TIndex,
      leafIterator: (Leaf[A], Index) => Iterator[B],
      leavesIterator: (MultiLeaf[A], TIndex, TSize) => Iterator[B]
  ): Iterator[B] =
    if from < size then
      _tree match
        case upper: UpperNode[A] =>
          val (multi, subIndex, leftSize) = leaf(upper, from)
          leavesIterator(multi, subIndex, leftSize)
        case LeafParentNode(elems) =>
          val (slot, subIndex, leftSize) = locate(Index.zero, from, leftSide = true)
          leavesIterator(elems(slot.value), subIndex, leftSize)
        case leaf: Leaf[A] => leafIterator(leaf, from)
        case null          => Iterator.empty
    else throw new IndexOutOfBoundsException

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def reverseIterator(from: TIndex): Iterator[A] =
    reverseIteratorImpl(from, _.reverseIterator(_), ReverseIterator.apply)

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def reverseIteratorWithIndex(from: TIndex): Iterator[(A, TIndex)] =
    reverseIteratorImpl(from, _.reverseIteratorWithIndex(_), ReverseIteratorWithIndex.apply)

  private def leaf(root: UpperNode[A], index: TIndex): (MultiLeaf[A], Index, TSize) =
    withLevelCaps(Positive.trust(_levels.value)) { height =>
      @tailrec def loop(
          capIndex: Index,
          leftSide: Boolean,
          node: Node[A],
          i: Index,
          leftSize: TSize
      ): (MultiLeaf[A], Index, TSize) =
        val (slot, subIndex, left) = locate(capIndex, i, leftSide)
        node match
          case UpperNode(elems) =>
            loop(capIndex.decr, leftSide && slot === 0, elems(slot.value), subIndex, leftSize + left)
          case LeafParentNode(elems) =>
            (elems(slot.value), subIndex, leftSize + left)

      loop(height.asNonNegative.decr, leftSide = true, root, index, TSize.zero)
    }

  protected[concurrent] def treeIterator: Iterator[Tree[A]] =
    treeIteratorWithLevel map (_._1)

  private def treeIteratorWithLevel: Iterator[(Tree[A], Level)] = _tree match
    case tree: Tree[A] =>
      val lastSize = size
      new AbstractIterator[(Tree[A], Level)]:
        private var consumedElems = TSize.zero
        private val stack         = Stack.empty[(Node[A], Index)]

        def hasNext: Boolean =
          def doneStillCheckForProperSize =
            if consumedElems < lastSize then
              // should never happen; println is preferable over Exception in test
              println(
                s"!!! mismatch detected in treeIteratorWithLevel: consumedElems=$consumedElems < size=$lastSize !!!"
              )
            false
          stack.nonEmpty || consumedElems == TSize.zero || doneStillCheckForProperSize

        def next(): (Tree[A], Level) =
          stack.headOption match
            case Some(UpperNode(elems) -> i) =>
              val node       = elems(i.value)
              val sizeBefore = stack.size
              stack push node -> Index.zero
              node            -> Level.trust(sizeBefore)
            case Some(LeafParentNode(elems) -> i) =>
              val leaf  = elems(i.value)
              val level = Level.trust(stack.size)
              consumedElems += leaf.size
              stack.popWhile { case node -> i =>
                i.incrTrusted == node.size
              }
              stack.headOption map { case node -> i =>
                stack.pop()
                stack push node -> i.incrTrusted
              }
              leaf -> level
            case None if hasNext => // first call of next()
              tree match
                case node: Node[A]         => stack push node -> Index.zero; node -> Level.zero
                case multi: MultiLeaf[A]   => consumedElems += multi.size; multi -> Level.zero
                case single: SingleLeaf[A] => consumedElems = consumedElems.incrTrusted; single -> Level.zero
            case None => throw new NoSuchElementException
    case null => Iterator.empty
  end treeIteratorWithLevel

  protected[concurrent] def prettifyTree(
      includeNodes: Boolean,
      marginSize: NonNegative = NonNegative(2),
      indentSize: Positive = Positive(2)
  ): String =
    val builder = new StringBuilder(8_192)
    val indent  = " ".repeat(indentSize.value)
    val margin  = " ".repeat(marginSize.value)

    infix def append(elem: String, level: Level): builder.type =
      builder append margin
      builder append indent.repeat(level.value)
      builder append elem
      builder append System.lineSeparator

    val it =
      if includeNodes then treeIteratorWithLevel
      else treeIteratorWithLevel.filter(_._1.isInstanceOf[Leaf[A]])

    it.foreach { case elem -> level => append(elem.toString, level) }
    builder.toString

object ArrayTree:
  /** Indicate that `ArrayTree`'s size is not necessarily limited to Int.
    * For the time being it's fine to limit support to Int, though.
    */
  type TSize  = Size; protected[concurrent] val TSize   = Size
  type TIndex = TSize; protected[concurrent] val TIndex = TSize

  type Capacity = Positive
  val Capacity = Positive

  type Log2Capacity = PositiveLog2Value
  val Log2Capacity = PositiveLog2Value

  type Level = NonNegative
  val Level = NonNegative

  def of[A: ClassTag](expectedSize20thPercentile: Positive, expectedSize90thPercentile: Positive) =
    // TODO
    new ArrayTree[A](???)

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
    protected[ArrayTree] infix def append(a: A): Index | Exhausted

    /** To be called after `append` has reported `Exhausted`. */
    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        leafCap: Log2Capacity,
        nodeCap: Log2Capacity,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision

    protected[ArrayTree] def leftNeighbor: MultiLeaf[A] | Null

    protected[ArrayTree] def reverseIterator: Iterator[A]
    protected[ArrayTree] def reverseIterator(from: Index): Iterator[A]

    protected[ArrayTree] def reverseIteratorWithIndex: Iterator[(A, Index)]
    protected[ArrayTree] def reverseIteratorWithIndex(from: Index): Iterator[(A, Index)]

  final protected[concurrent] case class SingleLeaf[A: ClassTag](elem: A) extends Leaf[A]:
    def capacity: Size = Size(1)
    def size: Size     = Size(1)

    protected[ArrayTree] infix def append(a: A): Index | Exhausted = Exhausted

    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        leafCap: Log2Capacity,
        nodeCap: Log2Capacity,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision =
      val leaf = MultiLeaf(leafCap.asPositive, leftNeighbor = null)(elem)
      leaf append a
      if updateState(leaf, Some(leaf), TSize.zero) then TIndex(1)
      else Collision

    protected[ArrayTree] inline def reverseIterator: Iterator[A] = Iterator(elem)

    protected[ArrayTree] inline def reverseIterator(from: Index): Iterator[A] =
      assert(from == Index.zero)
      reverseIterator

    protected[ArrayTree] inline def reverseIteratorWithIndex: Iterator[(A, Index)] =
      reverseIterator.zip(Iterator(Index.zero))

    protected[ArrayTree] inline def reverseIteratorWithIndex(from: Index): Iterator[(A, Index)] =
      assert(from == Index.zero)
      reverseIteratorWithIndex

    protected[ArrayTree] def leftNeighbor: MultiLeaf[A] | Null = null

  sealed protected[concurrent] trait Many[A] extends Tree[A]:
    type E
    protected[concurrent] def elems: Array[E]
    protected[concurrent] def last: E = elems(_used.get - 1)

    final protected val _used = AtomicInteger(0)

    final def capacity: Size = Size.trust(elems.length)
    final def size: Size     = Size.trust(_used.get)

    /** To be overridden when comparing array elements or other fields. */
    protected def equalFields(that: Many[?]): Boolean = true

    /** Non-traversing equality. Specifically, node references are not compared. */
    final override def equals(other: Any): Boolean = other match
      case many: Many[?] =>
        this.getClass == many.getClass &&
        this.size == many.size &&
        equalFields(many)
      case _ => false

    /** To be overridden when comparing array elements or other fields. */
    protected def fieldsHashCode: Int = size.hashCode

    final override def hashCode: Int = fieldsHashCode

    final protected def commonToString: String = s"used $size of $capacity elems"

    /** To be overridden when elements are to be included. */
    override def toString: String = s"${getClass.getSimpleName}($commonToString)"

    /** Appends `elem` to `this` if there is free space.
      *
      * @return the index where `elem` was inserted, or `Exhausted` if there was no free space.
      */
    @tailrec final protected[ArrayTree] infix def append(elem: E): Index | Exhausted =
      val idx = _used.get
      if idx < elems.length then
        if _used.compareAndSet(idx, idx + 1) then
          elems(idx) = elem
          Index.trust(idx)
        else append(elem)
      else Exhausted

  final protected[concurrent] case class MultiLeaf[A: ClassTag] private (
      protected[concurrent] val elems: Array[A],
      protected[concurrent] val leftNeighbor: MultiLeaf[A] | Null
  ) extends Leaf[A]
      with Many[A]:
    type E = A

    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        leafCap: Log2Capacity,
        nodeCap: Log2Capacity,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision =
      def ensureNodeAndAppend(a: A): TIndex | Collision =
        /** Searches for the closest extendable predecessor of `exhausted`.
          * @return
          *   - either
          *     - `Right` containing an extendable predecessor `Node` or
          *     - `Left` containing the exhausted root
          *   - the distance of the above from `this` leaf.
          */
        def findExtendable(root: Tree[A], exhausted: Many[A]): (Either[Many[A], Node[A]], Size) =
          root match
            case r: Node[A] =>
              @tailrec def loop(
                  node: Node[A],
                  extendable: Option[Node[A]],
                  distance: Size
              ): (Either[Many[A], Node[A]], Size) =
                node match
                  case u: UpperNode[A] if u.exhausted      => loop(u.last, extendable, distance.incrTrusted)
                  case u: UpperNode[A]                     => loop(u.last, Some(u), Size(1))
                  case l: LeafParentNode[A] if l.exhausted =>
                    extendable.map(n => Right(n) -> distance.incrTrusted).getOrElse(Left(r) -> distance.incrTrusted)
                  case l: LeafParentNode[A] => Right(l) -> Size(1)

              loop(r, None, Size.zero)

            case l: Leaf[A] =>
              assert(root eq exhausted)
              Left(exhausted) -> Size.zero

        def newLeaf(a: A) = MultiLeaf(leafCap.asPositive, this)(a)

        def newNode(upper: Boolean): Node[A] =
          if upper then UpperNode.empty[A](nodeCap)
          else LeafParentNode.empty[A](nodeCap)

        val newClosedSize = closedSize + size
        findExtendable(tree, this) match
          case Left(exhaustedRoot) -> distance =>
            val (newPath, pathLeaf): (Node[A], MultiLeaf[A]) =
              @tailrec def loop(i: Size, parent: Node[A]): MultiLeaf[A] =
                parent match
                  case upper: UpperNode[A] =>
                    assert(i < distance)
                    val incr = i.incrTrusted
                    loop(incr, newNode(upper = incr < distance) tap upper.append)
                  case leafParent: LeafParentNode[A] =>
                    assert(i == distance)
                    newLeaf(a) tap leafParent.append

              val root: Node[A] =
                exhaustedRoot match
                  case exhaustedNode: Node[A] =>
                    UpperNode.empty[A](nodeCap) tap (_ append exhaustedNode)
                  case exhaustedLeaf: MultiLeaf[A] =>
                    LeafParentNode.empty[A](nodeCap) tap (_ append exhaustedLeaf)

              root -> loop(Size.zero, root)

            if updateState(pathLeaf, Some(newPath), newClosedSize) then newClosedSize
            else Collision

          case Right(extendable) -> distance =>
            val (newPath, pathLeaf): (Many[A], MultiLeaf[A]) =
              @tailrec def loop(i: Size, parent: Node[A] | Null, root: Option[Node[A]]): (Many[A], MultiLeaf[A]) =
                parent match
                  case upper: UpperNode[A] =>
                    assert(i < distance)
                    val incr = i.incrTrusted
                    val n    = newNode(upper = incr < distance)
                    if root.isDefined then upper append n
                    loop(i.incrTrusted, n, root orElse Some(n))
                  case leafParent: LeafParentNode[A] if leafParent ne extendable =>
                    assert(i == distance)
                    val multi = newLeaf(a)
                    leafParent append multi
                    (root getOrElse multi) -> multi
                  case leafParent: LeafParentNode[A] =>
                    val multi = newLeaf(a)
                    (root getOrElse multi) -> multi

              loop(Size(1), extendable, None)

            if updateState(pathLeaf, None, newClosedSize) then
              (extendable, newPath) match
                case (upper: UpperNode[A], node: Node[A])        => upper append node
                case (leafP: LeafParentNode[A], m: MultiLeaf[A]) => if leafP eq extendable then leafP append m
                case _                                           => assert(false, "unexpected type mismatch")
              newClosedSize
            else Collision
      end ensureNodeAndAppend

      ensureNodeAndAppend(a) match
        case Collision                                              => Collision
        case idx: TIndex @unchecked /* works only as second case */ => idx

    protected[ArrayTree] def reverseIterator: Iterator[A] =
      MultiLeaf.ReverseIterator(elems, _used.get - 1)

    protected[ArrayTree] def reverseIterator(from: Index): Iterator[A] =
      assert(from.value < _used.get)
      MultiLeaf.ReverseIterator(elems, from.value)

    protected[ArrayTree] inline def reverseIteratorWithIndex: Iterator[(A, Index)] =
      reverseIterator zip Size.trust(_used.get).reverseIndexes

    protected[ArrayTree] inline def reverseIteratorWithIndex(from: Index): Iterator[(A, Index)] =
      assert(from.value < _used.get)
      MultiLeaf.ReverseIterator(elems, from.value) zip from.incrTrusted.reverseIndexes

    override protected def equalFields(that: Many[?]): Boolean =
      unsafeWrapArray(this.elems) == unsafeWrapArray(that.elems) &&
        (that match
          case m: MultiLeaf[?] => (this.leftNeighbor eq null) == (m.leftNeighbor eq null)
          case _               => false)

    override protected def fieldsHashCode: Int =
      unsafeWrapArray(elems).hashCode *
        (if leftNeighbor eq null then 1 else 7)

    override def toString: String =
      val parentToString = (if leftNeighbor eq null then "No" else "Some") + " left neighbor"
      val elemsToString  = size.indexes.map(elems(_).toString) mkString ", "
      s"$MultiLeaf($parentToString, $commonToString: $elemsToString)"

  protected[concurrent] case object MultiLeaf:
    def empty[A: ClassTag](cap: Capacity, leftNeighbor: MultiLeaf[A] | Null): MultiLeaf[A] =
      new MultiLeaf[A](new Array(cap.value), leftNeighbor)

    def apply[A: ClassTag](cap: Capacity, leftNeighbor: MultiLeaf[A] | Null)(elems: A*): MultiLeaf[A] =
      empty[A](cap, leftNeighbor) tap (elems foreach _.append)

    private[MultiLeaf] class ReverseIterator[A](elems: Array[A], from: Int) extends AbstractIterator[A]:
      override val knownSize: Int = from + 1
      private var remaining       = knownSize

      inline def hasNext: Boolean = remaining > 0

      def next(): A =
        if hasNext then
          remaining -= 1
          elems(remaining)
        else throw new NoSuchElementException

  sealed protected[concurrent] trait Node[A] extends Many[A]

  final protected[concurrent] case class UpperNode[A] private (
      protected[concurrent] val elems: Array[Node[A]]
  ) extends Node[A]:
    type E = Node[A]

  protected[concurrent] case object UpperNode:
    def empty[A: ClassTag](cap: Log2Capacity): UpperNode[A] =
      new UpperNode[A](new Array[Node[A]](cap.asInt))

    def apply[A: ClassTag](cap: Log2Capacity)(elems: Node[A]*): UpperNode[A] =
      empty[A](cap) tap (elems foreach _.append)

  final protected[concurrent] case class LeafParentNode[A] private (
      protected[concurrent] val elems: Array[MultiLeaf[A]]
  ) extends Node[A]:
    type E = MultiLeaf[A]

  protected[concurrent] case object LeafParentNode:
    def empty[A: ClassTag](cap: Log2Capacity): LeafParentNode[A] =
      new LeafParentNode[A](new Array[MultiLeaf[A]](cap.asInt))

    def apply[A: ClassTag](cap: Log2Capacity)(elems: MultiLeaf[A]*): LeafParentNode[A] =
      empty[A](cap) tap (elems foreach _.append)

  abstract private class AbstractReverseIterator[A, B](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
      extends AbstractIterator[B]:
    private var currentLeaf              = from
    protected var currentIt: Iterator[A] = fromIt
    protected var index: TIndex          = total

    override val knownSize: Int = total.value

    protected def nextResult: B

    override def hasNext: Boolean =
      if currentIt.hasNext then true
      else
        currentLeaf.leftNeighbor match
          case multi: MultiLeaf[A] =>
            currentLeaf = multi
            currentIt = multi.reverseIterator
            currentIt.hasNext
          case null => false

    override def next(): B =
      index = index.decr
      if hasNext then nextResult
      else throw new NoSuchElementException

  private class ReverseIterator[A](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
      extends AbstractReverseIterator[A, A](from, fromIt, total):
    protected def nextResult: A = currentIt.next()

  private object ReverseIterator:
    private[ArrayTree] def apply[A](from: MultiLeaf[A], leftSize: TSize): ReverseIterator[A] =
      new ReverseIterator(from, from.reverseIterator, leftSize + from.size)

    private[ArrayTree] def apply[A](from: MultiLeaf[A], fromIndex: Index, leftSize: TSize): ReverseIterator[A] =
      new ReverseIterator(from, from.reverseIterator(fromIndex), (leftSize + fromIndex).incr)

  private class ReverseIteratorWithIndex[A](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
      extends AbstractReverseIterator[A, (A, TIndex)](from, fromIt, total):
    protected def nextResult: (A, TIndex) = currentIt.next() -> index

  private object ReverseIteratorWithIndex:
    private[ArrayTree] def apply[A](from: MultiLeaf[A], leftSize: TSize): ReverseIteratorWithIndex[A] =
      new ReverseIteratorWithIndex(from, from.reverseIterator, leftSize + from.size)

    private[ArrayTree] def apply[A](
        from: MultiLeaf[A],
        fromIndex: Index,
        leftSize: TSize
    ): ReverseIteratorWithIndex[A] =
      new ReverseIteratorWithIndex(from, from.reverseIterator(fromIndex), (leftSize + fromIndex).incr)

  /** Defines the expected dimensions of an `ArrayTree`.
    * It is meant to be reused for `ArrayTree` instances, so avoid unnecessarily allocating `Config`s.
    *
    *  In general, try to minimize the number of nodes. A higher number of nodes makes only sense if the collection
    * size at some high percentile is orders of magnitude greater than its size at some low percentile.
    *
    * Examples:
    *   - Given an evenly distributed size of roughly 1,000 to 10,000, you might opt for
    *     - `initialCapacity` = 2,500
    *     - `leafCapacity` = 4,096
    *     - `nodeCapacity` = 16
    *   - but with some concern about memory usage due to many instances or other constraints, change the above like
    *     - `initialCapacity` = 1,800
    *     - `leafCapacity` = 512
    *     - `nodeCapacity` = 32.
    *   - Given a broad distribution of sizes between 1,000 and 1,000,000,000, a good choice would be to set
    *     - `initialCapacity` = 5,000
    *     - `leafCapacity` = 2,048
    *     - `nodeCapacity` = 265.
    *
    * @param initialCap number of elements of type `A` to be allocated in the first leaf.
    *                   This should cover between 10th to 40th percentile of size distribution.
    *                   The more memory usage concerns, the lower percentile is adequate.
    *                   In case you expect lots of instances with zero or just one element,
    *                   you can also set it to 1 to save main memory.
    * @param leafCap the power of 2 number of elements, expressed by its log2 value, to be allocated
    *                for subsequent leaves.
    *                For small collections, at least 16 is recommended.
    *                For bigger collections, choose a higher value that also fits `nodeCapacity`.
    * @param nodeCap the power of 2 number of elements, expressed by its log2 value, to be allocated
    *                for non-leaf nodes.
    *                For small collections, at least 4 is recommended.
    *                For best efficiency, choose it such that the tree height probably won't exceed 8.
    */
  final case class Config(initialCap: Capacity, leafCap: Log2Capacity, nodeCap: Log2Capacity):
    import Config.*
    given Log2Capacity = nodeCap

    /** Buffer with precalculated `LevelCap`s to support tree look-ups by index.
      * 8 levels are calculated in advance. Further levels are added on demand.
      * Index n corresponds to the tree height with a distance of n + 1 from the bottom, leaf level.
      */
    protected[concurrent] val levelCaps: ArrayBuffer[LevelCap] =
      populateLevelCaps(
        LevelCap(if initialCap > leafCap.asPositive then initialCap else leafCap.asPositive, leafCap),
        new ArrayBuffer[LevelCap](8)
      )

    /** Add another 8 levels of `LevelCap` at most.
      * @return Whether any new levels could be added.
      */
    protected[concurrent] def extendLevelCaps(): Boolean = levelCaps.last match
      case full: LevelCap.Full =>
        populateLevelCaps(full.next, levelCaps)
        true
      case partial => false

    /** Add 8 levels of `LevelCap`. The number of levels added might be less if capacity is exhausted. */
    private def populateLevelCaps(caps: LevelCap, buf: ArrayBuffer[LevelCap]): ArrayBuffer[LevelCap] =
      @tailrec def loop(level: Int, caps: LevelCap): ArrayBuffer[LevelCap] =
        caps match
          case full @ LevelCap.Full(initial, subsequent, total) if level < 8 =>
            buf += full
            loop(level + 1, full.next)
          case partial: LevelCap.Partial if level < 8 =>
            buf += partial
            buf
          case _ => buf

      loop(0, caps)

    @tailrec private def ensureLevels(startHeight: Positive): Boolean =
      if startHeight.value <= levelCaps.size then true
      else if extendLevelCaps() then ensureLevels(startHeight)
      else false

    protected[concurrent] def withLevelCaps[R](height: Positive)(body: Positive => R): R =
      if ensureLevels(height) then body(height)
      else throw new IllegalArgumentException(s"Internal error: $height is too heigh for $this.")

    protected[concurrent] inline def locate(capIndex: Index, i: Index, leftSide: Boolean): Location =
      levelCaps(capIndex.value).locate(i, leftSide)

  object Config:
    private type Location = (slot: Index, subIndex: Index, leftSize: TSize)

    /** Capacities per tree level. */
    sealed protected[concurrent] trait LevelCap:
      def first: Capacity

      def locate[U](i: Index, leftSide: Boolean): Location

      protected def locate[U](i: Index, leftSide: Boolean, subsequent: Log2Capacity): Location =
        if leftSide then
          if i < first.asNonNegative then (Index.zero, i, TSize.zero)
          else
            val iSubsequent = i.mapTrusted(_ - first.value)
            val slot        = iSubsequent / subsequent
            val leftSize    = first.asNonNegative + slot * subsequent
            (Index(1) + slot, iSubsequent % subsequent, leftSize)
        else
          val slot = i / subsequent
          (slot, i % subsequent, slot *! subsequent)

    protected[concurrent] object LevelCap:

      /** Fully defined capacities for some tree height. The Capacities of the subtrees are cumulated.
        * @param first capacity of the first node
        * @param subsequent capacity of subsequent nodes
        * @param total the capacity of all nodes for the given height.
        */
      protected[concurrent] case class Full(first: Capacity, subsequent: Log2Capacity, total: Capacity)
          extends LevelCap:
        inline def locate[U](i: Index, leftSide: Boolean): Location =
          locate(i, leftSide, subsequent)

        def next(using nodeCap: Log2Capacity): LevelCap =
          Try(nodeCap * subsequent).map(newSubsequent =>
            (newSubsequent, Try(total + (nodeCap.asPositive.decrTrusted *! newSubsequent)))
          ) match
            case Success(newSubsequent, Success(newTotal)) => Full(total, newSubsequent, newTotal)
            case Success(newSubsequent, _)                 => Partial(total, Some(newSubsequent))
            case _                                         => Partial(total, None)

      /** Partially defined capacities of the highest possible tree level.
        * This copes with a numeric overflow of `Capacity` somewhere within this tree level.
        */
      protected[concurrent] case class Partial(first: Capacity, subsequent: Option[Log2Capacity]) extends LevelCap:
        def locate[U](i: Index, leftSide: Boolean): Location =
          subsequent match
            case Some(s) => locate(i, leftSide, s)
            case None    => (Index(1), i.mapTrusted(_ - first.value), first.asNonNegative)

      def apply(first: Capacity, subsequent: Log2Capacity)(using nodeCap: Log2Capacity): LevelCap =
        Try(first + (nodeCap.asPositive.decrTrusted *! subsequent)) match
          case Success(total) => Full(first, subsequent, total)
          case _              => Partial(first, Some(subsequent))
