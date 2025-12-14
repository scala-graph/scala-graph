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
  @volatile private var _levels = Size.zero

  @volatile protected[concurrent] def tree: Tree[A] | Null = _tree

  def size: TSize = TSize.trust(_size.get)

  def capacity: TSize = _activeLeaf match
    case leaf: Leaf[A] => _closedSize + leaf.capacity
    case null          => TSize.zero

  /** @throws `IndexOutOfBoundsException` if `index` is not less than `size`. */
  def apply(index: TIndex): A =
    if index < size then
      import config.{withLevelCaps, slotAndSubIndex}
      _tree match
        case upper: UpperNode[A] =>
          withLevelCaps(Positive.trust(_levels.value)) {
            @tailrec def loop(capIndex: Index, leftSide: Boolean, node: Node[A], i: Index): A =
              val (slot, subIndex) = slotAndSubIndex(capIndex, i, leftSide)
              node match
                case UpperNode(elems, _) =>
                  loop(capIndex.decr, leftSide && slot === 0, elems(slot.value), subIndex)
                case LeafParentNode(elems, _) =>
                  elems(slot.value).elems(subIndex.value)

            loop(_levels.decr, leftSide = true, upper, index)
          }
        case LeafParentNode(elems, _) =>
          val (slot, subIndex) = slotAndSubIndex(Index.zero, index, leftSide = true)
          elems(slot.value).elems(subIndex.value)
        case MultiLeaf(elems, _) => elems(index.value)
        case SingleLeaf(elem)    => elem
    else throw new IndexOutOfBoundsException

  private val _collisions = AtomicLong(0)
  def collisions: Long    = _collisions.get

  private val treeSync = new ReentrantLock

  @tailrec infix def append(a: A): TIndex =
    treeSync.lock()
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
        if initialCapacity === 1 then SingleLeaf(a)
        else MultiLeaf(initialCapacity, parent = null)(a)
      if updateState(leaf, Some(leaf), TSize.zero) then TIndex.zero
      else append(a)
    else
      lastActiveLeaf append a match
        case Exhausted =>
          lastActiveLeaf.extendTreeAndAppend(leafCapacity, nodeCapacity, _closedSize, updateState)(a) match
            case Collision                                      => append(a)
            case idx: TIndex @unchecked /* must be last case */ => idx
        case idx: Index @unchecked /* must be last case */ =>
          if _size.incrementAndGet() > TSize.upperLimit then throw new LimitOverflowException
          lastClosedSize + idx
  end append

  protected[concurrent] def treeIterator: Iterator[Tree[A]] =
    treeIteratorWithLevel map (_._1)

  protected[concurrent] def treeIteratorWithLevel: Iterator[(Tree[A], Int)] = _tree match
    case tree: Tree[A] =>
      val lastSize = size
      new AbstractIterator[(Tree[A], Int)]:
        private var consumedElems = TSize.zero
        private val stack         = Stack.empty[(Node[A], Index)]

        def hasNext: Boolean = consumedElems < lastSize

        def next(): (Tree[A], Int) =
          stack.headOption match
            case Some(UpperNode(elems, _) -> i) =>
              val node = elems(i.value)
              stack push node -> Index.zero
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
                case node: Node[A]         => stack push node -> Index.zero; node -> 0
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

    infix def append(elem: String, level: Int): builder.type =
      builder append margin
      builder append indent.repeat(level)
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

  def of[A: ClassTag](expectedSize20Percentile: Size, expectedSize90percentile: PositiveSize) =
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
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision

  final protected[concurrent] case class SingleLeaf[A: ClassTag](elem: A) extends Leaf[A]:
    def capacity: Size = Size(1)
    def size: Size     = Size(1)

    protected[ArrayTree] infix def append(a: A): Index | Exhausted = Exhausted

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision =
      val leaf = MultiLeaf(leafCapacity, parent = null)(elem)
      leaf append a
      if updateState(leaf, Some(leaf), TSize.zero) then TIndex(1)
      else Collision

  sealed protected[concurrent] trait Many[A] extends Tree[A]:
    type E
    protected[concurrent] def elems: Array[E]
    // TODO drop redundant `parent`
    protected[concurrent] var parent: Node[A] | Null

    final protected val _used = AtomicInteger(0)

    final def capacity: Size = Size.trust(elems.length)
    final def size: Size     = Size.trust(_used.get)

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
      protected[concurrent] var parent: Node[A] | Null
  ) extends Leaf[A]
      with Many[A]:
    type E = A

    protected[ArrayTree] def extendTreeAndAppend(
        leafCapacity: PositiveSize,
        nodeCapacity: PositiveSize,
        closedSize: TSize,
        updateState: (Leaf[A], Option[Tree[A]], TSize) => Boolean
    )(a: A): TIndex | Collision =
      def ensureNodeAndAppend(a: A): TIndex | Collision =
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

        def newLeaf(a: A, parent: Node[A]) = MultiLeaf(leafCapacity, parent)(a)

        def newNode(upper: Boolean, parent: Node[A] | Null): Node[A] =
          if upper then UpperNode.empty[A](nodeCapacity, parent)
          else LeafParentNode.empty[A](nodeCapacity, parent)

        val newClosedSize = closedSize + size
        findExtendable(this, depth = Size(0)) match
          case Left(exhaustedRoot) -> distance =>
            val (newPath, pathLeaf): (Node[A], MultiLeaf[A]) =
              @tailrec def loop(i: Size, parent: Node[A]): MultiLeaf[A] =
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

              root -> loop(Size(0), root)

            if updateState(pathLeaf, Some(newPath), newClosedSize) then
              exhaustedRoot.parent = newPath
              newClosedSize
            else Collision

          case Right(extendable) -> distance =>
            val (newPath, pathLeaf): (Many[A], MultiLeaf[A]) =
              @tailrec def loop(i: Size, parent: Node[A] | Null, root: Option[Node[A]]): (Many[A], MultiLeaf[A]) =
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

              loop(Size(1), extendable, None)

            if updateState(pathLeaf, None, newClosedSize) then
              (extendable, newPath) match
                case (upper: UpperNode[A], node: Node[A])    => upper append node
                case (_: LeafParentNode[A], _: MultiLeaf[A]) =>
                case _                                       => assert(false, "unexpected type mismatch")
              newClosedSize
            else Collision
      end ensureNodeAndAppend

      ensureNodeAndAppend(a) match
        case Collision                                              => Collision
        case idx: TIndex @unchecked /* works only as second case */ => idx

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

  // TODO further tighten boundaries like `AtLeast2`
  // TODO optimize by packing fields into primitive

  /** In general, try to minimize the number of nodes. A higher number of nodes makes only sense if the collection
    * size at some high percentile is orders of magnitude greater than its size at some low percentile.
    *
    * Examples:
    *   - Given an evenly distributed size of roughly 1,000 to 10,000, you might opt for
    *     - `initialCapacity` = 2,500
    *     - `leafCapacity` = 1,500
    *     - `nodeCapacity` = 16
    *   - but with some concern about memory usage due to many instances or other constraints, change the above like
    *     - `initialCapacity` = 1,800
    *     - `leafCapacity` = 500
    *     - `nodeCapacity` = 32.
    *   - Given a broad distribution of sizes between 1,000 and 1,000,000,000, a good choice would be to set
    *     - `initialCapacity` = 5,000
    *     - `leafCapacity` = 2,500
    *     - `nodeCapacity` = 300.
    *
    * @param initialCapacity number of elements of type `A` to be allocated in the first leaf.
    *                        This should cover between 10th to 40th percentile of size distribution.
    *                        The more memory usage concerns, the lower percentile is adequate.
    *                        In case you expect lots of instances with zero or just one element,
    *                        you can also set it to 1 to save main memory.
    * @param leafCapacity number of elements of type `A` to be allocated for subsequent leaves.
    *                     For tiny collections, at least 8 is recommended.
    *                     For bigger collections, choose a higher value that also fits `nodeCapacity`.
    * @param nodeCapacity number of elements to be allocated for nodes. 2 at least formally,
    *                     but even for small collections, at least 4 is recommended.
    *                     For best efficiency, choose a capacity such that the tree height probably won't exceed 8.
    */
  final case class Config(initialCapacity: PositiveSize, leafCapacity: PositiveSize, nodeCapacity: PositiveSize):
    import Config.*
    given PositiveSize = nodeCapacity

    /** Buffer with precalculated `LevelCap`s to support tree look-ups by index.
      * 8 levels are calculated in advance. Further levels are added on demand.
      * Index n corresponds to the tree height with a distance of n + 1 from the bottom, leaf level.
      */
    protected[concurrent] val levelCaps: ArrayBuffer[LevelCap] =
      populateLevelCaps(LevelCap(initialCapacity, leafCapacity), new ArrayBuffer[LevelCap](8))

    /** Add another 8 levels of `LevelCap` at most.
      * @return Whether any new levels could be added.
      */
    protected[concurrent] def extendLevelCaps: Boolean = levelCaps.last match
      case full: LevelCap.Full =>
        populateLevelCaps(full.next, levelCaps)
        true
      case partial => false

    /** Add 8 levels of `LevelCap`. The number of levels added might be less if capacity is exhausted. */
    private def populateLevelCaps(sizes: LevelCap, buf: ArrayBuffer[LevelCap]): ArrayBuffer[LevelCap] =
      @tailrec def loop(level: Int, sizes: LevelCap): ArrayBuffer[LevelCap] =
        sizes match
          case full @ LevelCap.Full(initial, subsequent, total) if level < 8 =>
            buf += full
            loop(level + 1, full.next)
          case partial: LevelCap.Partial if level < 8 =>
            buf += partial
            buf
          case _ => buf

      loop(0, sizes)

    @tailrec private def ensureLevels(startHeight: Positive): Boolean =
      if startHeight.value <= levelCaps.size then true
      else if extendLevelCaps then ensureLevels(startHeight)
      else false

    protected[concurrent] def withLevelCaps[R](height: Positive)(body: => R): R =
      if ensureLevels(height) then body
      else throw new IllegalArgumentException(s"Internal error: $height is too heigh for $this.")

    protected[concurrent] inline def slotAndSubIndex(
        capIndex: Index,
        i: Index,
        leftSide: Boolean
    ): (Index, Index) =
      levelCaps(capIndex.value).slotAndSubIndex(i, leftSide)

  object Config:
    /** Capacities per tree level. */
    sealed protected[concurrent] trait LevelCap:
      def first: PositiveSize

      // TODO optimize return by packing it into primitive
      def slotAndSubIndex[U](i: Index, leftSide: Boolean): (Index, Index)

      protected def slotAndSubIndex[U](i: Index, leftSide: Boolean, subsequent: PositiveSize): (Index, Index) =
        if leftSide then
          if i < first.asNonNegative then Index.zero -> i
          else
            val iSubsequent = i.mapTrusted(_ - first.value)
            iSubsequent.mapTrusted(_ / subsequent.value + 1) -> (iSubsequent % subsequent.asNonNegative)
        else i.mapTrusted(_ / subsequent.value) -> (i % subsequent.asNonNegative)

    protected[concurrent] object LevelCap:

      /** Fully defined capacities of some tree level.
        * @param first capacity of the first node
        * @param subsequent capacity of subsequent nodes
        * @param total capacity of the level, in other words, capacity of all nodes
        */
      protected[concurrent] case class Full(first: PositiveSize, subsequent: PositiveSize, total: PositiveSize)
          extends LevelCap:
        def slotAndSubIndex[U](i: Index, leftSide: Boolean): (Index, Index) =
          slotAndSubIndex(i, leftSide, subsequent)

        def next(using nodeCapacity: PositiveSize): LevelCap =
          Try(nodeCapacity * subsequent).map(newSubsequent =>
            (newSubsequent, Try(total + nodeCapacity.decr * newSubsequent))
          ) match
            case Success(newSubsequent, Success(newTotal)) => Full(total, newSubsequent, newTotal)
            case Success(newSubsequent, _)                 => Partial(total, Some(newSubsequent))
            case _                                         => Partial(total, None)

      /** Partially defined capacities of the highest possible tree level.
        * This copes with a numeric overflow of `PositiveSize` somewhere within this tree level.
        */
      protected[concurrent] case class Partial(first: PositiveSize, subsequent: Option[PositiveSize]) extends LevelCap:
        def slotAndSubIndex[U](i: Index, leftSide: Boolean): (Index, Index) =
          subsequent match
            case Some(s) => slotAndSubIndex(i, leftSide, s)
            case None    => Index(1) -> i.mapTrusted(_ - first.value)

      // TODO optimize arithmetics by rounding up sizes to power of 2 and shifting
      def apply(first: PositiveSize, subsequent: PositiveSize)(using nodeCapacity: PositiveSize): LevelCap =
        Try(first + nodeCapacity.decr * subsequent) match
          case Success(total) => Full(first, subsequent, total)
          case _              => Partial(first, Some(subsequent))
