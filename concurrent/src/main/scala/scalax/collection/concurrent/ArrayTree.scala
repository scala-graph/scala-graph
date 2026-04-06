package scalax.collection.concurrent

import java.util.ConcurrentModificationException
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.{
  AbstractIterator, IndexedSeq, IndexedSeqOps, IterableFactoryDefaults, SeqFactory, StrictOptimizedSeqOps
}
import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.collection.mutable.{AbstractSeq, ArrayBuffer, Builder, Stack}
import scala.reflect.ClassTag
import scala.util.{Success, Try}
import scala.util.chaining.given
import scalax.util.collection.MaxSizeView
import scalax.util.invoke.ArrayVarHandle
import scalax.util.invoke.ArrayVarHandle.anyRefHandle
import scalax.util.primitives.{LimitOverflowException, *}
import ArrayTree.{Config, LeafLike, TSize, Tree}

/** Concurrent, appendable only, compressed rose tree with leaves of type `Array[A]`.
  *
  * Weak iterators skip any element not yet visible in the current thread.
  * Strong iterators wait for elements until they are visible.
  *
  * On x86, strong and weak iterators are equally fast unless under high contention.
  * Under high contention, weak iterators behave stable while strong iterators might block.
  * On ARM CPUs, weak iterators should generally be faster irrespective of contention.
  *
  * With above in mind, choose strong iterators only if you need to also include elements
  * that were possibly appended in other threads directly before your call.
  * Otherwise, go for weak iterators.
  *
  * While strong iterators always return an `Iterator` with `knownSize` set, weak iterators return `MaxSizeView`.
  * `MaxSizeView` allows for an optimal preallocation when the iterator gets materialized into some
  * collection that is initiated with the right size for best efficiency, especially into array-based collections.
  *
  * @param config allows for an optimal setup in terms of speed and memory usage.
  * @tparam A type of elements.
  */
final class ArrayTree[A] private (
    initialTree: Tree[A],
    initialSize: TSize,
    initialActiveLeaf: LeafLike[A],
    initialClosedSize: TSize
)(using config: Config)(using tag: ClassTag[A], varHandle: ArrayVarHandle[A])
    extends AbstractSeq[A],
      IndexedSeq[A],
      IndexedSeqOps[A, ArrayTree, ArrayTree[A]],
      StrictOptimizedSeqOps[A, ArrayTree, ArrayTree[A]],
      IterableFactoryDefaults[A, ArrayTree]:
  self =>
  import config.*
  import ArrayTree.*
  import scalax.collection.concurrent.ArrayTreeIntrinsics.SizeH

  @volatile private var _tree: Tree[A] = initialTree

  @unused("Access by VarHandle. '@volatile protected' needed to not be dropped.")
  @volatile protected var _size: TSize = initialSize

  /** The last, so probably not yet exhausted `Leaf` or `Empty`.
    * When extending the tree structure and updating shared values,
    * we always start with checking that this value was not changed concurrently.
    */
  @volatile private var _activeLeaf: LeafLike[A] = initialActiveLeaf

  /** Number of elements of type `A` in the tree <b>excluding</b> those in `_activeLeaf`. */
  @volatile private var _closedSize = initialClosedSize

  /** Number of tree levels. `_tree`, the root node, is always at this level.
    * This redundant number, it could also be calculated as a function of `config` and `_size`,
    * is handy for index-based look-ups.
    */
  @volatile private var _levels = Level.zero

  private val treeSync = new ReentrantReadWriteLock(fairLock)

  @volatile protected[concurrent] def tree: Tree[A] = _tree

  def length: Int = SizeH.get(this)

  private object readState:
    private[ArrayTree] inline def treeLevels                     = withLock(_tree, _levels)
    private[ArrayTree] inline def treeSize                       = withLock(_tree, self._size)
    private[ArrayTree] inline def activeLeafClosedSize           = withLock(_activeLeaf, _closedSize)
    private[ArrayTree] inline def treeActiveLeafClosedSize       = withLock(_tree, _activeLeaf, _closedSize)
    private[ArrayTree] inline def treeLevelsActiveLeafClosedSize = withLock(_tree, _levels, _activeLeaf, _closedSize)

    private def withLock[R](block: => R): R =
      val rLock = treeSync.readLock
      rLock.lock()
      val ret = block
      rLock.unlock()
      ret
  end readState

  private object updateState:
    def apply(prevActiveLeaf: LeafLike[A], activeLeaf: Leaf[A]): Boolean =
      withLock(prevActiveLeaf) {
        _activeLeaf = activeLeaf
        _tree = activeLeaf
        updateSize()
      }

    def apply(prevActiveLeaf: LeafLike[A], activeLeaf: MultiLeaf[A], tree: Node[A], closedSize: TSize): Boolean =
      withLock(prevActiveLeaf) {
        _activeLeaf = activeLeaf
        _tree = tree
        _levels = _levels.incr
        _closedSize = closedSize
        updateSize()
      }

    def apply(prevActiveLeaf: LeafLike[A], activeLeaf: MultiLeaf[A], mount: => Unit, closedSize: TSize): Boolean =
      withLock(prevActiveLeaf) {
        _activeLeaf = activeLeaf
        mount
        _closedSize = closedSize
        updateSize()
      }

    private def withLock(prevActiveLeaf: LeafLike[A])(block: => Unit): Boolean =
      val wLock = treeSync.writeLock
      wLock.lock()
      try
        if _activeLeaf eq prevActiveLeaf then
          block
          true
        else false
      finally
        wLock.unlock()

    private def updateSize(): Unit =
      if SizeH.incrAndGet(self) > TSize.upperLimit then throw new LimitOverflowException
  end updateState

  def apply(index: Int): A = apply(TIndex.unsafe(index))

  /** @throws `IndexOutOfBoundsException` if `index` is not less than `size`. */
  @targetName("applyTIndex")
  def apply(index: TIndex): A =
    if index < _size then
      val (tree, levels) = readState.treeLevels
      tree match
        case root: Many[A]    => _apply(root, levels, index)(_.apply(_))
        case SingleLeaf(elem) => elem
        case _: Empty[A]      => throw new IndexOutOfBoundsException
    else throw new IndexOutOfBoundsException

  /** `apply` two times optimized.
    * This is more efficient than calling `apply` two times in case `index1` and `index2` are close
    * such that both are stored in the same internal array of leaves.
    *
    * @throws `IndexOutOfBoundsException` if `index` is not less than `size`.
    */
  def apply(index1: TIndex, index2: TIndex): (A, A) =
    if index1 < _size && index2 < _size then
      val (tree, levels) = readState.treeLevels
      tree match
        case root: Many[A] =>
          def withMulti(multi: MultiLeaf[A], i: Index): (A, A) =
            val elem1 = multi(i)
            if index1 == index2 then (elem1, elem1)
            else
              val multiIndex = i.value - index1.value + index2.value
              if multiIndex >= 0 && multiIndex < multi.elems.length then (elem1, multi(Index.trust(multiIndex)))
              else (elem1, _apply(root, levels, index2)(_.apply(_)))

          _apply(root, levels, index1)(withMulti)
        case SingleLeaf(elem) => (elem, elem)
        case _: Empty[A]      => throw new IndexOutOfBoundsException
    else throw new IndexOutOfBoundsException

  private def _apply[R](many: Many[A], levels: NonNegative, index: TIndex)(withMulti: (MultiLeaf[A], Index) => R): R =
    many match
      case upper: UpperNode[A] =>
        val (multi, subIndex, _) = leaf(upper, levels, index)
        withMulti(multi, subIndex)
      case LeafParentNode(elems) =>
        val (slot, subIndex, _) = locate(Index.zero, index, leftSide = true)
        withMulti(elems(slot.value), subIndex)
      case multi: MultiLeaf[A] => withMulti(multi, index)

  @tailrec infix def append(a: A): TIndex =
    val (lastTree, lastActiveLeaf, lastClosedSize) = readState.treeActiveLeafClosedSize
    _append(a)(using lastTree, lastActiveLeaf, lastClosedSize) match
      case Collision                => append(a)
      case index: TIndex @unchecked => index

  /** Appends element `a` directly after the index `after` unless that index is already occupied.
    *
    * @param a the element to be appended.
    * @param after the last used index from the caller's perspective. Ignored in case of an empty collection.
    * @return The index of the appended element or `Conflict` if the subsequent index is already in use.
    *
    * @throws `IndexOutOfBoundsException` if `index` is not less than `size`.
    */
  def append(a: A, after: TIndex): TIndex | Conflict.type =
    val writeLock = treeSync.writeLock
    if writeLock.tryLock() then
      try
        val currentSize = self._size
        if currentSize > Size.zero && after == currentSize.decrTrusted || currentSize == Size.zero then
          _append(a)(using _tree, _activeLeaf, _closedSize) match
            case Collision => throw new AssertionError("Unexpected collision despite write lock.")
            case index: TIndex @unchecked /* must be last case */ => index
        else if after < currentSize then Conflict
        else throw new IndexOutOfBoundsException
      finally writeLock.unlock()
    else Conflict
  end append

  def _append(a: A)(using lastTree: Tree[A], lastActiveLeaf: LeafLike[A], lastClosedSize: TSize): TIndex | Collision =
    lastActiveLeaf append a match
      case Exhausted =>
        lastActiveLeaf.extendTreeAndAppend(lastTree, lastClosedSize, this)(a) match
          case Collision                                      => Collision
          case idx: TIndex @unchecked /* must be last case */ => idx
      case idx: Index @unchecked /* must be last case */ =>
        if SizeH.incrAndGet(self) > TSize.upperLimit then throw new LimitOverflowException
        lastClosedSize + idx

  def remove(index: TIndex, last: TSeqId): TSeqId | Conflict.type =
    ???

  def update(idx: HashCode, elem: A): Unit = ???

  /** Same as `strongIterator`. */
  override def iterator: Iterator[A] = strongIterator

  def strongIterator: Iterator[A] =
    val (tree, lastSize) = readState.treeSize
    val treeIt           = leaves(tree)
    if treeIt.hasNext then
      new AbstractIterator[A]:
        override val knownSize: Int     = lastSize.value
        private var remaining           = knownSize
        private var leafIt: Iterator[A] = treeIt.next().strongIterator(atMost = remaining)

        def hasNext: Boolean =
          leafIt.hasNext || (treeIt.hasNext && remaining > leafIt.knownSize)

        def next(): A =
          if leafIt.hasNext then leafIt.next()
          else if treeIt.hasNext && remaining > leafIt.knownSize then
            remaining -= leafIt.knownSize
            leafIt = treeIt.next().strongIterator(atMost = remaining)
            leafIt.next()
          else throw new IndexOutOfBoundsException
    else Iterator.empty

  def weakIterator: MaxSizeView[A] =
    val treeIt = leaves(tree)
    if treeIt.hasNext then
      val it: AbstractIterator[A] = new AbstractIterator[A]:
        override def size: Int =
          if hasNext then -1 else consumed

        var consumed                    = 0
        private var leafIt: Iterator[A] = treeIt.next().weakIterator

        def hasNext: Boolean =
          leafIt.hasNext || treeIt.hasNext

        def next(): A =
          if leafIt.hasNext then
            consumed += 1
            leafIt.next()
          else if treeIt.hasNext then
            leafIt = treeIt.next().weakIterator
            consumed += 1
            leafIt.next()
          else throw new IndexOutOfBoundsException

      MaxSizeView(it, _size)
    else MaxSizeView.empty

  def strongReverseIterator: Iterator[A] =
    reverseIteratorImpl(_.strongReverseIterator(), stronglyConsistent.ReverseIterator.apply)

  def weakReverseIterator: MaxSizeView[A] =
    MaxSizeView(reverseIteratorImpl(_.weakReverseIterator(), weaklyConsistent.ReverseIterator.apply), _size)

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def strongReverseIterator(from: TIndex): Iterator[A] =
    reverseIteratorImpl(from, _.strongReverseIterator(_), stronglyConsistent.ReverseIterator.from)

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def weakReverseIterator(from: TIndex): MaxSizeView[A] = MaxSizeView(
    reverseIteratorImpl(from, _.weakReverseIterator(_), weaklyConsistent.ReverseIterator.from),
    from.incrTrusted
  )

  def strongReverseIteratorWithIndex: Iterator[(A, TIndex)] =
    reverseIteratorImpl(_.strongReverseIteratorWithIndex(), stronglyConsistent.ReverseIteratorWithIndex.apply)

  def weakReverseIteratorWithIndex: MaxSizeView[(A, TIndex)] = MaxSizeView(
    reverseIteratorImpl(_.weakReverseIteratorWithIndex(), weaklyConsistent.ReverseIteratorWithIndex.apply),
    _size
  )

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def strongReverseIteratorWithIndex(from: TIndex): Iterator[(A, TIndex)] =
    reverseIteratorImpl(from, _.strongReverseIteratorWithIndex(_), stronglyConsistent.ReverseIteratorWithIndex.from)

  /** @throws `IndexOutOfBoundsException` if `from` is not less than `size`. */
  def weakReverseIteratorWithIndex(from: TIndex): MaxSizeView[(A, TIndex)] = MaxSizeView(
    reverseIteratorImpl(from, _.weakReverseIteratorWithIndex(_), weaklyConsistent.ReverseIteratorWithIndex.from),
    from.incrTrusted
  )

  private object reverseIteratorImpl:
    def apply[B](
        leafIterator: Leaf[A] => Iterator[B],
        leavesIterator: (MultiLeaf[A], TSize) => Iterator[B]
    ): Iterator[B] =
      val (activeLeaf, closedSize) = readState.activeLeafClosedSize
      activeLeaf match
        case leaf: Leaf[A] if leaf.leftNeighbor eq null => leafIterator(leaf)
        case multi: MultiLeaf[A]                        => leavesIterator(multi, closedSize)
        case _                                          => Iterator.empty

    def apply[B](
        from: TIndex,
        leafIterator: (Leaf[A], Index) => Iterator[B],
        leavesIterator: (MultiLeaf[A], TIndex, TSize) => Iterator[B]
    ): Iterator[B] =
      if from < _size then
        val (tree, levels, activeLeaf, closedSize) = readState.treeLevelsActiveLeafClosedSize
        val fromActiveLeaf                         = from >= closedSize

        def leavesIt: Iterator[B] =
          activeLeaf match
            case multi: MultiLeaf[A] => leavesIterator(multi, from - closedSize, closedSize)
            case x => throw new AssertionError(s"Unexpected non-multi leaf ${x.getClass.getSimpleName}.")

        tree match
          case _: UpperNode[A] if fromActiveLeaf => leavesIt
          case upper: UpperNode[A]               =>
            val (multi, subIndex, leftSize) = leaf(upper, levels, from)
            leavesIterator(multi, subIndex, leftSize)
          case LeafParentNode(_) if fromActiveLeaf => leavesIt
          case LeafParentNode(elems)               =>
            val (slot, subIndex, leftSize) = locate(Index.zero, from, leftSide = true)
            leavesIterator(elems(slot.value), subIndex, leftSize)
          case leaf: Leaf[A] => leafIterator(leaf, from)
          case _: Empty[A]   => Iterator.empty
      else throw new IndexOutOfBoundsException

  private def leaf(root: UpperNode[A], levels: NonNegative, index: TIndex): (MultiLeaf[A], Index, TSize) =
    withLevelCaps(Positive.trust(levels.value)) { height =>
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

  private inline def leaves(tree: Tree[A]): Iterator[Leaf[A]] =
    treeIterator(tree).collect { case leaf: Leaf[A] => leaf }

  protected[concurrent] def treeIterator(tree: Tree[A] = _tree): Iterator[NonEmpty[A]] =
    tree.iteratorWithLevel.map(_._1)

  protected[concurrent] def prettifyTree(
      includeNodes: Boolean,
      marginSize: NonNegative = NonNegative(2),
      indentSize: Positive = Positive(2)
  ): String =
    _tree.prettifyTree(includeNodes, marginSize, indentSize)

  override def knownSize: Int = size

  override def iterableFactory: SeqFactory[ArrayTree] =
    throw new UnsupportedOperationException("Extend ArrayTree or use ArrayTree.newBuilder or iterator instead.")

  override inline def empty: ArrayTree[A]                                         = ArrayTree.empty[A]
  override protected inline def fromSpecific(coll: IterableOnce[A]): ArrayTree[A] = ArrayTree[A](coll)
  override protected inline def newSpecificBuilder: Builder[A, ArrayTree[A]]      = newBuilder

  // provide complete signatures for the most often used methods that invoke `iterableFactory` otherwise

  private inline def adjustedInitialCap: Config =
    config.copy(initialCap = config.initialCap max _size.asPositiveOrElse1)

  inline def map[B: {ClassTag, ArrayVarHandle}](f: A => B): ArrayTree[B] =
    ArrayTree(iterator map f)(using adjustedInitialCap)

  inline def flatMap[B: {ClassTag, ArrayVarHandle}](f: A => IterableOnce[B]): ArrayTree[B] =
    ArrayTree(iterator flatMap f)

  inline def collect[B: {ClassTag, ArrayVarHandle}](pf: PartialFunction[A, B]): ArrayTree[B] =
    ArrayTree(iterator collect pf)

  inline def zip[B: {ClassTag, ArrayVarHandle}](that: IterableOnce[B]): ArrayTree[(A, B)] =
    ArrayTree(iterator zip that)(using adjustedInitialCap)

object ArrayTree:
  companion =>

  /** Indicate that `ArrayTree`'s size is not necessarily limited to Int.
    * For the time being it's fine to limit support to Int, though.
    */
  type TSize  = Size
  type TIndex = Size
  type TSeqId = Size
  protected[concurrent] val TSize, TIndex, TSeqId: NonNegative.type = Size

  inline val Conflict = -3

  private type Level = NonNegative
  private val Level = NonNegative

  def empty[A](using config: Config)(using tag: ClassTag[A], vh: ArrayVarHandle[A]): ArrayTree[A] =
    new ArrayTree[A](Empty.of[A], TSize.zero, Empty.of[A], TSize.zero)

  def apply[A](elem: A, elems: A*)(using config: Config)(using tag: ClassTag[A], vh: ArrayVarHandle[A]): ArrayTree[A] =
    applyNonEmpty(Iterator(elem) ++ elems)

  def apply[A](elems: IterableOnce[A])(using
      config: Config
  )(using tag: ClassTag[A], vh: ArrayVarHandle[A]): ArrayTree[A] =
    def isEmpty = elems match
      case it: Iterable[A] => it.isEmpty
      case once            => once.iterator.isEmpty

    if isEmpty then empty else applyNonEmpty(elems)

  private def applyNonEmpty[A](elems: IterableOnce[A])(using
      _config: Config
  )(using tag: ClassTag[A], vh: ArrayVarHandle[A]) =
    val it        = elems.iterator
    val knownSize = elems.knownSize
    if knownSize == 1 && _config.initialCap == Capacity(1) then
      val one = SingleLeaf(it.next())
      new ArrayTree[A](one, TSize(1), one, TSize(1))
    else
      val config = knownSize match
        case -1                                       => _config
        case size if _config.initialCap.value >= size => _config
        case bigSize                                  => _config.copy(initialCap = Capacity.unsafe(bigSize))
      import config.*

      @tailrec def loop(
          root: Many[A],
          closedSize: TSize,
          activeLeaf: MultiLeaf[A]
      ): (Many[A], TSize, MultiLeaf[A], TSize) =
        val newSize = closedSize + activeLeaf.appendUnsafeFrom(it)
        if it.hasNext then
          val nextLeaf = MultiLeaf.empty(leafCap.asPositive, activeLeaf)
          Tree.extend(root, activeLeaf, nextLeaf, mount = true) match
            case Left((newRoot = r)) => loop(r, newSize, nextLeaf)
            case Right((_, _))       => loop(root, newSize, nextLeaf)
        else (root, newSize, activeLeaf, closedSize)

      val initial =
        val firstLeaf = MultiLeaf.empty(initialCap, null)
        loop(firstLeaf, TSize.zero, firstLeaf)
      new ArrayTree[A](initial._1, initial._2, initial._3, initial._4)(using config)
  end applyNonEmpty

  private type Exhausted = -1; private val Exhausted: Exhausted = -1
  private type Collision = -2; private val Collision: Collision = -2

  sealed protected[concurrent] trait Tree[A]:
    def capacity: Size
    def size: Size
    final def exhausted: Boolean = size == capacity

    def iteratorWithLevel: Iterator[(NonEmpty[A], Level)] = this match
      case _: Empty[A]       => Iterator.empty
      case tree: NonEmpty[A] =>
        val lastSize = size
        new AbstractIterator[(NonEmpty[A], Level)]:
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

          def next(): (NonEmpty[A], Level) =
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
    end iteratorWithLevel

    def prettifyTree(
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
        if includeNodes then iteratorWithLevel
        else iteratorWithLevel.filter(_._1.isInstanceOf[Leaf[A]])

      it.foreach { case elem -> level => append(elem.toString, level) }
      builder.toString

  private object Tree:
    def extend[A](tree: Tree[A], exhausted: MultiLeaf[A], newMulti: MultiLeaf[A], mount: Boolean)(using
        config: Config
    )(using tag: ClassTag[A]): Either[(newRoot: Node[A]), (extendable: Node[A], newPath: Many[A])] =
      import config.nodeCap

      def newNode(upper: Boolean): Node[A] =
        if upper then UpperNode.empty[A](nodeCap)
        else LeafParentNode.empty[A](nodeCap)

      Tree.findExtendable(tree, exhausted) match
        case Left(exhaustedRoot) -> distance =>
          @tailrec def loop(i: Size, parent: Node[A]): Unit =
            parent match
              case upper: UpperNode[A] =>
                assert(i < distance)
                val incr = i.incrTrusted
                loop(incr, newNode(upper = incr < distance) tap upper.append)
              case leafParent: LeafParentNode[A] =>
                assert(i == distance)
                leafParent append newMulti

          val root: Node[A] =
            exhaustedRoot match
              case exhaustedNode: Node[A] =>
                UpperNode.empty[A](nodeCap) tap (_ append exhaustedNode)
              case exhaustedLeaf: MultiLeaf[A] =>
                LeafParentNode.empty[A](nodeCap) tap (_ append exhaustedLeaf)

          Left((newRoot = root tap (loop(Size.zero, _))))

        case Right(extendable) -> distance =>
          @tailrec def loop(i: Size, parent: Node[A] | Null, root: Option[Node[A]]): Many[A] =
            parent match
              case upper: UpperNode[A] =>
                assert(i < distance)
                val incr = i.incrTrusted
                val n    = newNode(upper = incr < distance)
                if root.isDefined then upper append n
                loop(i.incrTrusted, n, root orElse Some(n))
              case leafParent: LeafParentNode[A] if leafParent ne extendable =>
                assert(i == distance)
                leafParent append newMulti
                root getOrElse newMulti
              case leafParent: LeafParentNode[A] =>
                root getOrElse newMulti

          val path = loop(Size(1), extendable, None)
          if mount then Tree.mount(extendable, path)
          Right((extendable = extendable, newPath = path))
    end extend

    def mount[A](extend: Node[A], byPath: Many[A]): Unit =
      (extend, byPath) match
        case (upper: UpperNode[A], node: Node[A])        => upper append node
        case (leafP: LeafParentNode[A], m: MultiLeaf[A]) => if leafP eq extend then leafP append m
        case _                                           => assert(false, "unexpected Tree pattern")

    /** Searches for the closest extendable predecessor of `exhausted`.
      *
      * @return
      *   - either
      *     - `Left` containing the exhausted root or
      *     - `Right` containing an extendable predecessor `Node`
      *   - the distance of the above from `this` leaf.
      */
    private def findExtendable[A](root: Tree[A], exhausted: MultiLeaf[A]): (Either[Many[A], Node[A]], Size) =
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

        case l: LeafLike[A] =>
          assert(root eq exhausted)
          Left(exhausted) -> Size.zero
    end findExtendable

  sealed protected trait LeafLike[A] extends Tree[A]:
    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        closedSize: TSize,
        arrayTree: ArrayTree[A]
    )(a: A)(using config: Config): TIndex | Collision

    protected[ArrayTree] infix def append(a: A): Index | Exhausted

    protected[ArrayTree] def leftNeighbor: MultiLeaf[A] | Null

  sealed abstract protected[concurrent] class Empty[A: {ClassTag, ArrayVarHandle}] extends LeafLike[A]:
    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        closedSize: TSize,
        arrayTree: ArrayTree[A]
    )(a: A)(using config: Config): TIndex | Collision =
      val leaf: Leaf[A] =
        import config.initialCap
        if initialCap === 1 then SingleLeaf(a)
        else MultiLeaf(initialCap, leftNeighbor = null)(a)
      if arrayTree.updateState(this, leaf) then TIndex.zero
      else Collision

    protected[ArrayTree] inline infix def append(a: A): Index | Exhausted = Exhausted

    protected[ArrayTree] inline def leftNeighbor: MultiLeaf[A] | Null = null

    protected[ArrayTree] inline def strongIterator(atMost: Int): Iterator[A] = Iterator.empty

    def capacity: Size = Size.zero

    def size: Size = Size.zero

  private object Empty:
    private object EmptyInt    extends Empty[Int]
    private object EmptyLong   extends Empty[Long]
    private object EmptyAnyRef extends Empty[AnyRef]

    def of[A](using tag: ClassTag[A], varHandle: ArrayVarHandle[A]): Empty[A] = tag match
      case t if !t.runtimeClass.isPrimitive => EmptyAnyRef.asInstanceOf[Empty[A]]
      case ClassTag.Long                    => EmptyLong
      case ClassTag.Int                     => EmptyInt
      case _                                => new Empty[A] {}

  sealed protected[concurrent] trait NonEmpty[A] extends Tree[A]

  sealed protected[concurrent] trait Leaf[A: ClassTag] extends NonEmpty[A] with LeafLike[A]:
    /** Appends `a` to `this` if there is free space.
      *
      * @return the index where `a` was inserted, or `Exhausted` if there was no free space.
      */
    protected[ArrayTree] infix def append(a: A): Index | Exhausted

    /** To be called after `append` has reported `Exhausted`. */
    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        closedSize: TSize,
        arrayTree: ArrayTree[A]
    )(a: A)(using config: Config): TIndex | Collision

    protected[ArrayTree] def strongIterator(atMost: Int): Iterator[A]
    protected[ArrayTree] def weakIterator: Iterator[A]

    protected[ArrayTree] def strongReverseIterator(from: Index = size.decr): Iterator[A]
    protected[ArrayTree] def strongReverseIteratorWithIndex(from: Index = size.decr): Iterator[(A, Index)]

    protected[ArrayTree] def weakReverseIterator(from: Index = size.decr): Iterator[A]
    protected[ArrayTree] def weakReverseIteratorWithIndex(from: Index = size.decr): Iterator[(A, Index)]

  final protected[concurrent] case class SingleLeaf[A: {ClassTag, ArrayVarHandle}](elem: A) extends Leaf[A]:
    def capacity: Size = Size(1)
    def size: Size     = Size(1)

    protected[ArrayTree] infix def append(a: A): Index | Exhausted = Exhausted

    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        closedSize: TSize,
        arrayTree: ArrayTree[A]
    )(a: A)(using config: Config): TIndex | Collision =
      val leaf = MultiLeaf(config.leafCap.asPositive, leftNeighbor = null)(elem)
      leaf append a
      if arrayTree.updateState(this, leaf) then TIndex(1)
      else Collision

    protected[ArrayTree] inline def strongIterator(atMost: Int): Iterator[A] =
      if atMost > 0 then Iterator.single(elem)
      else Iterator.empty

    protected[ArrayTree] inline def weakIterator: Iterator[A] = strongIterator(1)

    protected[ArrayTree] inline def strongReverseIterator(from: Index = Index.zero): Iterator[A] =
      assert(from == Index.zero)
      Iterator.single(elem)

    protected[ArrayTree] inline def strongReverseIteratorWithIndex(from: Index = Index.zero): Iterator[(A, Index)] =
      assert(from == Index.zero)
      strongReverseIterator(from).zip(Iterator.single(Index.zero))

    protected[ArrayTree] inline def weakReverseIterator(from: Index = Index.zero): Iterator[A] =
      strongReverseIterator(from)

    protected[ArrayTree] inline def weakReverseIteratorWithIndex(from: Index = Index.zero): Iterator[(A, Index)] =
      strongReverseIteratorWithIndex(from)

    protected[ArrayTree] def leftNeighbor: MultiLeaf[A] | Null = null

  sealed protected[concurrent] trait Many[A] extends NonEmpty[A]:
    type E
    protected[ArrayTree] def elems: Array[E]

    /** Read element at `index` of `elems` with strong consistency. */
    protected[ArrayTree] inline def apply(index: Index): E = varHandle.pollAcquire(elems, index.value)

    /** The number of sequential elements used in `elems` starting at index 0.
      * Writers of `elems` always increment this before writing to ensure write consistency.
      */
    protected[ArrayTree] var _used: Int = 0
    protected[ArrayTree] def getUsed: Int
    protected[ArrayTree] def compareAndIncrUsed(current: Int): Boolean

    final def capacity: Size = Size.trust(elems.length)
    final def size: Size     = Size.trust(getUsed)

    final def last: E = varHandle.pollAcquire(elems, getUsed - 1)

    protected def varHandle: ArrayVarHandle[E]

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
      val idx = getUsed
      if idx < elems.length then
        if compareAndIncrUsed(idx) then
          varHandle.setRelease(elems, idx, elem)
          Index.trust(idx)
        else append(elem)
      else Exhausted
  end Many

  final protected[concurrent] case class MultiLeaf[A: {ClassTag as tag, ArrayVarHandle as handle}] private (
      protected[ArrayTree] val elems: Array[A],
      protected[ArrayTree] val leftNeighbor: MultiLeaf[A] | Null
  ) extends Leaf[A]
      with Many[A]:
    import ArrayTreeIntrinsics.MultiLeafUsedH as UsedH
    type E = A

    protected[ArrayTree] inline def compareAndIncrUsed(current: Int): Boolean = UsedH.compareAndIncr(this, current)
    protected[ArrayTree] inline def getUsed: Int                              = UsedH.get(this)

    protected inline def varHandle: ArrayVarHandle[E] = handle

    protected[ArrayTree] def extendTreeAndAppend(
        tree: Tree[A],
        closedSize: TSize,
        arrayTree: ArrayTree[A]
    )(a: A)(using config: Config): TIndex | Collision =
      def ensureNodeAndAppend(a: A): TIndex | Collision =
        val newMulti      = MultiLeaf(config.leafCap.asPositive, this)(a)
        val newClosedSize = closedSize + size

        Tree.extend(tree, this, newMulti, mount = false) match
          case Left((newRoot = root)) =>
            if arrayTree.updateState(this, newMulti, root, newClosedSize) then newClosedSize
            else Collision
          case Right((extendable = ext, newPath = path)) =>
            if arrayTree.updateState(this, newMulti, Tree.mount(ext, path), newClosedSize) then newClosedSize
            else Collision
      end ensureNodeAndAppend

      ensureNodeAndAppend(a) match
        case Collision                                              => Collision
        case idx: TIndex @unchecked /* works only as second case */ => idx

    /** Appends `elem` assuming that `capacity` is not exhausted in a single-threaded manner.
      *
      * @return the index `elem` has been inserted at.
      * @throws IndexOutOfBoundsException if capacity is exhausted.
      * @throws ConcurrentModificationException in case a concurrent append has been detected.
      */
    protected[ArrayTree] def appendUnsafe(elem: A): Index =
      val idx = _used
      elems(idx) = elem
      if !compareAndIncrUsed(idx) then throw new ConcurrentModificationException()
      varHandle.setRelease(elems, idx, elem)
      Index.trust(idx)

    /** Appends as many elements of `newElems` as capacity allows in a single threaded manner.
      * `setRelease` is called only for the last element appended.
      *
      * @return number of elements appended.
      * @throws ConcurrentModificationException in case a concurrent append has been detected.
      */
    protected[ArrayTree] def appendUnsafeFrom(newElems: Iterator[A]): Size =
      val used  = _used
      var index = used
      val it    = newElems.take(elems.length - index)
      while it.hasNext do
        elems(index) = it.next()
        index += 1
      if index > used then
        if !UsedH.compareAndSet(this, used, index) then throw new ConcurrentModificationException()
        val lastIdx = index - 1
        varHandle.setRelease(elems, lastIdx, elems(lastIdx))
        Size.trust(index - used)
      else Size.zero

    protected[ArrayTree] def strongIterator(atMost: Int): Iterator[A] =
      val used = getUsed
      MultiLeaf.StronglyConsistentIterator(elems, if used <= atMost then used else atMost)

    protected[ArrayTree] def weakIterator: Iterator[A] =
      MultiLeaf.WeaklyConsistentIterator(elems, getUsed)

    protected[ArrayTree] inline def strongReverseIterator(from: Index = Index.trust(getUsed - 1)): Iterator[A] =
      assert(from.value < getUsed)
      MultiLeaf.StronglyConsistentReverseIterator(elems, from.value)

    protected[ArrayTree] inline def strongReverseIteratorWithIndex(
        from: Index = Index.trust(getUsed - 1)
    ): Iterator[(A, Index)] =
      strongReverseIterator(from) zip from.incrTrusted.reverseIndexes

    protected[ArrayTree] inline def weakReverseIterator(from: Index = Index.trust(_used - 1)): Iterator[A] =
      assert(from.value < _used)
      MultiLeaf.WeaklyConsistentReverseIterator(elems, from.value)

    protected[ArrayTree] inline def weakReverseIteratorWithIndex(
        from: Index = Index.trust(_used - 1)
    ): Iterator[(A, Index)] =
      weakReverseIterator(from) zip from.incrTrusted.reverseIndexes

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
    private[ArrayTree] def empty[A: {ClassTag, ArrayVarHandle as handle}](
        cap: Capacity,
        leftNeighbor: MultiLeaf[A] | Null
    ): MultiLeaf[A] =
      new MultiLeaf[A](handle.newArray(cap.value), leftNeighbor)

    def apply[A: {ClassTag, ArrayVarHandle}](cap: Capacity, leftNeighbor: MultiLeaf[A] | Null)(elem: A): MultiLeaf[A] =
      empty[A](cap, leftNeighbor) tap (_.appendUnsafe(elem))

    def fromNonEmpty[A: {ClassTag, ArrayVarHandle}](cap: Capacity, leftNeighbor: MultiLeaf[A] | Null)(
        elems: A*
    ): MultiLeaf[A] =
      val it = elems.iterator
      assert(it.nonEmpty)
      empty[A](cap, leftNeighbor) tap (_.appendUnsafeFrom(it))

    private[MultiLeaf] class StronglyConsistentIterator[A: ArrayVarHandle as handle](elems: Array[A], until: Int)
        extends AbstractIterator[A]:
      override val knownSize: Int = until
      private var consumed        = 0

      inline def hasNext: Boolean = consumed < knownSize

      def next(): A =
        if hasNext then
          val r = handle.pollAcquire(elems, consumed)
          consumed += 1
          r
        else throw new NoSuchElementException

    private[MultiLeaf] class WeaklyConsistentIterator[A: ArrayVarHandle as handle](elems: Array[A], until: Int)
        extends SkippingIterator[A]:
      protected val undefined: A = handle.Undefined
      protected var nextElem: A  = undefined
      private var nextIndex      = 0

      protected def findNext(): Boolean =
        @tailrec def loop(i: Int): Boolean =
          if i < until then
            handle.get(elems, i) match
              case `undefined` => loop(i + 1)
              case elem        =>
                nextElem = elem
                nextIndex = i + 1
                true
          else
            nextIndex = until
            false

        loop(nextIndex)

    private[ArrayTree] class StronglyConsistentReverseIterator[A: ArrayVarHandle as handle](elems: Array[A], from: Int)
        extends AbstractIterator[A]:
      override val knownSize: Int = from + 1
      private var remaining       = knownSize

      inline def hasNext: Boolean = remaining > 0

      def next(): A =
        if hasNext then
          remaining -= 1
          handle.pollAcquire(elems, remaining)
        else throw new NoSuchElementException

    private[ArrayTree] class WeaklyConsistentReverseIterator[A: ArrayVarHandle as handle](elems: Array[A], from: Int)
        extends SkippingIterator[A]:
      protected val undefined: A = handle.Undefined
      protected var nextElem: A  = undefined
      private var nextIndex      = from

      protected def findNext(): Boolean =
        @tailrec def loop(i: Int): Boolean =
          if i >= 0 then
            handle.get(elems, i) match
              case `undefined` => loop(i - 1)
              case elem        =>
                nextElem = elem
                nextIndex = i - 1
                true
          else
            nextIndex = -1
            false

        loop(nextIndex)

    end WeaklyConsistentReverseIterator

    abstract protected class SkippingIterator[A] extends AbstractIterator[A]:
      protected val undefined: A
      protected var nextElem: A

      protected def findNext(): Boolean

      def hasNext: Boolean =
        if nextElem == undefined then findNext() else true

      def next(): A =
        if hasNext then
          val r = nextElem
          nextElem = undefined
          r
        else throw new NoSuchElementException

  sealed protected[concurrent] trait Node[A] extends Many[A]:
    type E <: Many[?]

    protected inline def varHandle: ArrayVarHandle[E] = anyRefHandle

  final protected[concurrent] case class UpperNode[A] private (
      protected[concurrent] val elems: Array[Node[A]]
  ) extends Node[A]:
    import ArrayTreeIntrinsics.UpperNodeUsedH as UsedH
    type E = Node[A]

    protected[ArrayTree] def compareAndIncrUsed(current: Int): Boolean = UsedH.compareAndIncr(this, current)
    protected[ArrayTree] def getUsed: Int                              = UsedH.get(this)

  protected[concurrent] case object UpperNode:
    def empty[A: ClassTag](cap: Log2Capacity): UpperNode[A] =
      new UpperNode[A](new Array[Node[A]](cap.asInt))

    def apply[A: ClassTag](cap: Log2Capacity)(elems: Node[A]*): UpperNode[A] =
      empty[A](cap) tap (elems foreach _.append)

  final protected[concurrent] case class LeafParentNode[A] private (
      protected[concurrent] val elems: Array[MultiLeaf[A]]
  ) extends Node[A]:
    import ArrayTreeIntrinsics.LeafParentNodeUsedH as UsedH
    type E = MultiLeaf[A]

    protected[ArrayTree] def compareAndIncrUsed(current: Int): Boolean = UsedH.compareAndIncr(this, current)
    protected[ArrayTree] def getUsed: Int                              = UsedH.get(this)

  protected[concurrent] case object LeafParentNode:
    def empty[A: ClassTag](cap: Log2Capacity): LeafParentNode[A] =
      new LeafParentNode[A](new Array[MultiLeaf[A]](cap.asInt))

    def apply[A: ClassTag](cap: Log2Capacity)(elems: MultiLeaf[A]*): LeafParentNode[A] =
      empty[A](cap) tap (elems foreach _.append)

  abstract protected class AbstractReverseIterator[A, B](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
      extends AbstractIterator[B]:
    private var currentLeaf              = from
    protected var currentIt: Iterator[A] = fromIt
    protected var index: TIndex          = total

    protected def nextResult(): B
    protected def multiLeafReverseIterator(multi: MultiLeaf[A]): Iterator[A]

    override def hasNext: Boolean =
      if currentIt.hasNext then true
      else
        currentLeaf.leftNeighbor match
          case multi: MultiLeaf[A] =>
            currentLeaf = multi
            currentIt = multiLeafReverseIterator(multi)
            currentIt.hasNext
          case null => false

    override def next(): B =
      index = index.decr
      if hasNext then nextResult()
      else throw new NoSuchElementException

  private object stronglyConsistent:
    abstract protected class AbstractReverseIterator[A: ArrayVarHandle, B](
        from: MultiLeaf[A],
        fromIt: Iterator[A],
        total: TSize
    ) extends ArrayTree.AbstractReverseIterator[A, B](from, fromIt, total):
      override def knownSize: Int = total.value

      protected def multiLeafReverseIterator(multi: MultiLeaf[A]): Iterator[A] =
        MultiLeaf.StronglyConsistentReverseIterator(multi.elems, multi.getUsed - 1)

    class ReverseIterator[A: ArrayVarHandle](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
        extends AbstractReverseIterator[A, A](from, fromIt, total):
      protected def nextResult(): A = currentIt.next()

    object ReverseIterator:
      private[ArrayTree] inline def apply[A: ArrayVarHandle](from: MultiLeaf[A], leftSize: TSize): ReverseIterator[A] =
        val fromSize = from.size
        new ReverseIterator(from, from.strongReverseIterator(fromSize.decr), leftSize + fromSize)

      private[ArrayTree] inline def from[A: ArrayVarHandle](
          from: MultiLeaf[A],
          fromIndex: Index,
          leftSize: TSize
      ): ReverseIterator[A] =
        new ReverseIterator(from, from.strongReverseIterator(fromIndex), (leftSize + fromIndex).incr)

    class ReverseIteratorWithIndex[A: ArrayVarHandle](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
        extends AbstractReverseIterator[A, (A, TIndex)](from, fromIt, total):
      protected def nextResult(): (A, TIndex) = currentIt.next() -> index

    object ReverseIteratorWithIndex:
      private[ArrayTree] inline def apply[A: ArrayVarHandle](
          from: MultiLeaf[A],
          leftSize: TSize
      ): ReverseIteratorWithIndex[A] =
        val fromSize = from.size
        new ReverseIteratorWithIndex(from, from.strongReverseIterator(fromSize.decr), leftSize + fromSize)

      private[ArrayTree] inline def from[A: ArrayVarHandle](
          from: MultiLeaf[A],
          fromIndex: Index,
          leftSize: TSize
      ): ReverseIteratorWithIndex[A] =
        new ReverseIteratorWithIndex(from, from.strongReverseIterator(fromIndex), (leftSize + fromIndex).incr)

  private object weaklyConsistent:
    abstract protected class AbstractReverseIterator[A: ArrayVarHandle, B](
        from: MultiLeaf[A],
        fromIt: Iterator[A],
        total: TSize
    ) extends ArrayTree.AbstractReverseIterator[A, B](from, fromIt, total):
      protected def multiLeafReverseIterator(multi: MultiLeaf[A]): Iterator[A] =
        MultiLeaf.WeaklyConsistentReverseIterator(multi.elems, multi._used - 1)

    class ReverseIterator[A: ArrayVarHandle](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
        extends AbstractReverseIterator[A, A](from, fromIt, total):
      protected def nextResult(): A = currentIt.next()

    object ReverseIterator:
      private[ArrayTree] inline def apply[A: ArrayVarHandle](from: MultiLeaf[A], leftSize: TSize): ReverseIterator[A] =
        val fromSize = from.size
        new ReverseIterator(from, from.weakReverseIterator(fromSize.decr), leftSize + fromSize)

      private[ArrayTree] inline def from[A: ArrayVarHandle](
          from: MultiLeaf[A],
          fromIndex: Index,
          leftSize: TSize
      ): ReverseIterator[A] =
        new ReverseIterator(from, from.weakReverseIterator(fromIndex), (leftSize + fromIndex).incr)

    class ReverseIteratorWithIndex[A: ArrayVarHandle](from: MultiLeaf[A], fromIt: Iterator[A], total: TSize)
        extends AbstractReverseIterator[A, (A, TIndex)](from, fromIt, total):
      protected def nextResult(): (A, TIndex) = currentIt.next() -> index

    object ReverseIteratorWithIndex:
      private[ArrayTree] inline def apply[A: ArrayVarHandle](
          from: MultiLeaf[A],
          leftSize: TSize
      ): ReverseIteratorWithIndex[A] =
        val fromSize = from.size
        new ReverseIteratorWithIndex(from, from.weakReverseIterator(fromSize.decr), leftSize + fromSize)

      private[ArrayTree] inline def from[A: ArrayVarHandle](
          from: MultiLeaf[A],
          fromIndex: Index,
          leftSize: TSize
      ): ReverseIteratorWithIndex[A] =
        new ReverseIteratorWithIndex(from, from.weakReverseIterator(fromIndex), (leftSize + fromIndex).incr)

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
    * @param fairLock whether the `ReentrantReadWriteLock` used internally should be fair.
    *                 Choose `true` only if you are concerned about delays due to continuous contention.
    */
  final case class Config(
      initialCap: Capacity,
      leafCap: Log2Capacity,
      nodeCap: Log2Capacity,
      fairLock: Boolean = false
  ):
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
    /** Suggests a `Config` based on expected sizes of some non-empty collection.
      *
      * @param expectedSize20thPercentile along with the absolute size estimate, this value determines `initialCap`.
      *                                   With bigger sizes, an `initialCap` below this value will be chosen.
      * @param expectedSize90thPercentile along with `expectedSize20thPercentile`, this value is used to suggest
      *                                   `leafCap` and `nodeCap` such that smaller collections, and collections
      *                                   with a smaller difference between 20th and 90th percentiles, will have
      *                                   a lower tree depth.
      *                                   This estimated size must be greater than that for the 20th percentile.
      * @param lean if `false` (default), bigger leaves are favored.
      *             If `true`, `initialCap` and `leafCap` get smaller to save RAM.
      * @param fairLock see constructor.
      */
    def fromRange[A](
        expectedSize20thPercentile: PositiveSize,
        expectedSize90thPercentile: PositiveSize,
        lean: Boolean = false,
        fairLock: Boolean = false
    ): Config =
      require(expectedSize20thPercentile < expectedSize90thPercentile)
      import math.{log, max, round}
      inline val log2 = 0.69314718

      val initialCap: Capacity =
        if lean then expectedSize20thPercentile.mapTrusted(v => max(v >> 1, 1))
        else expectedSize20thPercentile

      val leafCap: Log2Capacity =
        Log2Capacity.trust {
          val expansion = (expectedSize90thPercentile - initialCap) max Positive(1)
          val rawCap    = 0.4 * log(expansion.value) / log2
          val incr: Int = if lean then 0 else 1
          round((rawCap + incr).toFloat).toByte
        } max PositiveLog2Value(3)

      val nodeCap: Log2Capacity =
        Log2Capacity.trust {
          val rawCap    = 2 * log(leafCap.value) / log2
          val decr: Int = if lean then 1 else 0
          round((rawCap - decr).toFloat).toByte
        } max PositiveLog2Value(3)

      Config(initialCap, leafCap, nodeCap, fairLock)

    /** Suggests a `Config` based on the mean size of some non-empty collection.
      *
      * @param expectedMean the expected mean size
      * @param expectedSpread Factor for the deviation in percent, 100 at most.
      *                       The default value is 50 meaning, that 90% of the sizes
      *                       are between 50% and 150% of the `expectedMean`.
      */
    def fromMean[A](
        expectedMean: PositiveSize,
        expectedSpread: Positive = Positive(50),
        lean: Boolean = false,
        fairLock: Boolean = false
    ): Config = {
      require(expectedSpread <= Positive(100))

      def expansion(percent: Float): PositiveSize =
        expectedMean.mapTrusted { m =>
          (m * percent * expectedSpread.value / 100).toInt
        }

      val p20 =
        inline val mimicP20 = 3f / 5
        expectedMean minusOrLimit expansion(mimicP20)
      val p90 =
        inline val mimicP90 = 4f / 5
        expectedMean plusOrLimit expansion(mimicP90)

      fromRange(p20, p90, lean, fairLock)
    }

    private type Location = (slot: Index, subIndex: Index, leftSize: TSize)

    /** Capacities per tree level. */
    sealed protected[concurrent] trait LevelCap:
      def first: Capacity

      def locate[U](i: Index, leftSide: Boolean): Location

      protected def locate[U](i: Index, leftSide: Boolean, subsequent: Log2Capacity): Location =
        import scalax.util.primitives.PositiveLog2ValueOverNonNegative.*
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
      import scalax.util.primitives.PositiveLog2ValueOverPositive.*

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

  def newBuilder[A](using config: Config)(using ClassTag[A], ArrayVarHandle[A]): Builder[A, ArrayTree[A]] =
    new Builder[A, ArrayTree[A]]:
      private var buf = empty[A]

      inline def clear(): Unit              = buf = empty
      inline def result(): ArrayTree[A]     = buf
      inline def addOne(elem: A): this.type = { buf append elem; this }

      override def addAll(elems: IterableOnce[A]): this.type =
        if buf.isEmpty then apply(elems)
        else elems.iterator foreach addOne
        this

      override def sizeHint(size: Int): Unit = config.initialCap.value
