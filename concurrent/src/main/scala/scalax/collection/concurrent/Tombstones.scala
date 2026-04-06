package scalax.collection.concurrent

import java.lang.System.arraycopy
import java.util.Arrays.sort
import scala.language.implicitConversions
import scala.math.{max, min}
import scalax.collection.concurrent.ArrayTree.TIndex
import scalax.util.collection.MaxSizeView
import scalax.util.primitives.{Capacity, Index, NonNegative, PositiveLog2Value, Size}
import scalax.util.primitives.PositiveLog2ValueOverPositive.*
import IndexTIndex.*

import scala.annotation.tailrec

/** A concurrent, appendable-only, non-empty, limited-capacity, sorted map of `Index -> TIndex`
  * that is backed by a gap buffer of `Array[IndexTIndex]` where `IndexTIndex`, an opaque type over `Long`,
  * contains a packed `Index` and `TIndex`.
  *
  * Keys refer to the elements of some "positive" IndexedSeq, in specific ArrayTree.MultiLeaf elements,
  * to mark them as removed. Values refer to a snapshot of an `ArrayTree` from which point the element is
  * deemed removed.
  *
  * This map is sorted to facilitate O(log) look-ups and O(1) access by `ArrayTree` iterators.
  * The sorting itself is optimized to take O(log) time.
  *
  * The underlying array works with a gap for less element copying and less reallocation.
  * Whenever the capacity of the underlying array needs be increased `putIfAbsent` returns a new
  * instance, meaning that references to instances of this class are designed to be mutable.
  */
final protected[concurrent] class Tombstones private (
    elems: Array[IndexTIndex],
    private var gapFrom: Index,
    private var gapLen: Size,
    val maxCap: Capacity
):
  import Tombstones.*

  def capacity: Capacity = Capacity.trust(elems.length)
  def size: Size         = Size.trust(elems.length) - gapLen

  private[concurrent] type Gap = (from: Index, len: Size)
  private[concurrent] def gap: Gap = (gapFrom, gapLen)

  /** Inserts `key -> revision` in a thread-safe manner unless `key` is already present.
    * For an existing `key`, `revision` is not updated meaning the call results in a no-op.
    *
    * @return `Option` in case of success being `Some` if a new instance had to be created or
    *         `None` if the underlying array buffer could be used.
    *         `TIndex`, the existing `revision`, if `key` was already present. `revision`
    */
  def putIfAbsent(key: Index, revision: TIndex): Option[Tombstones] | TIndex =
    // TODO read state
    val gapFrom   = this.gapFrom
    val gapLen    = this.gapLen
    val elemsLen  = Capacity.trust(elems.length)
    val rightFrom = gapFrom + gapLen
    val newElem   = IndexTIndex(key, revision)

    inline def gapExists: Boolean   = gapLen > Size.zero
    inline def leftExists: Boolean  = gapFrom > Index.zero
    inline def rightExists: Boolean = gapExists && rightFrom < elemsLen.asNonNegative

    withLeftRightSearch(key: Index)(elems, gapFrom, gapLen) {
      case GapInsertion =>
        def insertRight(): Unit =
          elems(rightFrom.decrTrusted) = newElem

        def insertLeft(): Unit =
          elems(gapFrom) = newElem
          this.gapFrom = gapFrom.incrTrusted

        (leftExists, rightExists) match
          case (true, true) =>
            inline def leftDistance  = key - elems(gapFrom.decrTrusted).index
            inline def rightDistance = elems(rightFrom).index - key
            if leftDistance < rightDistance then insertLeft()
            else insertRight()
          case (true, false) => insertLeft()
          case (false, true) => insertRight()
          case _             => throw new IllegalStateException("Unexpected case since always non-empty.")

        this.gapLen = gapLen.decrTrusted
        None

      case searchResult: SearchResult @unchecked if gapExists =>
        searchResult.withFoundOrInsertionPoint(elems(_).tIndex) { insertionPoint =>
          inline def shiftElems(from: Index, len: Size, by: Int): Unit =
            arraycopy(elems, from.value, elems, from.value + by, len.value)

          if insertionPoint < gapFrom then
            inline def leftDistance =
              if insertionPoint == Index.zero then key
              else key - elems(insertionPoint.decrTrusted).index
            inline def rightDistance = elems(insertionPoint).index - key

            val len     = gapFrom - insertionPoint
            val shiftBy = if rightDistance == Size(1) || rightDistance < leftDistance then 1 else gapLen.value
            shiftElems(from = insertionPoint, len, shiftBy)

            elems(insertionPoint) = newElem
            this.gapFrom =
              if shiftBy == 1 then gapFrom.incrTrusted
              else gapFrom - len + Size(1)
          else
            require(insertionPoint > rightFrom)
            val shiftBy =
              if insertionPoint.value == elems.length then
                inline def leftDistance  = elems(rightFrom).index - elems(gapFrom.mapOrElseZero(_ - 1)).index
                inline def rightDistance = key - elems(insertionPoint.decrTrusted).index

                if leftDistance == Size(1) || rightDistance < leftDistance then -1
                else -gapLen.value
              else
                inline def leftDistance  = key - elems(insertionPoint.decrTrusted).index
                inline def rightDistance = elems(insertionPoint).index - key

                if leftDistance == Size(1) || leftDistance < rightDistance then -1
                else -gapLen.value
            val len = insertionPoint - rightFrom
            shiftElems(from = rightFrom, len, shiftBy)

            elems(insertionPoint.decrTrusted) = newElem
            if shiftBy < -1 then this.gapFrom = gapFrom + len

          this.gapLen = gapLen.decrTrusted
          None
        }

      case searchResult: SearchResult @unchecked /* buffer exhausted */ =>
        searchResult.withFoundOrInsertionPoint(elems(_).tIndex) { insertionPoint =>
          if elemsLen < maxCap then
            val newLen   = nextCap(elemsLen, maxCap)
            val newElems = new Array[IndexTIndex](newLen.value)

            given Conversion[Index, Int] = (index: Index) => index.value
            arraycopy(elems, 0, newElems, 0, insertionPoint)
            newElems(insertionPoint) = newElem
            val remaining = elemsLen.asNonNegative - insertionPoint
            arraycopy(elems, insertionPoint, newElems, newLen.asNonNegative - remaining, remaining)

            Some(
              new Tombstones(
                newElems,
                insertionPoint.incrTrusted,
                (newLen - elemsLen).asNonNegative.decrTrusted,
                maxCap
              )
            )
          else throw new IllegalStateException("Unexpected putIfAbsent on exhausted maxCap.")
        }
    }

  def get(key: Index): Option[TIndex] =
    withLeftRightSearch(key: Index)(elems, gapFrom, gapLen) {
      case GapInsertion                          => None
      case searchResult: SearchResult @unchecked =>
        searchResult.withFoundOrInsertionPoint(Some(_))(_ => None)
    }

  private def withLeftRightSearch[B](key: Index)(arr: Array[IndexTIndex], gapFrom: Index, gapLen: Size)(
      block: (SearchResult | GapInsertion.type) => B
  ): B =
    val arrayLen = capacity.asNonNegative
    if gapLen == Size.zero then block(search(key)(arr, Index.zero, arrayLen.decrTrusted))
    else
      def left: SearchResult | InspectRight.type =
        if gapFrom > Index.zero then
          val lastLeftIndex = gapFrom.decrTrusted
          val lastLeftElem  = arr(lastLeftIndex)
          val lastLeftKey   = lastLeftElem.index
          if lastLeftKey == key then SearchResult.foundAt(lastLeftElem.tIndex.value)
          else if lastLeftKey > key then search(key)(arr, Index.zero, lastLeftIndex)
          else InspectRight
        else InspectRight

      def right: SearchResult | GapInsertion.type =
        val rightFrom = gapFrom + gapLen
        if rightFrom < arrayLen then
          val firstRightElem = arr(rightFrom)
          val firstRightKey  = firstRightElem.index
          if firstRightKey == key then SearchResult.foundAt(firstRightElem.tIndex.value)
          else if firstRightKey < key then search(key)(arr, rightFrom, arrayLen.decrTrusted)
          else GapInsertion
        else GapInsertion

      left match
        case InspectRight                    => block(right)
        case result: SearchResult @unchecked => block(result)

  private[concurrent] def leftIterator: Iterator[IndexTIndex] =
    elems.view.slice(0, gapFrom.value).iterator

  private[concurrent] def rightIterator: Iterator[IndexTIndex] =
    val from = gapFrom + gapLen
    elems.view.slice(from.value, elems.length).iterator

  def iterator: Iterator[IndexTIndex] = leftIterator ++ rightIterator

  def keysIterator: Iterator[Index] = leftIterator.map(_.index) ++ rightIterator.map(_.index)

  def strongIterator: Iterator[IndexTIndex]  = ???
  def weakIterator: MaxSizeView[IndexTIndex] = ???

protected[concurrent] object Tombstones:

  private inline val GapInsertion = NonNegative.unused1
  private inline val InspectRight = NonNegative.unused2

  private inline def minCapIncrement = Capacity(8)
  private inline def capDevisor      = PositiveLog2Value(3)

  private inline def initialCap(maxCap: Capacity): Capacity =
    if maxCap > minCapIncrement then (minCapIncrement max (maxCap / capDevisor)) min maxCap
    else minCapIncrement min maxCap

  private def nextCap(current: Capacity, maxCap: Capacity): Capacity =
    val increment   = initialCap(maxCap)
    val incremented = increment + current
    if incremented + increment > maxCap then maxCap
    else incremented

  def apply(key: Index, TIndex: TIndex)(maxCap: Capacity): Tombstones =
    val cap   = initialCap(maxCap)
    val array = new Array[IndexTIndex](cap.value)
    new Tombstones(array, Index(1), cap.asNonNegative.decrTrusted, maxCap)

  /** Creates a `Tombstones` instance from `coll`.
    *
    * @param coll a non-empty iterable with unique `Index` part of the elements; it will be sorted if necessary.
    * @throws scala.IllegalArgumentException if coll is empty, or coll's Index part is not unique.
    */
  def from(coll: IterableOnce[IndexTIndex])(maxCap: Capacity): Tombstones =
    val it = coll.iterator
    require(it.nonEmpty, "Unexpected empty `coll`.")

    val len =
      val knownSize = coll.knownSize
      if knownSize == -1 then maxCap.value
      else min(max(knownSize + 4, initialCap(maxCap).value), maxCap.value)
    val arr = new Array[IndexTIndex](len)

    val (used, _, uniqueIndexes, sorted) =
      it.take(len).foldLeft(0, -1, true, true) { case ((i, prevIndex, unique, sorted), elem) =>
        arr(i) = elem
        val index = elem.index.value
        (i + 1, index, unique && index != prevIndex, sorted && index > prevIndex)
      }
    require(uniqueIndexes, "Unexpected duplicate index found.")
    if !sorted then sort(arr.asInstanceOf[Array[Long]], 0, used)

    new Tombstones(arr, gapFrom = Index.trust(used), gapLen = Size.trust(len - used), maxCap)
  end from

  opaque type SearchResult = Int

  object SearchResult:
    inline def foundAt(value: Int): SearchResult        = value
    inline def insertionPoint(value: Int): SearchResult = -(value + 1)

  extension (res: SearchResult)
    inline def toIndexOption: Option[Index] =
      if res >= 0 then Some(Index.trust(res))
      else None

    inline def withFoundOrInsertionPoint[A](inline withFound: Index => A)(inline withInsertionPoint: Index => A): A =
      if res >= 0 then withFound(Index.trust(res))
      else withInsertionPoint(Index.trust(-(res + 1)))

  private[concurrent] inline def linearUntil = Size(16)

  /** Searches for the `key` part of IndexTIndex in `arr` in the range [`from`, `to`].
    * Depending on the range, search is performed either linearly or binarily.
    *
    * @return see return of java.util.Arrays.binarySearch()
    */
  private[concurrent] def search(key: Index)(arr: Array[IndexTIndex], from: Index, to: Index): SearchResult =
    def linearSearch: Int =
      @tailrec def loop(i: Index): SearchResult =
        val currKey = arr(i).index
        if currKey == key then SearchResult.foundAt(i.value)
        else if key < currKey then SearchResult.insertionPoint(i.value)
        else if i < to then loop(i.incrTrusted)
        else SearchResult.insertionPoint(i.incrTrusted.value)

      loop(from)

    def binarySearch: Int =
      @tailrec def loop(low: Index, high: Int): SearchResult =
        if low.value <= high then
          val mid    = Index.trust((low.value + high) >>> 1)
          val midKey = arr(mid).index
          if midKey < key then loop(mid.incrTrusted, high)
          else if midKey > key then loop(low, mid.value - 1)
          else SearchResult.foundAt(mid.value)
        else SearchResult.insertionPoint(low.value)

      loop(from, to.value)

    if to - from < linearUntil then linearSearch else binarySearch
