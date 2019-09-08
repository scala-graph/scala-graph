package scalax.collection
package mutable

import collection.SortedSet
import collection.mutable.{SetLike => MutableSetLike}
import collection.generic.{CanBuildFrom, GenericCompanion, GenericSetTemplate, MutableSetFactory}

import interfaces.ExtSetMethods

/** A growable and compactable `mutable.Set` implementation based on `Array` and `mutable.Set`.
  *  It switches to the latter representation as soon as a given threshold for the number of
  *  elements is reached. Thus this implementation is a kind of mixture of
  *  scala.collection.mutable{ResizableArray, Set, HashSet} aimed at increasing the performance
  *  of sets having up to 200 to 250 elements.
  *
  *  @define COLL `ArraySet`
  */
trait ArraySet[A]
    extends MSet[A]
    with GenericSetTemplate[A, ArraySet]
    with MutableSetLike[A, ArraySet[A]]
    with ExtSetMethods[A] {
  override def companion: GenericCompanion[ArraySet] = ArraySet

  /** Compacts the current representation depending on [[ArraySet.Hints.compactUpToUsed]]. */
  def compact: Unit
  def find(elem: A): Option[A]

  /** The hints valid for this [[ArraySet!]]. */
  def hints: ArraySet.Hints

  /** For `isArray == true`, the current capacity of the internal array else 0. */
  def capacity: Int

  /** Whether the internal representation is currently based on `Array`. */
  def isArray: Boolean

  protected[collection] def array: Array[A]
  protected[collection] def set: MSet[A]

  /** Adds `elem` without checking for duplicates. */
  protected[collection] def +=!(elem: A): this.type

  /** Updates or inserts `elem`.
    *  @return `true` if an insert took place. */
  protected[collection] def upsert(elem: A with AnyRef): Boolean

  /** Sorts this $COLL according to a comparison function in place.
    *  @see scala.collection.SeqLike */
  def sortWith(lt: (A, A) => Boolean): SortedSet[A] = sorted(Ordering fromLessThan lt)

  /** Sorts this $COLL according to the Ordering which results from transforming
    *  an implicitly given Ordering with a transformation function in place.
    *  @see scala.collection.SeqLike */
  def sortBy(f: A => A)(implicit ord: Ordering[A]): SortedSet[A] = sorted(ord on f)

  /** Sorts this $COLL according to an Ordering in place.
    *  @see scala.collection.SeqLike */
  def sorted(implicit ord: Ordering[A]): SortedSet[A]
}

object ArraySet extends MutableSetFactory[ArraySet] {

  /** Returns an empty set with default hints that can grow as expected. */
  override def empty[A]: ArraySet[A] = SimpleArraySet.empty[A]

  /** Returns an empty set with custom hints that can grow as expected. */
  def emptyWithHints[A](implicit hints: Hints): ArraySet[A] = SimpleArraySet.emptyWithHints[A](hints)

  /** Returns an empty set with built-in hints constructed from `size`. */
  def empty[A](size: Int): ArraySet[A] = SimpleArraySet.emptyWithHints[A] {
    val newSize = math.min(size, 200)
    Hints(initialCapacity = newSize, hashTableThreshold = newSize)
  }

  def apply[A](elem: A)(implicit hints: Hints): ArraySet[A] = emptyWithHints[A](hints) += elem

  def apply[A](elem1: A, elem2: A, elems: A*)(implicit hints: Hints): ArraySet[A] =
    emptyWithHints[A](hints) += elem1 += elem2 ++= elems
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ArraySet[A]] = setCanBuildFrom[A]

  /** Enables tuning of the internal representation of adjacency lists. Basically, an `Array`
    * representation is preferable over a hash table up to a node degree of about 200.
    *
    * @param initialCapacity The initial length of the internal `Array` representation. It
    *        should be chosen such that it's greater than the final `size` in a significant
    *        percentage of cases. The less heap space is a concern, the higher `initialCapacity`
    *        may be set.
    * @param capacityIncrement The size of free space to add to the `initialCapacity` whenever
    *        the size of this `Set` becomes greater than the `initialCapacity`.
    *        It should be chosen such that incrementing need not take place too often.
    * @param hashTableThreshold The internal representation of the adjacency list switches
    *        to a hash table when the number of edges at a given node exceeds this value.
    *        If both `initialCapacity` and `capacityIncrement` and this value are set to `0`
    *        a solely hash table representation will be used irrespective of the set `size`.
    *        This value should be close to the `size` limit an `Array` representation is more
    *        efficient on the JVM with regard to looking up a given element based on `==`
    *        as opposite to `hashCode`. Varying with JVM implementations/configurations this
    *        limit may come in somewhere between 10 and 30.
    * @param compactUpToUsed Compact the underlying `Array` only if it has at most used
    *        space of this percentage. Compression takes place only user-triggered by a call to
    *        `compact`. The higher this value the more often `compact` leads to a compression.
    *        `0` means never, `100` means always compact.
    */
  sealed trait Hints {
    def initialCapacity: Int
    def capacityIncrement: Int
    def hashTableThreshold: Int
    def compactUpToUsed: Int

    /** Returns a positive number > currentCapacity for an array or 0 for a hash table.
      */
    final def nextCapacity(currentCapacity: Int): Int = currentCapacity match {
      case c if c > 0 =>
        if (capacityIncrement > 0) {
          val n = c + capacityIncrement
          if (n > hashTableThreshold) 0 else n
        } else 0
      case _ if initialCapacity > 0   => initialCapacity
      case _ if capacityIncrement > 0 => capacityIncrement
      case _                          => 0
    }
    def propagate(fromSize: Int): Hints =
      if (fromSize > hashTableThreshold)
        ArraySet.Hints.HashOnly
      else
        this match {
          case c: CheckedHints       => c.copy(initialCapacity = fromSize)
          case d: Hints.Default.type => Hints(initialCapacity = fromSize)
          case h                     => h
        }
  }

  @SerialVersionUID(1L)
  case class CheckedHints private[ArraySet] (override val initialCapacity: Int,
                                             override val capacityIncrement: Int,
                                             override val hashTableThreshold: Int,
                                             override val compactUpToUsed: Int)
      extends Hints

  object Hints {

    /** Returns an instance of Hints with possibly corrected argument values.
      *  If no argument is supplied same as `Default`. */
    def apply(initialCapacity: Int = 16,
              capacityIncrement: Int = 32,
              hashTableThreshold: Int = 48,
              compactUpToUsed: Int = 80): Hints = {

      require(
        initialCapacity >= 0 &&
          capacityIncrement >= 0 &&
          hashTableThreshold >= initialCapacity &&
          hashTableThreshold >= 0 &&
          compactUpToUsed >= 0 && compactUpToUsed <= 100)

      val corr_HashTableThreshold = {
        val afterFirstIncr = initialCapacity + capacityIncrement
        if (afterFirstIncr > hashTableThreshold) afterFirstIncr
        else hashTableThreshold
      }
      CheckedHints(initialCapacity, capacityIncrement, corr_HashTableThreshold, compactUpToUsed)
    }

    /** Default hints equaling to Hints(16, 32, 48, 80) */
    case object Default extends Hints {
      val initialCapacity    = 16
      val capacityIncrement  = 32
      val hashTableThreshold = 48
      val compactUpToUsed    = 80
    }

    /** A special hint telling that the internal representation should always be
      * hash-based as opposite to `Array`. This is meaningful if the average `size` of the
      * `ArraySet` is above the limit an `Array` representation is more efficient. */
    case object HashOnly extends Hints {
      val initialCapacity    = 0
      val capacityIncrement  = 0
      val hashTableThreshold = 0
      val compactUpToUsed    = 0
    }
  }
}
