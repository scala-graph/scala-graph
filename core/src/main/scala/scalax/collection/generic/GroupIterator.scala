package scalax.collection.generic

/** Iterator facilitating group-wise iteration over two ore more levels.
  * Each level is represented by an inner iterator. Interactions between these
  * levels are handled internally thus the implementor just needs to define
  * the individual level iterators. Level iterator implementations will typically be
  * inner objects inheriting from the public inner traits of this trait.
  *
  * This trait is not graph-specific.
  *
  * @tparam A Simple or composite type of elements being subject to the iteration.
  *         For instance, a two-level iteration could expose just the elements of
  *         the inner loop or both the outer and the inner loop elements as a tuple.
  * @define LAZY To avoid an infinite loop on initialization time, `lazy` should be
  *         used when overriding.
  * @author Peter Empen
  */
trait GroupIterator[A] extends Iterator[A] {

  /** Called on the first time when `hasNext` of the innermost iterator returns
    * `false`. The default implementation does nothing. */
  def onExit {}

  sealed protected trait LevelIterator[A] extends Iterator[A] {

    /** Type of internal level elements iterated over as it may be different from `A`. */
    protected type I

    /** Internal level-specific iterator. When referencing iterators take care to
      *  override with `val`. */
    protected def levelIterator: Iterator[I]

    /** Current element of `iterator` converted to `A`. This variable will be set
      *  by the predefined level-specific implementations of `hasNext` so the
      *  user just needs to formally initialize it to any value. */
    protected var _current: A = _

    /** Converts the current element of the level-specific `iterator` to `A`. */
    protected def elmToCurrent(elm: I): A

    /** Optional cashed hasNext result to avoid repeated calculations. */
    protected var optHasNext: Option[Boolean] = None

    /** The next element of `this` iterator also cashed in `current`. */
    def next: A =
      if (hasNext) {
        optHasNext = None
        _current
      } else throw new java.util.NoSuchElementException

    /** The value returned by the last call of `next`. */
    def current: A = _current
  }
  sealed protected trait OutermostIteratorDecl {
    protected type InnerElm

    /** $LAZY */
    protected val inner: InnermostIterator[InnerElm]
  }

  /** Iterator for the top level group. For instance, if we iterate over cities
    * grouped by countries, the implementation of this trait will correspond to the
    * country iterator. */
  trait OutermostIterator[A] extends LevelIterator[A] with OutermostIteratorDecl {

    /** Cashed implementation of `hasNext` which also initializes the inner
      *  iterator bound to this outer iterator. */
    def hasNext: Boolean = optHasNext getOrElse {
      val has = levelIterator.hasNext
      optHasNext = Some(has)
      if (has) {
        _current = elmToCurrent(levelIterator.next)
        inner.init
        true
      } else false
    }
  }
  sealed protected trait InnermostIteratorDecl {
    protected type OuterElm

    /** $LAZY */
    protected val outer: OutermostIterator[OuterElm]
    protected def onOuterChange(newOuter: OuterElm): Unit
  }

  /** Iterator for the bottom level group. For instance, if we iterate over cities
    * grouped by countries, the implementation of this trait will correspond to the
    * city iterator. */
  trait InnermostIterator[A] extends LevelIterator[A] with InnermostIteratorDecl {
    final protected[GroupIterator] def init {
      onOuterChange(outer.current)
      optHasNext = None
    }

    /** Cached implementation of `hasNext` which also skips in the outer iterator
      * bound to this inner iterator if `this` iterator has been exhausted. */
    def hasNext: Boolean = optHasNext getOrElse {
      while (outer.hasNext) {
        val has = levelIterator.hasNext
        optHasNext = Some(has)
        if (has) {
          _current = elmToCurrent(levelIterator.next)
          return true
        } else outer.next
      }
      onExit
      false
    }
  }

  /** Iterator for mid level groups. For instance, if we iterate over cities
    * grouped by countries 'and' continents, the implementation of this trait
    * will correspond to the country iterator. */
  trait MidIterator[A, I, O] extends LevelIterator[A] with OutermostIteratorDecl with InnermostIteratorDecl {

    /** Cached implementation of `hasNext` which is also responsible for
      *  a) skipping in the outer iterator bound to this mid-level iterator if
      *     `this` iterator has been exhausted and
      *  b) initializing the inner iterator bound to this mid-level iterator. */
    override def hasNext: Boolean = optHasNext getOrElse {
      while (outer.hasNext) {
        val has = levelIterator.hasNext
        optHasNext = Some(has)
        if (has) {
          _current = elmToCurrent(levelIterator.next)
          inner.init
          return true
        } else outer.next
      }
      false
    }
  }
}
