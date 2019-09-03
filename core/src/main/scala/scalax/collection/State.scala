package scalax.collection

import language.{higherKinds, postfixOps}

import GraphPredef.EdgeLikeIn
import mutable.ExtBitSet

/** Adds bit fields to the graph and its nodes facilitating fast storage and retrieval of
  *  traversal-specific flags as a decoupled implementation trait. These flags are often used
  *  to keep track of visited nodes.
  *  Traversals (algorithms) acquire handles by calling `withHandle`. Then,
  *  in `withHandle`'s code block, `node.visited` or `node.bit` is called with the supplied
  *  handle as an implicit parameter to set or get a node's flag.
  *
  *  @author Peter Empen
  */
protected trait State[N, E[+X] <: EdgeLikeIn[X]] {
  this: GraphTraversalImpl[N, E] =>

  import State._

  /** Flags in `inUse` refer to required but unclosed handles. */
  private val inUse = new FlagStore

  /** Flags in `dirty` refer to released handles with dirty flags at the nodes. */
  private val dirty   = new FlagStore
  private val monitor = new Object with Serializable

  protected def dump(store: FlagStore): ExtBitSet = {
    val words = store.flagsExt.cloneWords
    val dump  = new Array[Long](words.length + 1)
    dump(0) = store.flags
    Array.copy(words, 0, dump, 1, words.length)
    new ExtBitSet(dump)
  }

  /** Returns a copy of the current `inUse`-flags for dump purposes. */
  def dumpInUse: ExtBitSet = dump(inUse)

  /** Returns a copy of the current `dirty`-flags for dump purposes. */
  def dumpDirty: ExtBitSet = dump(dirty)

  /** Avoid calling this directly, prefer `withHandle` instead. */
  protected def nextHandle: Handle = monitor.synchronized {
    def clearNodes(hasDirtyExt: Boolean) {
      clearNodeStates(dirty.flags, if (hasDirtyExt) dirty.flagsExt else null)
      dirty.flags = 0L
      if (hasDirtyExt) dirty.flagsExt.clear
    }
    val free         = ~(inUse.flags | dirty.flags)
    val nrDirtyFlags = java.lang.Long.bitCount(dirty.flags)
    val newHandle: Handle =
      if (free != 0) {
        val nextFree = java.lang.Long.lowestOneBit(free)
        new Handle(singleWord, nextFree)
      } else if (nrDirtyFlags >= minBitsForClear) {
        clearNodes(dirty.flagsExt.headOption.nonEmpty)
        val nextFree = java.lang.Long.lowestOneBit(~inUse.flags)
        new Handle(singleWord, nextFree)
      } else
        inUse.flagsExt.onOrFindUnset(dirty.flagsExt) map { handle =>
          handle
        } getOrElse { // no free flag in flagsExt
          if (nrDirtyFlags + dirty.flagsExt.size >= minBitsForClearExt) {
            val handle = dirty.flagsExt.lowestOneBit.get
            clearNodes(true)
            handle
          } else { // we expand flagsExt
            val handle = new Handle(inUse.flagsExt.nrWords, 1L)
            handle
          }
        }
    inUse(newHandle) = true
    newHandle
  }

  /** Avoid calling this directly, prefer `withHandle` instead. */
  protected def releaseHandle(handle: Handle) = monitor.synchronized {
    inUse(handle) = false
    dirty(handle) = true
  }

  /** Executes a code block in the context of a new or reused state handler.
    *  @return The result of the code block executed.
    */
  protected def withHandle[T](reuse: Option[Handle] = None)(block: Handle => T): T = {
    val thisHandler = reuse getOrElse nextHandle
    val res         = block(thisHandler)
    if (reuse isEmpty) releaseHandle(thisHandler)
    res
  }

  /** Executes a code block in the context `nr` new state handlers
    *  or alternatively in the context of the state handlers `reuse`.
    *  @return The result of the code block executed.
    */
  protected def withHandles[T](nr: Int, reuse: Array[Handle] = Array.empty[Handle])(block: Array[Handle] => T): T = {
    val newHandlers = reuse isEmpty
    val theseHandles =
      if (newHandlers) Array.fill(nr)(nextHandle)
      else reuse
    val res = block(theseHandles)
    if (newHandlers) theseHandles foreach releaseHandle
    res
  }

  trait InnerNodeState {
    protected[State] var flags: FlagWord     = 0L
    protected[State] var flagsExt: FlagWords = null
    @inline final protected def withFlagsExt[T](block: (ExtBitSet) => T): T =
      block {
        if (flagsExt eq null) flagsExt = initFlagSet
        flagsExt
      }

    def dumpState: Array[Long] = {
      val dump = new Array[Long](1 + (if (flagsExt eq null) 0 else flagsExt.nrWords))
      dump(0) = flags
      if (flagsExt ne null)
        Array.copy(flagsExt.cloneWords, 0, dump, 1, flagsExt.nrWords)
      dump
    }

    @inline final protected[collection] def bit[T](implicit handle: Handle): Boolean =
      if (handle.index == singleWord) (flags & handle.mask) != 0L
      else withFlagsExt(_.apply(handle.index, handle.mask))

    /** Whether this node is marked as visited with respect to `handle`. */
    @inline final protected[collection] def visited(implicit handle: Handle): Boolean =
      bit(handle)

    @inline final protected[collection] def bit_=[T](isSet: Boolean)(implicit handle: Handle) {
      monitor.synchronized {
        if (handle.index == singleWord)
          flags =
            if (isSet) flags | handle.mask
            else flags & ~handle.mask
        else withFlagsExt(_.update(handle.index, handle.mask, isSet))
      }
    }

    /** Sets this node to `visited` with respect to to `handle`. */
    @inline final protected[collection] def visited_=(visited: Boolean)(implicit handle: Handle) {
      bit_=(visited)(handle)
    }
  }

  protected def clearNodeStates(flags: FlagWord, flagsExt: ExtBitSet) {
    val clear      = ~flags
    val doClearExt = flagsExt != null
    val clearExt   = if (doClearExt) ~flagsExt else null
    nodes foreach { n =>
      n.flags &= clear
      if (doClearExt && (n.flagsExt ne null)) n.flagsExt &= clearExt
    }
  }
}

object State {

  /** Word of flags, that is unit of bitwise boolean state information.
    *  These flags are mainly used to store whether a node counts as visited
    *  with respect to a given traversal where each traversal is represented by a `Handle`. */
  type FlagWord = Long
  protected val nrOfFlagWordBits = 64
  protected val minBitsForClear  = nrOfFlagWordBits >> 2

  /** Growable collection for storing bitwise boolean state information
    *  as an extension of `FlagsWord`. */
  type FlagWords = ExtBitSet
  protected val minBitsForClearExt = nrOfFlagWordBits
  require(minBitsForClear < minBitsForClearExt)

  /** state accessor with respect to a given traversal. */
  class Handle(val index: Int, val mask: FlagWord)
  val singleWord  = -1
  def emptyHandle = new Handle(singleWord, 0L)
  def initFlagSet = new ExtBitSet

  final protected class FlagStore(var flags: FlagWord = 0L, var flagsExt: ExtBitSet = initFlagSet) extends Serializable {

    /** Whether `store` is set with respect to `handle`. */
    def apply(handle: Handle): Boolean =
      if (handle.index == singleWord)
        (flags & handle.mask) != 0
      else
        flagsExt(handle.index, handle.mask)

    /** Sets `store` to `isSet` with respect to `handle`. */
    def update(handle: Handle, isSet: Boolean) {
      if (handle.index == singleWord)
        flags =
          if (isSet) flags | handle.mask
          else flags & ~handle.mask
      else
        flagsExt(handle.index, handle.mask) = isSet
    }
  }

  /** Dumps the state flags of a `node`. */
  def dump[N, E[+X] <: EdgeLikeIn[X]](node: Graph[N, E]#NodeT): ExtBitSet =
    node.containingGraph match {
      case g: State[_, _] =>
        node match {
          case n: g.InnerNode with g.InnerNodeState => new ExtBitSet(n.dumpState)
        }
    }

  /** Represents state flags of a graph for dump purposes. */
  class GraphDump(val inUse: ExtBitSet, val dirty: ExtBitSet) {
    override def toString = "inUse = %s%ndirty = %s%n".format(inUse.toString, dirty.toString)
  }

  /** Dumps the state flags of a `graph`. */
  def dump[N, E[+X] <: EdgeLikeIn[X]](graph: Graph[N, E]): GraphDump = graph match {
    case g: State[_, _] => new GraphDump(g.dumpInUse, g.dumpDirty)
  }
}
