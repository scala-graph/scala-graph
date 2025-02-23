package scalax.collection.concurrent

import java.util.concurrent.locks.ReentrantLock

import scala.math.max

private[concurrent] type Size = Int

private[concurrent] case class Chunk[A, C](prefix: C, var used: Size, array: Array[A]):
  def elem(globalIndex: Index, chunkSize: Size): A =
    val i = globalIndex % chunkSize
    if i < used then array(i)
    else throw new IndexOutOfBoundsException

private[concurrent] object Chunk:
  def empty[A, C](prefix: C)(implicit size: Size): Chunk[A, C] =
    Chunk[A, C](prefix, 0, new Array[AnyRef](size).asInstanceOf[Array[A]])

  def apply[A, C](prefix: C, elem: A)(implicit size: Size): Chunk[A, C] =
    val arr = new Array[AnyRef](size).asInstanceOf[Array[A]]
    arr(0) = elem
    Chunk[A, C](prefix, 1, arr)

/** Growing only, chunked indexed sequence with concurrently mutable chunks.
  *
  * @param vector sequence of chunks  of constant length
  * @param chunkSize the size of chunks that sit at `vector`'s elements
  * @tparam A type of elements
  * @tparam C type of chunk-level data
  */
final protected[concurrent] class VectorArraySeq[A, C] private (vector: Vector[Chunk[A, C]])(implicit
    val chunkSize: Size
):

  private type This = VectorArraySeq[A, C]

  private def appended(chunk: Chunk[A, C]): This =
    new VectorArraySeq[A, C](vector :+ chunk)

  def chunkElem(i: Index): (C, A) =
    val c = chunk(i)
    c.prefix -> c.elem(i, chunkSize)

  def elem(i: Index): A = chunk(i).elem(i, chunkSize)

  def chunk(i: Index): Chunk[A, C] = vector(i / chunkSize)

  def size: Size =
    val chunkCount = vector.size
    if chunkCount > 0 then (chunkCount - 1) * chunkSize + vector.last.used
    else 0

  def lastIndex: Index = size - 1

  private val appending = new ReentrantLock()

  /** Adds `elem` to `this` or creates new instance with the elements of `this` plus `elem` in a thread-safe way. */
  def appended(elem: A)(implicit chunkPrefix: C): This =
    appending.lock()
    try
      vector.lastOption match
        case Some(last) if last.used < chunkSize && last.prefix == chunkPrefix =>
          last.array(size) = elem
          last.used += 1
          this
        case _ =>
          appended(Chunk(chunkPrefix, elem))
    finally
      appending.unlock()

  /** Alias for `appended`. */
  inline def :+(elem: A)(implicit chunkPrefix: C): This = appended(elem)

  /** Computes a new collection with the contents of this collection but the `chunk` at `c`. */
  def updated(c: Index, newChunk: (C, Array[A])): VectorArraySeq[A, C] =
    ???

  def iterator: Iterator[A] =
    vector.iterator.flatMap { case Chunk(_, used, arr) =>
      arr.iterator.slice(0, used)
    }

  private[concurrent] def trace: String =
    val v = vector.iterator
      .map { case Chunk(prefix, used, arr) =>
        s"""prefix: $prefix, used: $used, {${arr.map(elem => Option(elem).fold("null")(_.toString)) mkString ", "}}"""
      }
      .mkString("  ", "\n  ", "")
    s"""chunkSize: $chunkSize, size: $size, chunks:
       |$v""".stripMargin

private[concurrent] object VectorArraySeq:

  def empty[A, C](sizeHint: Size, writeIntensive: Boolean = false): VectorArraySeq[A, C] =
    new VectorArraySeq[A, C](Vector.empty[Chunk[A, C]])(chunkSize(sizeHint, writeIntensive))

  private[concurrent] val MinChunkSize = 16

  private[concurrent] def chunkSize(sizeHint: Size, heavyUtilization: Boolean): Int =
    import Integer.{highestOneBit, numberOfTrailingZeros}

    def approxSqrt(i: Int): Int        = 1 << (numberOfTrailingZeros(highestOneBit(i)) >> 1)
    def largestPowerOfTwo(i: Int): Int = highestOneBit(i)

    val normal = max(largestPowerOfTwo(approxSqrt(sizeHint)), MinChunkSize)
    if heavyUtilization then max(normal >> 2, MinChunkSize)
    else normal
