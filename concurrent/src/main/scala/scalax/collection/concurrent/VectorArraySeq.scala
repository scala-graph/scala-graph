package scalax.collection.concurrent

import scala.math.max

private[concurrent] type Size = Int
private[concurrent] case class Chunk[A <: AnyRef, C](prefix: C, var size: Size, array: Array[A])

/** Growing only, chunked indexed sequence with concurrently mutable chunks.
  * `Vector` elements correspond to a chunk of constant length.
  *
  * @param chunkSize the size of chunks that sit at `Vector` elements
  * @tparam A type of elements
  * @tparam C type of chunk-level data
  */
final protected[concurrent] class VectorArraySeq[A <: AnyRef, C] private (chunkSize: Size, vector: Vector[Chunk[A, C]]):

  def chunkElem(i: Index): (C, A) = chunk(i) match {
    case Chunk(prefix, _, array) => (prefix, array(i % chunkSize))
  }

  def elem(i: Index): A = chunk(i).array(i % chunkSize)

  def chunk(i: Index): Chunk[A, C] = vector(i / chunkSize)

  /** Adds `elem` to this collection in a thread-safe way. */
  def addOne(elem: A)(implicit chunkPrefix: C): Index = ???

  /** Alias for `addOne`. */
  inline def +=(elem: A)(implicit chunkPrefix: C): Index = addOne(elem)

  /** Computes a new collection with the contents of this collection but the `chunk` at `c`. */
  def updated(c: Index, newChunk: (C, Array[A])): VectorArraySeq[A, C] = ???

private[concurrent] object VectorArraySeq:

  def empty[A <: AnyRef, C](sizeHint: Int, heavyUtilization: Boolean = false): VectorArraySeq[A, C] =
    new VectorArraySeq[A, C](chunkSize(sizeHint, heavyUtilization), Vector.empty[Chunk[A, C]])

  private[concurrent] val MinChunkSize = 16

  private[concurrent] def chunkSize(sizeHint: Int, heavyUtilization: Boolean): Int =
    import Integer.{highestOneBit, numberOfTrailingZeros}

    def approxSqrt(i: Int): Int        = 1 << (numberOfTrailingZeros(highestOneBit(i)) >> 1)
    def largestPowerOfTwo(i: Int): Int = highestOneBit(i)

    val normal = max(largestPowerOfTwo(approxSqrt(sizeHint)), MinChunkSize)
    if heavyUtilization then max(normal >> 2, MinChunkSize)
    else normal
