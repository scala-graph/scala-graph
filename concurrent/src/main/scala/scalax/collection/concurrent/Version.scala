package scalax.collection.concurrent

import scalax.util.primitives.{Limited, Validated}

object Version:
  /** 64-bit encoding of two unsigned integers to represent
    *   - a branch ID and
    *   - a revision within the branch.
    */
  protected[concurrent] opaque type Version = Long

  protected[concurrent] object Version:
    def apply(branchId: BranchId, revision: Revision): Version =
      (branchId.toLong << Revision.length) | revision

    def unapply(version: Version): (BranchId, Revision) =
      version.branchId -> version.revision

    inline def first: Version = Version(BranchId.first, Revision.first)

    inline def first(branchId: BranchId): Version =
      Version(branchId, Revision.first)

    import Revision.given
    extension (version: Version)
      def branchId: BranchId = (version >>> Revision.length).toInt
      def revision: Revision = version & Revision.upperLimit

      /** @throws LimitOverflowException if the revision exceeds its `upperLimit`. */
      inline def nextRevision: Version =
        Version(version.branchId, version.revision.incr)

  /** 28 bit, verified, non-negative `Int` up to `268,435,455`. */
  protected[concurrent] opaque type BranchId = Int

  protected[concurrent] object BranchId extends Validated[Int, BranchId]:
    inline val length = 28
    inline def lowerLimit = 0
    inline def upperLimit = 0xFFF_FFFF

    inline def min: BranchId = lowerLimit
    inline def max: BranchId = upperLimit

    inline def first: BranchId = min

    protected inline def fromValid(a: Int): BranchId = a
    protected inline def valid(a: Int): Boolean = a >= lowerLimit && a <= upperLimit
    protected inline def errMsgSuffix: String = " is invalid for BranchId"

    given Limited[Int, BranchId] with
      inline def lowerLimit: BranchId = BranchId.lowerLimit
      inline def upperLimit: BranchId = BranchId.upperLimit

      inline def lt(a: BranchId, b: BranchId): Boolean = a < b
      protected[scalax] inline def underlying(a: BranchId): Int = a
      protected inline def fromValid(a: Int): BranchId = a
      protected inline def validIncrement(a: Int): Int = a + 1
      protected inline def unsafeAdd(a: Int, b: Int): Int = a + b

  extension(branchId: BranchId)(using limited: Limited[Int, BranchId])
    protected[concurrent] inline def underlying: Int = limited.underlying(branchId)

    inline def <(other: BranchId): Boolean = limited.lt(branchId, other)
    inline def incr: BranchId = limited.incr(branchId)
    inline def +(summand: BranchId): BranchId = limited.added(branchId, summand)

  /** 36 bit, verified, non-negative `Long` up to `68,719,476,735`. */
  protected[concurrent] opaque type Revision = Long

  protected[concurrent] object Revision extends Validated[Long, Revision]:
    inline val length = 36
    inline def lowerLimit = 0L
    inline def upperLimit = 0xF_FFFF_FFFFL

    inline def min: Revision = lowerLimit
    inline def max: Revision = upperLimit

    inline def first: Revision = min

    protected inline def fromValid(a: Long): Revision = a
    protected inline def valid(a: Long): Boolean = a >= lowerLimit && a <= upperLimit
    protected inline def errMsgSuffix: String = " is invalid for RevisionId"

    given Limited[Long, Revision] with
      inline def lowerLimit: Revision = Revision.lowerLimit
      inline def upperLimit: Revision = Revision.upperLimit

      inline def lt(a: Revision, b: Revision): Boolean = a < b
      protected[scalax] inline def underlying(a: Revision): Long = a
      protected inline def fromValid(a: Long): Revision = a
      protected inline def validIncrement(a: Long): Long = a + 1
      protected inline def unsafeAdd(a: Long, b: Long): Long = a + b

  extension(revision: Revision)(using limited: Limited[Long, Revision])
    protected[concurrent] inline def underlying: Long = limited.underlying(revision)

    inline def <(other: Revision): Boolean = limited.lt(revision, other)
    inline def incr: Revision = limited.incr(revision)
    inline def +(summand: Revision): Revision = limited.added(revision, summand)
