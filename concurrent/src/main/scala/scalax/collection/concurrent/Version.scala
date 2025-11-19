package scalax.collection.concurrent

import scalax.util.primitives.{LimitedInt, LimitedLong}

object Version:
  /** 64-bit encoding of two unsigned integers to represent
    *   - a branch ID and
    *   - a revision within the branch.
    */
  protected[concurrent] opaque type Version = Long

  protected[concurrent] object Version:
    def apply(branchId: BranchId, revision: Revision): Version =
      (branchId.toLong << Revision.length) | revision

    inline def unapply(version: Version): (BranchId, Revision) =
      version.branchId -> version.revision

    inline def first: Version = Version(BranchId.first, Revision.first)

    inline def first(branchId: BranchId): Version =
      Version(branchId, Revision.first)

    import Revision.*
    extension (version: Version)
      def branchId: BranchId        = (version >>> Revision.length).toInt
      inline def revision: Revision = version & Revision.upperLimit

      /** @throws LimitOverflowException if the revision exceeds its `upperLimit`. */
      inline def nextRevision: Version =
        Version(version.branchId, version.revision.incr)

  /** 28 bit, verified, non-negative `Int` up to `268,435,455`. */
  protected[concurrent] opaque type BranchId = Int

  protected[concurrent] object BranchId extends LimitedInt[BranchId]:
    inline val length     = 28
    inline def lowerLimit = 0
    inline def upperLimit = 0xfff_ffff

    inline def min: BranchId = lowerLimit
    inline def max: BranchId = upperLimit

    inline def first: BranchId = min

    protected inline def valid(a: Int): Boolean = a >= lowerLimit && a <= upperLimit
    protected inline def errMsgSuffix: String   = " is invalid for BranchId"

    given LimitedInt[BranchId] = BranchId

  /** 36 bit, verified, non-negative `Long` up to `68,719,476,735`. */
  protected[concurrent] opaque type Revision = Long

  protected[concurrent] object Revision extends LimitedLong[Revision]:
    inline val length     = 36
    inline def lowerLimit = 0L
    inline def upperLimit = 0xf_ffff_ffffL

    inline def min: Revision = lowerLimit
    inline def max: Revision = upperLimit

    inline def first: Revision = min

    protected inline def valid(a: Long): Boolean = a >= lowerLimit && a <= upperLimit
    protected inline def errMsgSuffix: String    = " is invalid for RevisionId"

    given LimitedLong[Revision] = Revision
