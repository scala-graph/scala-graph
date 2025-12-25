package scalax.collection.concurrent

import scala.compiletime.{codeOf, error}
import scalax.util.primitives.{Limited, LimitedIntImpl}

object Version:
  /** 64-bit encoding of two unsigned integers to represent
    *   - a branch ID and
    *   - a revision within the branch.
    * 4 bits unused.
    */
  protected[concurrent] opaque type Version = Long

  protected[concurrent] object Version:
    inline def apply(branchId: BranchId, revision: Revision): Version =
      (branchId.toLong << Revision.length) | revision

    inline def unapply(version: Version): (BranchId, Revision) =
      version.branchId -> version.revision

    inline def first: Version = Version(BranchId.first, Revision.first)

    inline def first(branchId: BranchId): Version =
      Version(branchId, Revision.first)

    import Revision.*
    extension (version: Version)
      inline def branchId: BranchId = (version >>> Revision.length).toInt
      inline def revision: Revision = (version & 0xffff_ffff).toInt

      /** @throws LimitOverflowException if the revision exceeds its `upperLimit`. */
      inline def nextRevision: Version =
        Version(version.branchId, version.revision.incr)

  /** 28 bit, verified, non-negative `Int` up to `268,435,455`. */
  protected[concurrent] opaque type BranchId = Int

  protected[concurrent] object BranchId extends Limited[Int, BranchId]:
    transparent inline def length     = 28
    transparent inline def lowerLimit = 0
    transparent inline def upperLimit = 0xfff_ffff

    inline def min: BranchId = lowerLimit
    inline def max: BranchId = upperLimit

    inline def first: BranchId = min

    inline def valid(a: Int): Boolean =
      a >= lowerLimit && a <= upperLimit

    final inline def apply(i: Int): BranchId =
      inline if valid(i) then i
      else error(codeOf(i) + " is invalid for BranchId.")

    private inline given Limited[Int, BranchId] = BranchId
    extension (n: BranchId)
      inline def <(b: BranchId): Boolean = n < b

      inline def incr: BranchId        = LimitedIntImpl.incr(n)
      inline def incrTrusted: BranchId = n + 1
      inline def decr: BranchId        = LimitedIntImpl.decr(n)
      inline def decrTrusted: BranchId = n - 1

  /** 32 bit, verified, non-negative `Int` up to `Int.MaxValue`. */
  protected[concurrent] opaque type Revision = Int

  protected[concurrent] object Revision extends Limited[Int, Revision]:
    transparent inline def length     = 32
    transparent inline def lowerLimit = 0
    transparent inline def upperLimit = Int.MaxValue

    inline def min: Revision = lowerLimit
    inline def max: Revision = upperLimit

    inline def first: Revision = min

    inline def valid(a: Int): Boolean =
      a >= lowerLimit && a <= upperLimit

    final inline def apply(i: Int): Revision =
      inline if valid(i) then i
      else error(codeOf(i) + " is invalid for Revision with upper limit" + upperLimit)

    private inline given Limited[Int, Revision] = Revision
    extension (n: Revision)
      inline def <(b: Revision): Boolean = n < b

      inline def incr: Revision        = LimitedIntImpl.incr(n)
      inline def incrTrusted: Revision = n + 1
      inline def decr: Revision        = LimitedIntImpl.decr(n)
      inline def decrTrusted: Revision = n - 1
