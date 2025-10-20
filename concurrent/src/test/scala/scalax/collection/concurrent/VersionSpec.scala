package scalax.collection.concurrent

import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class VersionSpec extends RefSpec with Matchers with OptionValues:
  import Version.*

  def `BranchId `: Unit =
    "BranchId(-1)" shouldNot compile
    BranchId.from(BranchId.lowerLimit).value shouldBe BranchId.lowerLimit
    BranchId(777_777)
    BranchId.from(BranchId.upperLimit).value shouldBe BranchId.upperLimit
    BranchId.from(BranchId.upperLimit + 1) shouldBe empty

  def `Revision `: Unit =
    "Revision(-1)" shouldNot compile
    Revision.from(Revision.lowerLimit).value shouldBe Revision.lowerLimit
    Revision(8_888_888_888L)
    Revision.from(Revision.upperLimit).value shouldBe Revision.upperLimit
    Revision.from(Revision.upperLimit + 1) shouldBe empty

  private val aBranchId   = BranchId(777_777)
  private val maxBranchId = BranchId.max

  private val aRevision   = Revision(8_888_888_888L)
  private val maxRevision = Revision.max

  def `lengths `: Unit =
    BranchId.length + Revision.length shouldBe 64

  def `apply, extractors `: Unit =
    "Version(1, 0)" shouldNot compile
    Version(aBranchId, aRevision)
    "val v: Long = Version(aBranchId, aRevision)" shouldNot compile

    def check(branchId: BranchId, revision: Revision): Unit =
      val version = Version(branchId, revision)
      version.branchId shouldBe branchId
      version.revision shouldBe revision

    check(aBranchId, aRevision)
    check(maxBranchId, maxRevision)

  def `unapply `: Unit =
    val version = Version(aBranchId, aRevision)
    version match
      case Version(b, r) =>
        b shouldBe aBranchId
        r shouldBe aRevision

  def `first `: Unit =
    Version.first.branchId shouldBe BranchId.min
    Version.first.revision shouldBe Revision.min

    val first = Version.first(aBranchId)
    first.branchId shouldBe aBranchId
    first.revision shouldBe Revision.min

  def `next revision`: Unit =
    val current = Version(aBranchId, aRevision)
    val next    = current.nextRevision
    current.branchId shouldBe next.branchId
    next.revision shouldBe aRevision.incr
