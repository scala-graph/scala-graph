package scalax.collection.concurrent

import scala.util.chaining._

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class VectorArraySeqSpec extends RefSpec with Matchers:
  import VectorArraySeq._

  private def tinySeq[A, C] =
    VectorArraySeq.empty[A, C](2) tap (_.chunkSize shouldBe MinChunkSize)

  def `'chunkSize' based on hints`(): Unit =
    List(
      10        -> (MinChunkSize, MinChunkSize),
      300       -> (MinChunkSize, 32),
      8_000     -> (64, 128),
      150_000   -> (256, 512),
      2_000_000 -> (1_024, 2_048)
    ) foreach { case sizeHint -> (expected @ (expectedMinChunkSize, expectedMaxChunkSize)) =>
      val normal = chunkSize(sizeHint, heavyUtilization = false)
      // info(s"$sizeHint -> $calculated which is between $expected")

      normal should (be >= expectedMinChunkSize and be <= expectedMaxChunkSize)
      if normal > MinChunkSize then chunkSize(sizeHint, heavyUtilization = true) should (be < normal)
    }

  def `'appended' within chunk, single-threaded`(): Unit =
    given prefix: Int = 0
    val elem          = 0
    var seq           = tinySeq[elem.type, Int]
    (1 to seq.chunkSize) foreach { i =>
      seq = seq :+ elem
      seq.size shouldBe i
      seq.elem(i - 1) shouldBe elem
    }
    seq.iterator.length shouldBe seq.chunkSize
