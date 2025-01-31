package scalax.collection.concurrent

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class VectorArraySeqSpec extends RefSpec with Matchers:
  import VectorArraySeq._

  def `chunk size based on hints`(): Unit =
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
