package scalax.collection.concurrent

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

import scala.collection.mutable.Buffer

import scalax.collection.concurrent.ArrayTree.TIndex
import scalax.util.primitives.Index

class IndexTIndexSpec extends RefSpec with Matchers:
  import IndexTIndex.*

  private val pairs: List[(Index, TIndex)] =
    val indexes  = List(Index.zero, Index(77), Index(33_333), Index(Index.upperLimit))
    val tIndexes = List(TIndex.zero, TIndex(77), TIndex(33_333), TIndex(Index.upperLimit))

    indexes.foldLeft(List.empty[(Index, TIndex)]) { case acc -> index =>
      val buf = Buffer.empty[(Index, TIndex)]
      tIndexes.foreach(tIndex => buf += ((index, tIndex)))
      buf ++: acc
    }

  def `pack `: Unit =
    pairs foreach { case index -> tIndex =>
      IndexTIndex(index, tIndex) shouldBe a[Long]
    }

  def `unpack `: Unit =
    pairs foreach { case index -> tIndex =>
      IndexTIndex(index, tIndex) match
        case IndexTIndex(unpackedIndex, unpackedTIndex) =>
          unpackedIndex shouldBe index
          unpackedTIndex shouldBe tIndex
    }
