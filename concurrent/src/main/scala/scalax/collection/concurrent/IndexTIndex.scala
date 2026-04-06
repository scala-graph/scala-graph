package scalax.collection.concurrent

import scalax.collection.concurrent.ArrayTree.TIndex
import scalax.util.primitives.Index

protected[concurrent] object IndexTIndex:
  /** 64-bit encoding of two `Index`es, resp. `Index` and `TIndex`, both opaque types over `Int`. */
  opaque type IndexTIndex = Long

  object IndexTIndex:
    inline def apply(index: Index, tIndex: TIndex): IndexTIndex =
      (index.value.toLong << 32) | tIndex.value

    inline def unapply(packed: IndexTIndex): (Index, TIndex) =
      packed.index -> packed.tIndex

    extension (packed: IndexTIndex)
      inline def value: Long = packed

      inline def high: Index = Index.trust((packed >>> 32).toInt)
      inline def low: Index  = Index.trust((packed & 0xffff_ffff).toInt)

      inline def index: Index   = high
      inline def tIndex: TIndex = low

  type IndexIndex = IndexTIndex
  val IndexIndex: IndexTIndex.type = IndexTIndex
