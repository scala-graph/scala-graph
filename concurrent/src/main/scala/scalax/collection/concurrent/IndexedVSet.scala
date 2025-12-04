package scalax.collection.concurrent

//import scala.collection.immutable.IntMap

private[concurrent] type HashCode = Int

/** Set of versioned elements that can be accessed by index.
  * The index starts with 0 and will be incremented on `incl`.
  *
  * @tparam A type of the elements.
  * @tparam F type of the version family. A value of type `V` is valid in the context of a given value of type `F`.
  *           The version family is stored at chunk level.
  * @tparam V type of version stored to each element. Version typically includes branch information.
  */
/*
final private[collection] class IndexedVSet[A, F, @specialized(Long) V](
    elems: VIndexedSeq[(A, HashCode, V), F],
    hashCodeToIndex: IntMap[Set[Index]],
    nextVersion: V => (Option[F], V)
):
  def apply(i: Index): A                                           = ???
  def apply(elem: A, currentFamily: F, currentVersion: V): Boolean = ???

  def iterator(incl: (F, V) => Boolean): Iterator[A] = ???

  def incl(elem: A, currentFamily: F, currentVersion: V): IndexedVSet[A, F, V] =
    ???
//    if contains(elem) then
//      if
//    else
  def excl(elem: A, from: V): IndexedVSet[A, F, V] = ???
 */
