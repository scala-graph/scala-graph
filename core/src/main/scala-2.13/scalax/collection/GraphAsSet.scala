package scalax.collection

import language.higherKinds

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.generic.GraphCompanion
import scala.collection.{SetOps, mutable}
import scala.reflect.ClassTag

private[collection] trait GraphAsSet[N,
                                     E[+X] <: EdgeLikeIn[X],
                                     +This[NN, EE[+XX] <: EdgeLikeIn[XX]] <: GraphLike[NN, EE, This] with AnySet[
                                       Param[NN, EE]] with Graph[NN, EE]]
    extends AnySet[Param[N, E]]
    with SetOps[Param[N, E], AnySet, This[N, E]] {
  this: This[N, E] with GraphAsSet[N, E, This] with AnySet[Param[N, E]] with Graph[N, E] =>

  /** The companion object of `This`. */
  val graphCompanion: GraphCompanion[This]

  override def empty: This[N, E]                                                      = graphCompanion.empty[N, E]
  override protected def fromSpecific(coll: IterableOnce[Param[N, E]]): This[N, E]    = graphCompanion.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[Param[N, E], This[N, E]] = graphCompanion.newBuilder

  protected def bulkOp(elems: IterableOnce[Param[N, E]], isPlusPlus: Boolean): This[N, E]

  override def concat(elems: IterableOnce[Param[N, E]]): This[N, E] = bulkOp(elems, isPlusPlus = true)
  override def diff(that: AnySet[Param[N, E]]): This[N, E]          = this -- that
  override def --(elems: IterableOnce[Param[N, E]]): This[N, E]     = bulkOp(elems, isPlusPlus = false)

  def map[NN, EE[+X] <: EdgeLikeIn[X]](f: Param[N, E] => Param[NN, EE])(
      implicit edgeT: ClassTag[EE[NN]]): This[NN, EE] =
    graphCompanion.from(super.map(f))(edgeT, config)
}
