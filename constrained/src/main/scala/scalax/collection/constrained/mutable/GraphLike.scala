package scalax.collection.constrained
package mutable

import scalax.collection.GraphPredef.{EdgeLikeIn, Param}
import scalax.collection.constrained.PreCheckFollowUp._

import scala.collection.Set
import scala.collection.generic.{Growable, Shrinkable}
import scala.collection.mutable.Cloneable
import scala.language.postfixOps

trait GraphLike[N, E[+X] <: EdgeLikeIn[X], +This[X, Y[+X] <: EdgeLikeIn[X]] <: GraphLike[X, Y, This] with Graph[X, Y]]
    extends scalax.collection.mutable.GraphLike[N, E, This]
    with scalax.collection.constrained.GraphLike[N, E, This]
    with GraphOps[N, E, This]
    with Growable[Param[N, E]]
    with Shrinkable[Param[N, E]]
    with Cloneable[This[N, E]] {
  selfGraph: // This[N,E] => see https://youtrack.jetbrains.com/issue/SCL-13199
  This[N, E] with GraphLike[N, E, This] with Graph[N, E] =>

  trait NodeSet extends super.NodeSet {

    protected def checkedRemove(fake: NodeT, ripple: Boolean): Either[ConstraintViolation, Boolean] =
      nodes find fake map { node =>
        def remove = withoutChecks {
          subtract(node, ripple, minus, minusEdges)
        }
        if (checkSuspended) Right(remove)
        else {
          val preCheckResult = preSubtract(node.asInstanceOf[self.NodeT], ripple)
          preCheckResult.followUp match {
            case Complete => Right(remove)
            case PostCheck =>
              val incidentEdges = node.edges.toBuffer
              if (remove) {
                postSubtract(selfGraph, Set(node), Set.empty[E[N]], preCheckResult).fold(
                  failure => {
                    withoutChecks {
                      selfGraph addOne node.value
                      selfGraph ++= incidentEdges
                    }
                    Left(failure)
                  },
                  _ => Right(true)
                )
              } else Right(false)
            case Abort => Left(preCheckResult)
          }
        }
      } getOrElse Right(false)

    final override def remove(node: NodeT): Boolean       = remove_?(node) getOrElse false
    final override def removeGently(node: NodeT): Boolean = removeGently_?(node) getOrElse false

    def remove_?(fake: NodeT): Either[ConstraintViolation, Boolean]       = checkedRemove(fake, ripple = true)
    def removeGently_?(fake: NodeT): Either[ConstraintViolation, Boolean] = checkedRemove(fake, ripple = false)
  }

  override def +(node: N): This[N, E] = +?(node) getOrElse this

  def +?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = nodes contains Node(node),
      preAdd = preAdd(node),
      copy = clone += node,
      nodes = Set(node),
      edges = Set.empty
    )

  final override protected def +#(e: E[N]): This[N, E] = +#?(e) getOrElse this

  final protected def +#?(e: E[N]): Either[ConstraintViolation, This[N, E]] =
    checkedPlus(
      contained = edges contains Edge(e),
      preAdd = preAdd(e),
      copy = clone +=# e,
      nodes = Set.empty,
      edges = Set(e)
    )

  override def addAll(elems: TraversableOnce[Param[N, E]]): this.type = ++=?(elems) getOrElse this

  def ++=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type] = {
    def add: this.type = withoutChecks(super.addAll(elems))
    if (checkSuspended) Right(add)
    else {
      def process(elems: Iterable[Param[N, E]]): Option[ConstraintViolation] = {
        val (filteredElems, newNodes, newEdges) = {
          val p     = new Param.Partitions[N, E]((elems filterNot contains).toSet)
          val edges = p.toOuterEdges
          (
            p.toInParams.toSeq,
            nodesToAdd(p.toOuterNodes, edges),
            edges
          )
        }
        val preCheckResult = preAdd(filteredElems: _*)
        if (preCheckResult.abort) Some(preCheckResult)
        else {
          add
          if (preCheckResult.postCheck) {
            postAdd(this, newNodes, newEdges, preCheckResult).fold(
              failure => {
                withoutChecks {
                  newNodes foreach super.remove
                  newEdges foreach super.remove
                }
                Some(failure)
              },
              _ => None
            )
          } else None
        }
      }
      (elems match {
        case elems: Iterable[Param[N, E]] => process(elems)
        case traversableOnce              => process(traversableOnce.toSet)
      }).fold[Either[ConstraintViolation, this.type]](Right(this))(Left(_))
    }
  }

  final def -?(node: N): Either[ConstraintViolation, This[N, E]] =
    checkedMinusNode(node, forced = true, (outer: N, inner: NodeT) => { val c = clone; c -= outer; c })

  final override protected def -#(e: E[N]): This[N, E] = +#?(e) getOrElse this

  final protected def -#?(e: E[N]): Either[ConstraintViolation, This[N, E]] =
    checkedMinusEdge(e, simple = true, (outer: E[N], inner: EdgeT) => { val c = clone; c -=# outer; c })

  def --=?(elems: TraversableOnce[Param[N, E]]): Either[ConstraintViolation, this.type] = {
    val (outerNodes, outerEdges) = {
      val p = partition(elems)
      (p.toOuterNodes.toSet, p.toOuterEdges.toSet)
    }
    val (innerNodes, innerEdges) = (outerNodes map find flatten, outerEdges map find flatten)
    val preCheckResult =
      preSubtract(innerNodes.asInstanceOf[Set[self.NodeT]], innerEdges.asInstanceOf[Set[self.EdgeT]], true)
    preCheckResult.followUp match {
      case Complete => Right(withoutChecks(super.--=(elems)))
      case PostCheck =>
        val subtractables = (elems filter this.contains).toArray ++ innerNodes.flatMap(_.edges).toBuffer
        withoutChecks(super.--=(subtractables))
        postSubtract(this, outerNodes, outerEdges, preCheckResult).fold(
          failure => {
            withoutChecks(super.++=(subtractables))
            Left(failure)
          },
          _ => Right(this)
        )
      case Abort => Left(preCheckResult)
    }
  }
}
