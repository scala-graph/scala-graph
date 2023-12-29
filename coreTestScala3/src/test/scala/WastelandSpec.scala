import scala.annotation.tailrec

import scalax.collection.OneOrMore
import scalax.collection.generic.AbstractDiHyperEdge
import scalax.collection.immutable.Graph

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

/** Directed hyperedge to represent nodes of the network https://adventofcode.com/2023/day/8.
  * Each line of the input file like `AAA = (BBB, CCC)` corresponds to one instance.
  */
case class Fork(source: String, leftTarget: String, rightTarget: String)
    extends AbstractDiHyperEdge[String](
      sources = OneOrMore.one(source),
      targets = OneOrMore(leftTarget, rightTarget)
    )

object Fork:
  def hook(source: String) = Fork(source, source, source)

type Wasteland = Graph[String, Fork]

enum Direction:
  case Left, Right

/** The claim is not to be complete but to demonstrate the use-case specific graph walk.
  */
def solve(wasteland: Wasteland, startingFork: String, instructions: Iterable[Direction]): Either[String, Int] =
  @tailrec def traverse(
      current: wasteland.NodeT,
      instructions: Iterator[Direction],
      count: Int
  ): Option[Int] =
    /** Uses inner node `fork` to navigate to the next node. Thus we can avoid repeated node lookup.
      */
    def target(fork: wasteland.NodeT, direction: Direction): Option[wasteland.NodeT] =
      fork.outgoing.headOption.map { e =>
        import Direction._
        direction match
          case Left  => e.targets(0)
          case Right => e.targets(1)
      }

    instructions.nextOption match
      case Some(instruction) =>
        target(current, instruction) match
          case Some(fork) => traverse(fork, instructions, count + 1)
          case None       => None
      case None => Some(count)

  wasteland.find(startingFork) match
    case Some(root) =>
      traverse(root, instructions.iterator, 0) match
        case Some(count) => Right(count)
        case None        => Left("Node without successors.")
    case None =>
      Left(s"Fork '$startingFork' does not exist in wasteland.")

final class WastelandSpec extends RefSpec with Matchers:

  /** @see diagram https://github.com/scala-graph/scala-graph/issues/300#issuecomment-1854583980
    */
  val wasteland: Wasteland = Graph.from(
    nodes = Nil,
    edges = List(
      Fork("A", "B", "C"),
      Fork("B", "D", "E"),
      Fork.hook("D"),
      Fork.hook("E"),
      Fork("C", "Z", "G"),
      Fork.hook("Z"),
      Fork.hook("G")
    )
  )

  def instructions(chars: String): Iterable[Direction] =
    import Direction._
    chars.flatMap {
      case 'L' | 'l' => Left :: Nil
      case 'R' | 'r' => Right :: Nil
      case _         => Nil
    }

  def `start at A`: Unit =
    solve(wasteland, "A", instructions("LR")) shouldBe Right(2)

  def `pass some non-existing fork`: Unit =
    solve(wasteland, "/", instructions("LR")).isLeft shouldBe true
