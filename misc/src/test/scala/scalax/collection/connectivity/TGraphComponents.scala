package scalax.collection.connectivity

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.connectivity.GraphComponents.graphToComponents
import scalax.collection.Graph
import org.scalatest.matchers.ShouldMatchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * Tests algorithms for finding graph components.
 * @author Vasco Figueira
 * @author Peter Empen
 */
@RunWith(classOf[JUnitRunner])
class TGraphComponentsTest extends FunSuite with ShouldMatchers {

  val graphs = List(
    Graph(
      'u', 'v', 'w', 'x', 'y', 'z', 'a',
      'v' ~> 'y',
      'w' ~> 'y',
      'w' ~> 'z',
      'z' ~> 'z',
      'y' ~> 'x',
      'x' ~> 'v',
      'u' ~> 'v',
      'u' ~> 'x'
    ), Graph( // same as before but inverted
      'u', 'v', 'w', 'x', 'y', 'z', 'a',
      'y' ~> 'v',
      'y' ~> 'w',
      'z' ~> 'w',
      'z' ~> 'z',
      'x' ~> 'y',
      'v' ~> 'x',
      'v' ~> 'u',
      'x' ~> 'u'
    ), Graph(
      'a' ~> 'd',
      'd' ~> 'b',
      'd' ~> 'c',
      'b' ~> 'c',
      'b' ~> 'e',
      'e' ~> 'g',
      'g' ~> 'd',
      'f' ~> 'h',
      'h' ~> 'f'
    ), Graph(
      'a' ~> 'b',
      'b' ~> 'a',
      'c' ~> 'b',
      'c' ~> 'f',
      'f' ~> 'c',
      'e' ~> 'f',
      'a' ~> 'e',
      'd' ~> 'g',
      'g' ~> 'd',
      'g' ~> 'i',
      'i' ~> 'h',
      'h' ~> 'i',
      'h' ~> 'd'
    )
  )

  test("a graph's SCCs") {
    val expectedSet = Array(
      Set(
        Set('x', 'y', 'v'),
        Set('u'),
        Set('w'),
        Set('z'),
        Set('a')
      ), Set(
        Set('x', 'y', 'v'),
        Set('u'),
        Set('w'),
        Set('z'),
        Set('a')
      ), Set(
        Set('c'),
        Set('h', 'f'),
        Set('d', 'b', 'e', 'g'),
        Set('a')
      ), Set(
        Set('f', 'e', 'a', 'c', 'b'),
        Set('d', 'g', 'h', 'i')
      )
    )
    (graphs zip expectedSet) foreach {
      case (g, e) => {
        g.stronglyConnectedNodeSets.map(_.map(_.value)) should be (e)
        g.stronglyConnectedSets                         should be (e)
      }
    }
  }

  test("a graph's SCC-subgraph DAG") {
    val expectedDAGs = Array(
      """|DAG:
         |Graph(a)
         |Graph(u)
         |Graph(v, x, y, v~>y, x~>v, y~>x)
         |Graph(w)
         |Graph(z, z~>z)
         |Graph(u)~>Graph(v, x, y, v~>y, x~>v, y~>x)
         |Graph(w)~>Graph(v, x, y, v~>y, x~>v, y~>x)
         |Graph(w)~>Graph(z, z~>z)
         |""".stripMargin
    , """|DAG:
         |Graph(a)
         |Graph(u)
         |Graph(v, x, y, v~>x, x~>y, y~>v)
         |Graph(w)
         |Graph(z, z~>z)
         |Graph(v, x, y, v~>x, x~>y, y~>v)~>Graph(u)
         |Graph(v, x, y, v~>x, x~>y, y~>v)~>Graph(w)
         |Graph(z, z~>z)~>Graph(w)
         |""".stripMargin
    , """|DAG:
         |Graph(a)
         |Graph(b, d, e, g, b~>e, d~>b, e~>g, g~>d)
         |Graph(c)
         |Graph(f, h, f~>h, h~>f)
         |Graph(a)~>Graph(b, d, e, g, b~>e, d~>b, e~>g, g~>d)
         |Graph(b, d, e, g, b~>e, d~>b, e~>g, g~>d)~>Graph(c)
         |""".stripMargin
    , """|DAG:
         |Graph(a, b, c, e, f, a~>b, a~>e, b~>a, c~>b, c~>f, e~>f, f~>c)
         |Graph(d, g, h, i, d~>g, g~>d, g~>i, h~>d, h~>i, i~>h)
         |""".stripMargin
    )

    (graphs zip expectedDAGs) foreach {
      case (g, e) => {
        val d = g.stronglyConnectedComponentsDag
        "DAG:\n" + d.asSortedString("\n", "\n", "\n") + "\n" should be (e)
        d should not be ('isCyclic)
      }
    }
  }
}