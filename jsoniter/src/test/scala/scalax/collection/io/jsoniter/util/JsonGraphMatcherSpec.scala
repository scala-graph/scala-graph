package scalax.collection.io.jsoniter.util

import org.scalatest.exceptions.TestFailedException
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class JsonGraphMatcherSpec extends RefSpec with Matchers:

  private val empty: String = """{ "nodes": [], "edges": [] }"""

  private def populated(nodes: Iterable[String] = Nil, edges: Iterable[String] = Nil): String =
    s"""{ "nodes": [${nodes mkString ", "}], "edges": [${edges mkString ", "}] }"""

  def `fails on bad frame`(): Unit =
    intercept[TestFailedException]("""{ "nodes": [] }""" shouldBe sameAs(empty))
    intercept[TestFailedException]("""{ "nodes": [], "edgesX": [] }""" shouldBe sameAs(empty))
    intercept[TestFailedException]("""{ "nodes": [1, 2, "edges": [] }""" shouldBe sameAs(empty))
    intercept[TestFailedException]("""{ "nodes": [], "edges": []""" shouldBe sameAs(empty))

  def `succeeds on empty graph`(): Unit =
    empty.minified shouldBe sameAs(empty)

  def `fails on missing nodes`(): Unit =
    intercept[TestFailedException] {
      populated(nodes = List("1", "2")) shouldBe sameAs(populated(nodes = List("1", "2", "3")))
    }
    intercept[TestFailedException] {
      populated(nodes = List("""{ "size": 32 }""")) shouldBe sameAs(populated(nodes = List("""{ "size": 33 }""")))
    }

  def `fails on missing edges`(): Unit =
    intercept[TestFailedException] {
      populated(edges =
        List(
          """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "AMS" }"""
        )
      ) shouldBe sameAs(
        populated(edges =
          List(
            """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "AMS" }""",
            """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "JFK" }"""
          )
        )
      )
    }

  def `succeeds on equalling nodes in different order`(): Unit =
    populated(nodes = List("1", "2")) shouldBe sameAs(populated(nodes = List("2", "1")))

  def `succeeds on equalling edges in different order`(): Unit =
    populated(edges =
      List(
        """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "JFK" }""",
        """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "AMS" }"""
      )
    ) shouldBe sameAs(
      populated(edges =
        List(
          """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "AMS" }""",
          """{ "edgeT": "DiEdge", "sourceId": "LHR", "targetId": "JFK" }"""
        )
      )
    )
