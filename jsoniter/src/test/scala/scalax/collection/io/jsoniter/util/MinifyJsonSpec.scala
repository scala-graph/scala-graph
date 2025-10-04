package scalax.collection.io.jsoniter.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec

class MinifyJsonSpec extends RefSpec with Matchers:

  def `minify JSON string`(): Unit =
    """{
      "a": "some text",
      "b": "more text"
    }""".minified shouldBe """{"a":"some text","b":"more text"}"""
