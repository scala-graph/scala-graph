package scalax.collection.io.jsoniter

import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.config.GraphConfig

trait GraphCodecSpecBase extends RefSpec with Matchers:

  protected def onJsonNullFail[A]: A = null.asInstanceOf[A]

  protected val smallGraphConfig: GraphConfig =
    GraphCodec.graphConfig(orderHint = 32, degreeHint = 8)
