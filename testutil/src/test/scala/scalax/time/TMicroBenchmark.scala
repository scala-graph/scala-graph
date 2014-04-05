package scalax.time

import org.scalatest.Spec
import org.scalatest.Matchers

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TMicroBenchmarkTest
    extends Spec
       with Matchers {
  
  import MicroBenchmark._

  object `relative times` {
    def `should reflect operation complexity` {
      val r = 1 to 20
      val relTimes = relativeTimes()(
        r.toList.sorted, 
        r.toList.toArray.toList.sorted, 
        r.toList.toSet.toArray.toList.sorted 
      )
      relTimes sliding 2 foreach {l =>
        l.head should be < l.tail.head
      }
    }
  }
}