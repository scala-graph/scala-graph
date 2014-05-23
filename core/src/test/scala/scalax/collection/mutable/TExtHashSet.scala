package scalax.collection
package mutable

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import GraphPredef._, GraphEdge._

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/** Tests [[ExtHashSet]]. */
@RunWith(classOf[JUnitRunner])
class TExtHashSetTest
  extends Spec
  with    ShouldMatchers
{
  import Data._
  val set = ExtHashSet(outerElemsOfDi_1: _*)
  val outerEdge: DiEdge[Int] = outerElemsOfDi_1.head
  val graph = Graph(outerElemsOfDi_1: _*)
  val innerEdge = graph get outerEdge

  def test_findEntry {
    /* `inner.edge == outer` returns the expected result because Graph#InnerEdge.equal
     * is aware of the inner edge structure. The opposite will be false since, as a rule,
     * outer object types will not be Graph-aware with regard to their equal.
     */
    def eq(outer: DiEdge[Int], inner: graph.EdgeT) = inner.edge == outer
    set.findEntry(innerEdge, eq) should be (outerEdge)
  }
  def test_drawElement {
    val randomElems = collection.mutable.Set.empty[DiEdge[Int]]
    val r = new util.Random
    for (i <- 1 to (set.size * 16))
      randomElems += set draw r
    randomElems should have size (set.size)
  }
  def test_hashCodeIterator {
    set.hashCodeIterator(-228876066).toList should have size (0)
    outerEdge.hashCode should be (innerEdge.hashCode)
    val elems = set.hashCodeIterator(outerEdge.hashCode).toList
    elems      should have size (1)
    elems.head should be (outerEdge)
  }
  def test_multiHashCodeIterator {
    case class C(i: Int, j: Int) {
      override def hashCode = i.##
    }
    val multi = ExtHashSet(C(1,0), C(1,1), C(2,2), C(1,3), C(2,0))
    for (i <- 0 to 2) {
      val elems = multi.hashCodeIterator(i.##).toList
      elems should have size (multi count (_.i == i))
    }
  }
}