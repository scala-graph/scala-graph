package scalax.collection.io.json

import language.higherKinds

import org.scalatest.Spec
import org.scalatest.Suites
import org.scalatest.Informer
import org.scalatest.matchers.ShouldMatchers

import net.liftweb.json._

import scalax.collection._
import scalax.collection.GraphPredef._,
       scalax.collection.GraphEdge._
import scalax.collection.generic.GraphCoreCompanion
import scalax.collection.edge.CBase._

import scalax.collection.io.edge.CEdgeParameters
import scalax.collection.io.json.descriptor.CEdgeDescriptor

import serializer._, imp._, imp.Parser.{parse => graphParse},
       descriptor._, descriptor.predefined._, descriptor.Defaults._,
       exp.Export
       
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TCustomEdgeRootTest
  extends Suites(
      new TCustomEdge[immutable.Graph](immutable.Graph),
      new TCustomEdge[  mutable.Graph](  mutable.Graph))
  with ShouldMatchers
{
  // ---------------------------------------- mutable tests
}
/**	Tests JSON import/export of graphs with custom edges.
 */
class TCustomEdge[CC[N,E[X] <: EdgeLikeIn[X]] <: Graph[N,E] with GraphLike[N,E,CC]]
    (val factory: GraphCoreCompanion[CC] with GraphCoreCompanion[CC])
	extends	Spec
	with	ShouldMatchers
{
  val jsonText = """
    { "nodes" : [
        ["Editor"],
        ["Menu"],
        ["Settings"]],
      "edges": [
        ["Editor", "Menu",     "M", "Alt" ],
        ["Menu",   "Settings", "S", "NoneModifier"]]
    }""".filterNot(_.isWhitespace)
  import KeyModifier._
  val descriptor =
    new Descriptor[String](
      StringNodeDescriptor,
      new CEdgeDescriptor[String, Transition, Transition.type, Transition.P](
          edgeCompanion    = Transition,
          sampleAttributes = ('A', NoneModifier),
          customSerializer = Some(new Transition.Serializer),
          typeId           = "TraceEdge")
    )
  val graph = factory[String,Transition](
      Transition("Editor", "Menu",     'M', Alt),
      Transition("Menu",   "Settings", 'S', NoneModifier))

  def test_import {
    factory.fromJson[String,Transition](jsonText, descriptor) should be (graph)
  }
  def test_ImEx {
    factory.fromJson[String,Transition](
        graph.toJson(descriptor), descriptor) should be (graph)
  }
}
/** Type of the custom attribute of `CustomEdge`. */
object KeyModifier extends Enumeration {
   type KeyModifier = Value
   val NoneModifier, Alt, Ctrl, Shift = Value
}
import KeyModifier._
/** Custom edge with the two custom attributes `key` and `keyMod`.
 *  Note that it is also necessary to extend `Attributes` enabling to be a member
 *  of the JSON `CEdgeDescriptor`. */
class Transition[N](from: N, to: N, val key: Char, val keyMod: KeyModifier)
    extends DiEdge  [N](NodeProduct(from, to))
    with ExtendedKey[N]
    with EdgeCopy   [Transition]
    with OuterEdge     [N, Transition]
    with Attributes [N] {

  def keyAttributes = Seq(key, keyMod)
  override protected def attributesToString = " (" + key + "," + keyMod + ")"

  type P = Transition.P
  override def attributes: P = new Tuple2(key, keyMod)
  override def copy[NN](newNodes: Product): Transition[NN] = 
    Transition.newEdge[NN](newNodes, attributes)
}
/** Custom edge companion object extending `CEdgeCompanion`. This is necessary
 *  to enable this custom edge to be a member of the JSON `CEdgeDescriptor`. */
object Transition extends CEdgeCompanion[Transition] {
  class Serializer extends CustomSerializer[CEdgeParameters[Transition.P]]( formats => (
    { case JArray(JString(n1)  :: JString(n2)     ::
                    JString(key) :: JString(keyMod) :: Nil) =>
           new CEdgeParameters[Transition.P](n1, n2, (key(0), KeyModifier.withName(keyMod)))
    },
    { case CEdgeParameters((nId_1,nId_2), (key, keyMod)) =>
           JArray(JString(nId_1)        :: JString(nId_2)           ::
                  JString(key.toString) :: JString(keyMod.toString) :: Nil)
    }))
 /** Assuming that nodes are of type String. */
  def apply(from: String, to: String, key: Char, keyMod: KeyModifier) =
    new Transition[String](from, to, key, keyMod)
  def unapply[N](e: Transition[String]): Option[(String,String,Char,KeyModifier)] =
    if (e eq null) None
    else Some(e.from, e.to, e.key, e.keyMod)

  type P = (Char, KeyModifier)
  override protected def newEdge[N](nodes: Product, attributes: P) = nodes match {
    case (from: N, to: N) =>
      new Transition[N](from, to, attributes._1, attributes._2)
  }
}