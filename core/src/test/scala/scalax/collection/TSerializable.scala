package scalax.collection

import java.io._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import GraphPredef._, GraphEdge._
import generic.GraphCoreCompanion

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.visualization.Visualizer

@RunWith(classOf[JUnitRunner])
class TSerializableRootTest
    extends Suites(new TSerializable[immutable.Graph](immutable.Graph), new TSerializable[mutable.Graph](mutable.Graph))

/**	Tests for standard java serialization.
  */
final class TSerializable[CC[N, E[X] <: EdgeLikeIn[X]] <: Graph[N, E] with GraphLike[N, E, CC]](
    val factory: GraphCoreCompanion[CC])
    extends FlatSpec
    with Matchers
    with BeforeAndAfterEach
    with Visualizer[CC] {
  private val factoryName     = factory.getClass.getName
  private val isImmutableTest = factoryName contains "immutable"

  s"Tests based on ${factoryName}" should "start" in {}

  trait GraphStore {
    protected trait Exec {
      def save[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E]): Unit
      def restore[N, E[X] <: EdgeLikeIn[X]]: CC[N, E]
    }
    protected def newTest: Exec
    def test[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E]): CC[N, E] = {
      val exec = newTest
      exec.save[N, E](g)
      val r = exec.restore[N, E]
      given(r) { _ should be(g) }
      r
    }
  }

  /** to save and restore graphs to/from files */
  object GraphFile extends GraphStore {
    protected class Exec(filename: String) extends super.Exec {
      import FileSerialization._
      def save[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E]): Unit = write(g, filename) recover {
        case e => fail(s"Couldn't write $g: $e")
      }

      def restore[N, E[X] <: EdgeLikeIn[X]]: CC[N, E] = read(filename).recover {
        case e => fail(s"Couldn't read graph: $e")
      }.get
    }
    private val tmpDir = System.getProperty("java.io.tmpdir")
    private var cnt    = 0
    protected def newTest: Exec = {
      cnt += 1
      new Exec( // include the name of the test method
        s"$tmpDir${File.separator}${s"${this.getClass.getSimpleName.init}.${if (isImmutableTest) "i" else "m"}-${testNames.toArray.apply(cnt)}"}.ser")
    }
  }

  /** to save and restore graphs to/from byte arrays */
  object GraphByteArray extends GraphStore {
    protected class Exec extends super.Exec {
      import ByteArraySerialization._
      private var _saved: Array[Byte] = _

      def save[N, E[X] <: EdgeLikeIn[X]](g: CC[N, E]): Unit = write(g) match {
        case Success(s) => _saved = s
        case Failure(e) => fail("Couldn't write: " + g, e)
      }

      def restore[N, E[X] <: EdgeLikeIn[X]]: CC[N, E] = read[CC[N, E]](_saved) match {
        case Success(s) => s
        case Failure(e) => fail("Couldn't read graph", e)
      }
    }
    protected def newTest: Exec = new Exec
  }

  /** normally we test with byte arrays but may be set to GraphFile instead */
  private lazy val store: GraphStore = GraphByteArray

  private val work = "be serializable"

  "An empty graph" should work in {
    val g = factory.empty[Nothing, Nothing]
    store.test[Nothing, Nothing](g)
  }
  "A graph of type [Int,Nothing]" should work in {
    val g = factory[Int, Nothing](-1, 1, 2)
    store.test[Int, Nothing](g)
  }
  "A graph of type [Int,UnDiEdge]" should work in {
    val g = factory(-1 ~ 1, 2 ~> 1)
    store.test[Int, UnDiEdge](g)
  }
  "A graph of type [String,UnDiEdge]" should work in {
    val g = factory("a" ~ "b", "b" ~> "c")
    store.test[String, UnDiEdge](g)
  }
  "A graph of type [Int,DiEdge]" should work in {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)
    store.test[Int, DiEdge](g)
  }
  "A graph of [MyNode,WLDiEdge]" should work in {
    import edge.WLDiEdge

    val (a1, a2, b1, b2) = ("a1", "a2", "b1", "b2")
    val (n1, n2)         = (MyNode(List(a1, b1)), MyNode(List(a2, b2)))
    val label            = MyLabel("abab")
    val e                = WLDiEdge(n1, n2)(11, label)

    {
      /* if MyNode is declared as an inner class, it is not serializable;
       * so we assert first, that both the node class and the label class are serializable
       */
      val bos = new ByteArrayOutputStream
      val out = new ObjectOutputStream(bos)
      out writeObject n1
      out writeObject label
      bos.close
    }

    given(factory(e)) { g =>
      val back = store.test[MyNode, WLDiEdge](g)

      back.graphSize should be(1)

      val inner_1 = (back get n1)
      inner_1.diSuccessors should have size (1)
      inner_1.diSuccessors.head should be(n2)

      val backEdge = back.edges.head
      backEdge.source.s should be(List(a1, b1))
      backEdge.target.s should be(List(a2, b2))
      backEdge.label should be(label)
    }
  }
  "After calling diSuccessors the graph" should work in {
    import Data.elementsOfDi_1
    given(factory(elementsOfDi_1: _*)) { g =>
      g.nodes.head.diSuccessors
      val back = store.test[Int, DiEdge](g)
      back should be(g)
    }
  }
  "After calling pathTo the graph" should work in {
    import Data.elementsOfDi_1
    given(factory(elementsOfDi_1: _*)) { g =>
      g.nodes.head.diSuccessors
      val n = g.nodes.head
      n.pathTo(n)
      val back = store.test[Int, DiEdge](g)
      back should be(g)
    }
  }
  "A deserialized graph" should "be traversable" in {
    import Data.elementsOfDi_1
    given(factory(elementsOfDi_1: _*)) { g =>
      val back                        = store.test[Int, DiEdge](g)
      def op(g: CC[Int, DiEdge]): Int = g.nodes.head.outerNodeTraverser.size
      op(back) should be(op(g))
    }
  }
  "A deserialized graph" should "have the same successors" in {
    import Data.elementsOfDi_1
    given(factory(elementsOfDi_1: _*)) { g =>
      def outerSuccessors(g: CC[Int, DiEdge]) =
        g.nodes map (innerNode => innerNode.value -> innerNode.diSuccessors.map(_.value))
      val diSuccBefore = outerSuccessors(g)
      val back         = store.test[Int, DiEdge](g)
      outerSuccessors(back) should be(diSuccBefore)
    }
  }

  trait EdgeStore {
    def save[N, E[X] <: EdgeLikeIn[X]](e: Iterable[InParam[N, E]]): Unit
    def restore[N, E[X] <: EdgeLikeIn[X]]: Iterable[InParam[N, E]]
    def test[N, E[X] <: EdgeLikeIn[X]](e: Iterable[InParam[N, E]]): Iterable[InParam[N, E]] = {
      save[N, E](e)
      val r = restore[N, E]
      r should be(e)
      r
    }
  }

  class EdgeByteArray extends EdgeStore {
    import ByteArraySerialization._
    private var _saved: Array[Byte] = _

    def save[N, E[X] <: EdgeLikeIn[X]](it: Iterable[InParam[N, E]]): Unit = write(it) match {
      case Success(s) => _saved = s
      case Failure(e) => fail(s"Couldn't write '$it': $e")
    }

    def restore[N, E[X] <: EdgeLikeIn[X]]: Iterable[InParam[N, E]] =
      readWithCustomClassLoader[Iterable[InParam[N, E]]](_saved) match {
        case Success(s) => s
        case Failure(e) => fail(s"Couldn't read iterable: $e")
      }
  }

  "A graph of [Int,WUnDiEdge]" should work in {
    import edge.WUnDiEdge, Data.elementsOfWUnDi_2
    given(factory(elementsOfWUnDi_2: _*)) { _ =>
      new EdgeByteArray test [Int, WUnDiEdge] (elementsOfWUnDi_2)
    }
  }
}

object ByteArraySerialization {
  def write(obj: AnyRef, initSize: Int = 8000): Try[Array[Byte]] = {
    val bos = new ByteArrayOutputStream(initSize)
    val out = new ObjectOutputStream(bos)
    Try {
      out writeObject obj
      out.close()
      bos.toByteArray
    } recoverWith {
      case e =>
        out.close()
        Failure[Array[Byte]](e)
    }
  }

  def readWithCustomClassLoader[A](from: Array[Byte]): Try[A] =
    read[A](new CustomObjectInputStream(new ByteArrayInputStream(from)))

  def read[A](from: Array[Byte]): Try[A] =
    read(new ObjectInputStream(new ByteArrayInputStream(from)))

  def read[A](in: ObjectInputStream): Try[A] =
    Try {
      val read = in.readObject
      in.close()
      read.asInstanceOf[A]
    } recoverWith {
      case e =>
        in.close()
        Failure[A](e)
    }

  // resolves ClassNotFound issue with SBT
  private val cl = classOf[TSerializableRootTest].getClassLoader
  private class CustomObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
    override def resolveClass(cd: ObjectStreamClass): Class[_] =
      try {
        cl.loadClass(cd.getName())
      } catch {
        case cnf: ClassNotFoundException =>
          super.resolveClass(cd)
      }
    override def resolveProxyClass(interfaces: Array[String]): Class[_] =
      try {
        val ifaces = interfaces map { iface =>
          cl.loadClass(iface)
        }
        java.lang.reflect.Proxy.getProxyClass(cl, ifaces: _*)
      } catch {
        case e: ClassNotFoundException =>
          super.resolveProxyClass(interfaces)
      }
  }
}

object FileSerialization {
  def write(obj: AnyRef, filename: String): Try[File] = write(obj, new File(filename))
  def write(obj: AnyRef, file: File): Try[File] = {
    var out: ObjectOutputStream = null
    Try {
      out = new ObjectOutputStream(new FileOutputStream(file))
      out writeObject obj
      out.close()
      file
    } recoverWith {
      case e =>
        if (out ne null) out.close()
        Failure[File](e)
    }
  }
  def read[A](filename: String): Try[A] = read(new File(filename))
  def read[A](file: File): Try[A] = {
    var in: ObjectInputStream = null
    Try {
      in = new ObjectInputStream(new FileInputStream(file))
      val read = in.readObject
      in.close()
      read.asInstanceOf[A]
    } recoverWith {
      case e =>
        if (in ne null) in.close()
        Failure[A](e)
    }
  }
}

// to be serializable, the following classes must be defined at top level
case class MyNode(val s: List[String]) extends Serializable
case class MyLabel(val s: String)      extends Serializable

// examining https://issues.scala-lang.org/browse/SI-5773?jql=text%20~%20%22%40transient%22
protected object ScalaObjectSerialization extends App {

  private trait Base {
    trait Inner {
      def d: String
      def v: String
    }
    def inner: Inner
  }

  private object MyObject extends Base with Serializable {
    @transient // ok: no serialization takes place
    object Inner extends super.Inner {
      def d = "MyDef"
      val v = "MyVal"
    }
    def inner = Inner
  }

  private trait MyTrait extends Base with Serializable {
    @transient // !!! ignored so Serializable needs to be mixed in
    object Inner extends super.Inner with Serializable {
      def d = "MyDef"
      val v = "MyVal"
    }
    val inner = Inner
  }

  private class MyClass extends Base with Serializable {
    @transient // !!! same as MyTrait
    object Inner extends super.Inner with Serializable {
      def d = "MyDef"
      val v = "MyVal"
    }
    val inner = Inner
  }

  import ByteArraySerialization._
  def test[A <: Base](my: A): Unit =
    write(my) flatMap { saved =>
      println(s"saved (${saved.length} bytes)=${new String(saved)}")
      println(s"  contains MyVal=${new String(saved) contains "MyVal"}")
      read[A](saved)
    } map (my => println(s"  okDef=${my.inner.d}, okVal=${my.inner.v}")) recover {
      case e =>
        println(s"serialization of $my failed with $e")
    }

  test(MyObject)
  test(new MyTrait {})
  test(new MyClass)
}
