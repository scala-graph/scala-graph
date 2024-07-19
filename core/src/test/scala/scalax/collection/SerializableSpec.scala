package scalax.collection

import java.io._

import scala.util.{Failure, Success, Try}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scalax.collection.OuterImplicits._
import scalax.collection.edges._
import scalax.collection.generic.{AnyEdge, Edge, GenericGraphCoreFactory}

import org.scalatest._

import scalax.collection.visualization.Visualizer

class SerializableSpec
    extends Suites(
      new TSerializable[immutable.Graph](immutable.Graph),
      new TSerializable[mutable.Graph](mutable.Graph)
    )

/** Tests standard java serialization.
  */
final private class TSerializable[CC[N, E <: Edge[N]] <: AnyGraph[N, E] with GraphLike[N, E, CC]](
    val factory: GenericGraphCoreFactory[CC]
) extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with IntelliJ[CC]
    with Visualizer {
  private val factoryName     = factory.getClass.getName
  private val isImmutableTest = factoryName contains "immutable"

  s"Tests based on $factoryName" should "start" in {}

  trait GraphStore {
    protected trait ExecMethods {
      def save[N, E <: Edge[N]](g: CC[N, E]): Unit
      def restore[N, E <: Edge[N]]: CC[N, E]
    }
    protected def newTest: ExecMethods
    def test[N, E <: Edge[N]](g: CC[N, E]): CC[N, E] = {
      val exec = newTest
      exec.save[N, E](g)
      val r = exec.restore[N, E]
      withGraph(r.asAnyGraph)(_ should equal(g))
      r
    }
  }

  /** to save and restore graphs to/from files */
  object GraphFile extends GraphStore {
    protected class Exec(filename: String) extends ExecMethods {
      import FileSerialization._
      def save[N, E <: Edge[N]](g: CC[N, E]): Unit = write(g, filename) recover { case e =>
        fail(s"Couldn't write $g: $e")
      }

      def restore[N, E <: Edge[N]]: CC[N, E] = read(filename).recover { case e =>
        fail(s"Couldn't read graph: $e")
      }.get
    }
    private val tmpDir = System.getProperty("java.io.tmpdir")
    private var cnt    = 0
    protected def newTest: Exec = {
      cnt += 1
      new Exec( // include the name of the test method
        s"$tmpDir${File.separator}${s"${this.getClass.getSimpleName.init}.${if (isImmutableTest) "i" else "m"}-${testNames.toArray.apply(cnt)}"}.ser"
      )
    }
  }

  /** to save and restore graphs to/from byte arrays */
  object GraphByteArray extends GraphStore {
    protected class Exec extends ExecMethods {
      import ByteArraySerialization._
      private var _saved: Array[Byte] = _

      def save[N, E <: Edge[N]](g: CC[N, E]): Unit = write(g) match {
        case Success(s) => _saved = s
        case Failure(e) => fail("Couldn't write: " + g, e)
      }

      def restore[N, E <: Edge[N]]: CC[N, E] = read[CC[N, E]](_saved) match {
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
    val g = factory[Int, AnyEdge](-1 ~ 1, 2 ~> 1)
    store.test[Int, AnyEdge[Int]](g)
  }

  "A graph of type [String,UnDiEdge]" should work in {
    val g = factory[String, AnyEdge]("a" ~ "b", "b" ~> "c")
    store.test[String, AnyEdge[String]](g)
  }

  "A graph of type [Int,DiEdge]" should work in {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)
    store.test[Int, DiEdge[Int]](g)
  }

  /* TODO L
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

      back.size should be(1)

      val inner_1 = back get n1
      inner_1.diSuccessors should have size 1
      inner_1.diSuccessors.head should be(n2)

      val backEdge = back.edges.head
      backEdge.source.s should be(List(a1, b1))
      backEdge.target.s should be(List(a2, b2))
      backEdge.label should be(label)
    }
  }
   */

  "After calling diSuccessors the graph" should work in {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)
    g.nodes.head.diSuccessors
    store.test[Int, DiEdge[Int]](g)
  }

  "After calling pathTo the graph" should work in {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)
    g.nodes.head.diSuccessors
    val n = g.nodes.head
    n.pathTo(n)
    store.test[Int, DiEdge[Int]](g)
  }

  "A deserialized graph" should "be traversable" in {
    import Data.elementsOfDi_1
    val g    = factory(elementsOfDi_1: _*)
    val back = store.test[Int, DiEdge[Int]](g)
    withGraph(g.asAnyGraph) { g =>
      def op(g: AnyGraph[Int, DiEdge[Int]]): Int = g.nodes.head.outerNodeTraverser.size
      op(back.asAnyGraph) shouldBe op(g)
    }
  }

  "A deserialized graph" should "have the same successors" in {
    import Data.elementsOfDi_1
    val g = factory(elementsOfDi_1: _*)

    def outerSuccessors(g: AnyGraph[Int, DiEdge[Int]]) =
      g.nodes map (innerNode => innerNode.outer -> innerNode.diSuccessors.map(_.outer))

    val diSuccBefore = outerSuccessors(g.asAnyGraph)
    val back         = store.test[Int, DiEdge[Int]](g)
    withGraph(g.asAnyGraph) { g =>
      outerSuccessors(back.asAnyGraph) shouldBe diSuccBefore
    }
  }

  trait EdgeStore {
    def save[N, E <: Edge[N]](e: Iterable[OuterElem[N, E]]): Unit
    def restore[N, E <: Edge[N]]: Iterable[OuterElem[N, E]]
    def test[N, E <: Edge[N]](e: Iterable[OuterElem[N, E]]): Iterable[OuterElem[N, E]] = {
      save[N, E](e)
      val r = restore[N, E]
      r should be(e)
      r
    }
  }

  class EdgeByteArray extends EdgeStore {
    import ByteArraySerialization._
    private var _saved: Array[Byte] = _

    def save[N, E <: Edge[N]](it: Iterable[OuterElem[N, E]]): Unit = write(it) match {
      case Success(s) => _saved = s
      case Failure(e) => fail(s"Couldn't write '$it': $e")
    }

    def restore[N, E <: Edge[N]]: Iterable[OuterElem[N, E]] =
      readWithCustomClassLoader[Iterable[OuterElem[N, E]]](_saved) match {
        case Success(s) => s
        case Failure(e) => fail(s"Couldn't read iterable: $e")
      }
  }

  "A graph of [Int,WUnDiEdge]" should work in {
    import Data.elementsOfMixed_2
    withGraph(factory.from(elementsOfMixed_2).asAnyGraph) { _ =>
      (new EdgeByteArray).test[Int, AnyEdge[Int]](elementsOfMixed_2)
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
    } recoverWith { case e =>
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
    } recoverWith { case e =>
      in.close()
      Failure[A](e)
    }

  // resolves ClassNotFound issue with SBT
  private val cl = classOf[SerializableSpec].getClassLoader
  private class CustomObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
    override def resolveClass(cd: ObjectStreamClass): Class[_] =
      try
        cl.loadClass(cd.getName())
      catch {
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
    } recoverWith { case e =>
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
    } recoverWith { case e =>
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

  protected trait Base {
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
    def inner: Inner.type = Inner
  }

  private trait MyTrait extends Base with Serializable {
    @transient // !!! ignored so Serializable needs to be mixed in
    object Inner extends super.Inner with Serializable {
      def d = "MyDef"
      val v = "MyVal"
    }
    val inner: Inner.type = Inner
  }

  private class MyClass extends Base with Serializable {
    @transient // !!! same as MyTrait
    object Inner extends super.Inner with Serializable {
      def d = "MyDef"
      val v = "MyVal"
    }
    val inner: Inner.type = Inner
  }

  import ByteArraySerialization._
  def test[A <: Base](my: A): Unit =
    write(my) flatMap { saved =>
      println(s"saved (${saved.length} bytes)=${new String(saved)}")
      println(s"  contains MyVal=${new String(saved) contains "MyVal"}")
      read[A](saved)
    } map (my => println(s"  okDef=${my.inner.d}, okVal=${my.inner.v}")) recover { case e =>
      println(s"serialization of $my failed with $e")
    }

  test(MyObject)
  test(new MyTrait {})
  test(new MyClass)
}
