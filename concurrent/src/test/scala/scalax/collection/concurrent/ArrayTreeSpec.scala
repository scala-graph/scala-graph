package scalax.collection.concurrent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.scalaUtilChainingOps
import org.scalactic.Prettifier
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.{LazyArg, MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.refspec.RefSpec
import scalax.collection.concurrent.ArrayTree.Config.LevelCap
import scalax.util.primitives.*
import ArrayTree.*

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, Buffer}

class ArrayTreeSpec extends RefSpec with Matchers with ScalaFutures:
  implicit val disableDefaultArrayHandling: Prettifier = Prettifier(_.toString)

  /* The following conversions are not recommended in production code,
   * but they are useful in test by allowing to pass a plain Int instead of a factory method
   * like `2` instead of `Size(2)`. On invalid values an exception will be thrown on test execution.
   */
  import scala.language.implicitConversions
  private given Conversion[Int, Size]         = (i: Int) => Size.unsafe(i)
  private given Conversion[Int, PositiveSize] = (i: Int) => PositiveSize.unsafe(i)
  private given Conversion[Int, Log2Capacity] = (i: Int) => Log2Capacity.powerOf2Unsafe(i)

  import ArrayTreeSpec.*

  def `append single threaded`: Unit =
    def check(appendCount: PositiveSize)(
        initialCap: Capacity,
        leafCap: Log2Capacity = 2,
        nodeCap: Log2Capacity = 2
    ): Unit =
      info(f"$appendCount%2d times to tree($initialCap, $leafCap, $nodeCap)")
      given Config = Config(initialCap, leafCap, nodeCap)
      val tree     = ArrayTree.empty[Int]
      1 to appendCount.value foreach { i =>
        (tree append i).value shouldBe i - 1
        tree.size.value shouldBe i
      }

    check(2)(initialCap = 1)
    check(4)(initialCap = 4)
    check(3)(initialCap = 1)
    check(3)(initialCap = 2)
    check(5)(initialCap = 4)
    check(7)(initialCap = 4)
    check(9)(initialCap = 4)
    check(40)(initialCap = 1, leafCap = 4, nodeCap = 4)

  def `append concurrently`: Unit =
    given ExecutionContext = ExecutionContext.global

    def append(count: Int)(
        initialCap: Capacity,
        leafCap: Log2Capacity = 2,
        nodeCap: Log2Capacity = 2
    ): Unit =
      given tree: ArrayTree[Int] = ArrayTree.empty[Int](using Config(initialCap, leafCap, nodeCap))
      val range                  = 0 until count
      val seq                    = Future.sequence(range map (i => Future(tree append i)))
      withClue(f"$count%2d futures, tree($initialCap, $leafCap, $nodeCap)$lineSeparator")(
        whenReady(seq) { indexes =>
          indexes should coverRange(range)
          tree.size.value shouldBe count
        }
      )

    append(count = 5)(initialCap = 5)
    append(count = 10)(initialCap = 4)
    append(count = 20)(initialCap = 4)
    append(count = 333)(initialCap = 3)

  object `treeIterator, leafCapacity: 2, nodeCapacity: 2`:
    private val defaultLeafCapacity: Log2Capacity = 2
    private val defaultNodeCapacity: Log2Capacity = 2

    private def tree(size: PositiveSize)(
        initialCap: Capacity,
        leafCap: Log2Capacity = defaultLeafCapacity,
        nodeCap: Log2Capacity = defaultNodeCapacity
    ): ArrayTree[Int] =
      given Config = Config(initialCap, leafCap, nodeCap)
      ArrayTree.empty[Int] tap (t => 1 to size.value foreach t.append)

    extension (multi: MultiLeaf.type)
      private def fake: MultiLeaf[Int] =
        MultiLeaf(defaultLeafCapacity.asPositive, null)(0)

      private def withNullLeftNeighbor(elems: Int*): MultiLeaf[Int] =
        MultiLeaf.fromNonEmpty(defaultLeafCapacity.asPositive, null)(elems*)

      private def withFakeLeftNeighbor(elems: Int*): MultiLeaf[Int] =
        MultiLeaf.fromNonEmpty(defaultLeafCapacity.asPositive, fake)(elems*)

    extension (node: UpperNode.type)
      private def fake: UpperNode[Int] =
        UpperNode.empty[Int](defaultNodeCapacity)

      private def denseFake: UpperNode[Int] =
        UpperNode(defaultNodeCapacity)(Array.fill(defaultNodeCapacity.asInt)(UpperNode.fake)*)

      private def sparseFake(elemCount: PositiveSize): UpperNode[Int] =
        UpperNode(defaultNodeCapacity)(Array.fill(elemCount.value)(UpperNode.fake)*)

    extension (node: LeafParentNode.type)
      private def denseFake: LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity)(Array.fill(defaultNodeCapacity.asInt)(MultiLeaf.fake)*)

      private def sparseFake(elemCount: PositiveSize): LeafParentNode[Int] =
        LeafParentNode(defaultNodeCapacity)(Array.fill(elemCount.value)(MultiLeaf.fake)*)

    def `size:  1, initialCapacity: 1`: Unit =
      tree(1)(initialCap = 1) should equalTree(SingleLeaf(1))

    def `size:  2, initialCapacity: 1`: Unit =
      tree(2)(initialCap = 1) should equalTree(MultiLeaf.withNullLeftNeighbor(1, 2))

    def `size:  3, initialCapacity: 1`: Unit =
      tree(3)(initialCap = 1) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf.withNullLeftNeighbor(1, 2),
        MultiLeaf.withFakeLeftNeighbor(3)
      )

    def `size:  3, initialCapacity: 2`: Unit =
      tree(3)(initialCap = 1) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf.withNullLeftNeighbor(1, 2),
        MultiLeaf.withFakeLeftNeighbor(3)
      )

    def `size:  5, initialCapacity: 4`: Unit =
      tree(5)(initialCap = 4) should equalTree(
        LeafParentNode.denseFake,
        MultiLeaf.fromNonEmpty(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5)
      )

    def `size:  7, initialCapacity: 4`: Unit =
      tree(7)(initialCap = 4) should equalTree(
        UpperNode.denseFake,
        LeafParentNode.denseFake,
        MultiLeaf.fromNonEmpty(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5, 6),
        LeafParentNode.sparseFake(elemCount = 1),
        MultiLeaf.withFakeLeftNeighbor(7)
      )

    def `size: 12, initialCapacity: 4`: Unit =
      tree(12)(initialCap = 4) should equalTree(
        UpperNode.denseFake,
        UpperNode.denseFake,
        LeafParentNode.denseFake,
        MultiLeaf.fromNonEmpty(4, null)(1, 2, 3, 4),
        MultiLeaf.withFakeLeftNeighbor(5, 6),
        LeafParentNode.denseFake,
        MultiLeaf.withFakeLeftNeighbor(7, 8),
        MultiLeaf.withFakeLeftNeighbor(9, 10),
        UpperNode.sparseFake(elemCount = 1),
        LeafParentNode.sparseFake(elemCount = 1),
        MultiLeaf.withFakeLeftNeighbor(11, 12)
      )

  object `apply index`:
    val initialOneConfig = Config(initialCap = 1, leafCap = 8, nodeCap = 4)
    val smallConfig      = Config(initialCap = 20, leafCap = 8, nodeCap = 4)

    private def smallConfigLevelCaps(i: Int): LevelCap.Full =
      smallConfig.levelCaps(i) match
        case full: LevelCap.Full => full
        case _                   => fail()

    private def populateAndCheck(size: PositiveSize): Unit =
      val tree = ArrayTree.empty[Int](using smallConfig)
      size.indexes foreach tree.append

      Range(start = 0, end = size.value - 1, step = 3) foreach { i =>
        tree(TIndex.trust(i)) shouldBe i
      }
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(size.asNonNegative))

    def `empty tree`: Unit =
      val tree = ArrayTree.empty[Int](using initialOneConfig)
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(TIndex(0)))

    def `root SingleLeaf`: Unit =
      val tree = ArrayTree.empty[Int](using initialOneConfig)
      tree append 7
      tree(TIndex.zero) shouldBe 7
      an[IndexOutOfBoundsException] shouldBe thrownBy(tree(TIndex(1)))

    def `root MultiLeaf`: Unit =
      populateAndCheck(smallConfig.initialCap)

    def `root LeafParentNode`: Unit =
      val caps0 = smallConfigLevelCaps(0)
      populateAndCheck(caps0.first + caps0.subsequent.asPositive)

    def `root UpperNode`: Unit =
      val caps1 = smallConfigLevelCaps(1)
      populateAndCheck(caps1.first + caps1.subsequent.asPositive * 3 + 2)

  object `config `:
    import ArrayTree.Config
    import ArrayTree.Config.LevelCap.*

    def `initial length`: Unit =
      Config(
        initialCap = 10,
        leafCap = 4,
        nodeCap = 4
      ).levelCaps should have length 8

    def `full levelSizes`: Unit =
      Config(
        initialCap = 1000,
        leafCap = 128,
        nodeCap = 4
      ).levelCaps should contain theSameElementsInOrderAs List(
        Full(1_000, 128, 1_384),
        Full(1_384, 512, 2_920),
        Full(2_920, 2_048, 9_064),
        Full(9_064, 8_192, 33_640),
        Full(33_640, 32_768, 131_944),
        Full(131_944, 131_072, 525_160),
        Full(525_160, 524_288, 2_098_024),
        Full(2_098_024, 2_097_152, 8_389_480)
      )

    def `partial levelSizes`: Unit =
      val config = Config(Capacity(1000), 32, 8)
      config.levelCaps.last match
        case Full(_, _, total) =>
          config.extendLevelCaps() shouldBe true
          config.levelCaps.last shouldBe a[Partial]
        case p: Partial =>
          fail()

    object `propagate index`:
      val initialCap = Capacity(100)
      val config     = Config(initialCap = initialCap, leafCap = 8, nodeCap = 4)

      def `illustrate levelCaps of config`: Unit =
        /* height left side    non-left side
         * ----------------------------------
         *      5     8.284
         *      4     2.140            2.048
         *      3       604              512
         *      2       220              128
         *      1       124               32
         *      0       100 8 8 8    8 8 8 8
         */
        config.levelCaps.take(4) should contain theSameElementsInOrderAs List(
          Full(100, 8, 124),
          Full(124, 32, 220),
          Full(220, 128, 604),
          Full(604, 512, 2_140)
        )

      def `index zero`: Unit =
        propagatedIndexes(startHeight = 1, index = TIndex.zero) shouldBe ArraySeq(0, 0)

      def `index below initial capacity`: Unit =
        val index = TIndex(97)
        propagatedIndexes(startHeight = 4, index) shouldBe ArraySeq(0, 0, 0, 0, index)

      def `index in first subsequent leaf`: Unit =
        val index = TIndex(103)
        propagatedIndexes(startHeight = 4, index) shouldBe
          ArraySeq(0, 0, 0, 1, index.value - initialCap.value)

      def `index in the middle of the tree`: Unit =
        propagatedIndexes(startHeight = 4, TIndex(1200)) shouldBe ArraySeq(2, 0, 2, 2, 4)

      def `index in the last leaf`: Unit =
        propagatedIndexes(startHeight = 5, TIndex(8276)) shouldBe ArraySeq(3, 3, 3, 3, 3, 0)

      private def propagatedIndexes[A](startHeight: Positive, index: TIndex, leftSide: Boolean = true): ArraySeq[Int] =
        import config.*
        withLevelCaps(startHeight) { height =>
          val buf = new Array[Index](height.incr.value)

          @tailrec def loop(capIndex: Index, leftSide: Boolean, i: TIndex, arrayIndex: Int): Index =
            val (slot, subIndex, _) = levelCaps(capIndex.value).locate(i, leftSide)
            buf(arrayIndex) = slot
            if capIndex.value > 0 then loop(capIndex.decr, leftSide && slot === 0, subIndex, arrayIndex + 1)
            else subIndex

          buf(height.value) = loop(height.asNonNegative.decr, leftSide, index, 0)
          ArraySeq.ofInt(buf.asInstanceOf[Array[Int]])
        }

  object `iterator `:
    given Config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size): Unit =
      val tree     = ArrayTree.empty[Int] tap (t => 0 until size.value foreach t.append)
      val expected = Array.tabulate(size.value)(identity)
      tree.iterator.toBuffer should contain theSameElementsInOrderAs expected

    def `size:  0`: Unit = check(0)
    def `size:  1`: Unit = check(1)
    def `size:  2`: Unit = check(2)
    def `size:  5`: Unit = check(5)
    def `size: 10`: Unit = check(10)
    def `size: 21`: Unit = check(21)

  object `reverse iterator`:
    given Config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size): Unit =
      val tree     = ArrayTree.empty[Int] tap (t => 1 to size.value foreach t.append)
      val expected = Array.tabulate(size.value)(size.value - _)
      tree.strongReverseIterator.toBuffer should contain theSameElementsInOrderAs expected
      tree.weakReverseIterator.toBuffer should contain theSameElementsInOrderAs expected

    def `size:  0`: Unit = check(0)
    def `size:  1`: Unit = check(1)
    def `size:  2`: Unit = check(2)
    def `size:  5`: Unit = check(5)
    def `size: 10`: Unit = check(10)
    def `size: 21`: Unit = check(21)

  object `reverse iterator from`:
    given Config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size, from: TIndex): Unit =
      val tree         = ArrayTree.empty[Int] tap (t => 1 to size.value foreach t.append)
      val expectedSize = from.incr.value
      val expected     = Array.tabulate(expectedSize)(expectedSize - _)
      tree.strongReverseIterator(from).toBuffer should contain theSameElementsInOrderAs expected
      tree.weakReverseIterator(from).toBuffer should contain theSameElementsInOrderAs expected

    def `size:  1, from  0`: Unit = check(1, 0)
    def `size:  2, from  1`: Unit = check(2, 1)
    def `size:  5, from  3`: Unit = check(5, 3)
    def `size: 10, from  7`: Unit = check(10, 7)
    def `size: 21, from 11`: Unit = check(21, 11)
    def `size: 21, from 21`: Unit = an[IndexOutOfBoundsException] shouldBe thrownBy(check(21, 21))

  object `reverse iterator with index`:
    given Config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size): Unit =
      val tree     = ArrayTree.empty[Int] tap (t => 1 to size.value foreach t.append)
      val expected = Array.tabulate(size.value)(n => (size.value - n) -> (size.value - n - 1))
      tree.strongReverseIteratorWithIndex.toBuffer should contain theSameElementsInOrderAs expected
      tree.weakReverseIteratorWithIndex.toBuffer should contain theSameElementsInOrderAs expected

    def `size:  0`: Unit = check(0)
    def `size:  1`: Unit = check(1)
    def `size:  2`: Unit = check(2)
    def `size:  5`: Unit = check(5)
    def `size: 10`: Unit = check(10)
    def `size: 21`: Unit = check(21)

  object `reverse iterator from with index`:
    given Config = Config(initialCap = 1, leafCap = 4, nodeCap = 2)

    private def check(size: Size, from: TIndex): Unit =
      val tree         = ArrayTree.empty[Int] tap (t => 1 to size.value foreach t.append)
      val expectedSize = from.incr.value
      val expected     = Array.tabulate(expectedSize)(n => (expectedSize - n) -> (expectedSize - n - 1))
      tree.strongReverseIteratorWithIndex(from).toBuffer should contain theSameElementsInOrderAs expected
      tree.weakReverseIteratorWithIndex(from).toBuffer should contain theSameElementsInOrderAs expected

    def `size:  1, from  0`: Unit = check(1, 0)
    def `size:  2, from  1`: Unit = check(2, 1)
    def `size:  5, from  3`: Unit = check(5, 3)
    def `size: 10, from  7`: Unit = check(10, 7)
    def `size: 21, from 11`: Unit = check(21, 11)
    def `size: 21, from 21`: Unit = an[IndexOutOfBoundsException] shouldBe thrownBy(check(21, 21))

  object `reverse iterator concurrently`:
    given ExecutionContext = ExecutionContext.global
    given Config           = Config(initialCap = 20, leafCap = 16, nodeCap = 4)

    private def check(size: Int): Unit =
      val tree    = ArrayTree.empty[Int]
      val writers = Future.sequence(for _ <- 1 to 4 yield Future(for _ <- 1 to size / 4 do tree append 1))
      val readers =
        type ErrMsg = String
        Range(1, size, step = size / 3).foldLeft(Future.successful(List.empty[Option[ErrMsg]])) { (acc, minSize) =>
          acc.flatMap { results =>
            Future {
              inline val count                         = 12
              val readingsWeak, readingsStrong         = new ArrayBuffer[Buffer[Int]](count)
              val readingsWeakFrom, readingsStrongFrom = new ArrayBuffer[(NonNegative, Buffer[Int])](count)

              // Start reader once `tree` has reached size `minSize` in order to increase failure ratio
              while tree.size < minSize do Thread.`yield`()

              def read(): Unit =
                readingsWeak += tree.weakReverseIterator.toBuffer
                readingsWeakFrom += {
                  val from = tree.size - 1
                  from -> tree.weakReverseIterator(from).toBuffer
                }
                readingsStrong += tree.strongReverseIterator.toBuffer
                readingsStrongFrom += {
                  val from = tree.size - 1
                  from -> tree.strongReverseIterator(from).toBuffer
                }

              // amplify
              for _ <- 1 to count do read()

              def checkValues: Option[ErrMsg] =
                List(
                  readingsWeak,
                  readingsStrong,
                  readingsWeakFrom.map(_._2),
                  readingsStrongFrom.map(_._2)
                ).iterator.flatten.find(_ exists (_ != 1)).map(buf => s"Expected 1 for all elements but got $buf.")

              def checkSizes: Option[ErrMsg] =
                type SizeFailure = (weak: Boolean, maxSize: NonNegative, actual: Int)
                def check(weak: Boolean, buffer: ArrayBuffer[(NonNegative, Buffer[Int])])(
                    expectation: (NonNegative, Buffer[Int]) => Boolean
                ): Option[SizeFailure] =
                  buffer.iterator
                    .find { case from -> result =>
                      !expectation(from + 1, result)
                    }
                    .map { case from -> result =>
                      (weak = weak, maxSize = from + 1, actual = result.size)
                    }

                def checkWeak: Option[ErrMsg] = check(true, readingsWeakFrom)(_.value >= _.size).map {
                  case (weak, expected, actual) => s"Expected size <= $expected but got $actual."
                }
                def checkStrong: Option[ErrMsg] = check(false, readingsStrongFrom)(_.value == _.size).map {
                  case (weak, expected, actual) => s"Expected size = $expected but got $actual."
                }

                checkWeak orElse checkStrong
              end checkSizes

              checkValues orElse checkSizes
            }.map(res => results :+ res)
          }
        }
      val both = readers zip writers
      withClue(f"$size%2d")(whenReady(both)((read, _) => read foreach (_ shouldBe empty)))

    def `size:  40`: Unit = check(40)
    def `size: 222`: Unit = check(222)
    def `size: 333`: Unit = check(333)
    def `size: 555`: Unit = check(555)

  object `apply factory`:
    enum Signature:
      case RepeatedParams, IterableOnce
    import Signature.*
    given Config = Config(initialCap = 8, leafCap = 4, nodeCap = 2)

    def check(size: Size, signature: Signature): Unit =
      val tree = signature match
        case RepeatedParams => ArrayTree(0, 1 until size.value*)
        case IterableOnce   => ArrayTree(0 until size.value)
      val expected = Array.tabulate(size.value)(identity)
      tree.size.value shouldBe expected.length
      tree.iterator.toBuffer should contain theSameElementsInOrderAs expected

    def `  1 RepeatedParams`: Unit = check(1, RepeatedParams)
    def `  3 RepeatedParams`: Unit = check(3, RepeatedParams)
    def ` 13 RepeatedParams`: Unit = check(13, RepeatedParams)
    def ` 37 RepeatedParams`: Unit = check(37, RepeatedParams)

    def `  1 IterableOnce`: Unit = check(1, IterableOnce)
    def `  3 IterableOnce`: Unit = check(3, IterableOnce)
    def ` 13 IterableOnce`: Unit = check(13, IterableOnce)
    def ` 37 IterableOnce`: Unit = check(37, IterableOnce)

  def `concurrent integration`: Unit =
    given ExecutionContext   = ExecutionContext.global
    given Config             = Config(initialCap = 1, leafCap = 4, nodeCap = 2)
    val tree: ArrayTree[Int] = ArrayTree.empty[Int]

    def append(values: Range): IndexedSeq[TIndex] = values map tree.append

    def clue(values: Range, indexes: IndexedSeq[TIndex]) =
      LazyString(() => s"""
                          |values: $values, indexes: ${indexes mkString "-"}
                          |""".stripMargin)

    def appendApply(values: Range): IndexedSeq[TIndex] =
      val indexes = append(values)
      withClue(clue(values, indexes)) {
        indexes.map(tree.apply) shouldBe values
      }
      indexes

    def appendIterate(values: Range): IndexedSeq[TIndex] =
      val indexes = append(values)
      withClue(clue(values, indexes)) {
        tree.weakReverseIterator.filter(values.contains).toBuffer shouldBe values.reverse
      }
      indexes

    def appendIterateFrom(values: Range): IndexedSeq[TIndex] =
      val indexes = append(values)
      withClue(clue(values, indexes)) {
        tree.weakReverseIterator(indexes(3)).filter(values.contains).toList shouldBe values.take(4).reverse
      }
      indexes

    val useCases = List(
      appendApply       -> Range(start = 1, end = 14),
      appendIterate     -> Range(start = 50, end = 72),
      appendIterateFrom -> Range(start = 100, end = 119)
    )
    val futures = Future.sequence(useCases map { case f -> values =>
      Future(f(values))
    })
    withClue(LazyString(() => tree.prettifyTree(includeNodes = true))) {
      whenReady(futures) { results =>
        results.flatten.size shouldBe useCases.map(_._2.size).sum
      }
    }

object ArrayTreeSpec:
  private val lineSeparator = System.lineSeparator
  private val sep           = s"$lineSeparator  "

  def equalTree(expected: Tree[Int]*): Matcher[ArrayTree[Int]] =
    Matcher { (tree: ArrayTree[Int]) =>
      def msg(key: String): String =
        s"""The tree
           |${tree.prettifyTree(includeNodes = true)}
           |$key the elements
           |  ${expected mkString sep}.
         """.stripMargin

      MatchResult(
        tree.treeIterator().toList == expected,
        "{0}",
        "{1}",
        Vector(
          LazyArg("does not have")(key => msg(key.toString)),
          LazyArg("has")(key => key.toString)
        )
      )
    }

  def coverRange(expectedIndexes: Range)(using tree: ArrayTree[Int]): Matcher[IndexedSeq[TIndex]] =
    Matcher { (indexes: IndexedSeq[TIndex]) =>
      def msg(key: String): String =
        s"""The actual indexes returned by `append`
           |  ${indexes.map(_.value).sorted mkString ", "}
           |$key to $expectedIndexes in the tree
           |${tree.prettifyTree(includeNodes = true)}
         """.stripMargin

      MatchResult(
        indexes.map(_.value).sum == expectedIndexes.sum,
        "{0}",
        "{1}",
        Vector(
          LazyArg("do not correspond")(key => msg(key.toString)),
          LazyArg("correspond")(key => key.toString)
        )
      )
    }

  private class LazyString(thunk: () => String):
    private lazy val value: String = thunk()
    override def toString: String  = value.toString
