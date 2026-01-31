package scalax.util.invoke

import java.lang.invoke.VarHandle
import java.lang.invoke.MethodHandles.arrayElementVarHandle
import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Some specific, optimized `VarHandle` functionality to be used for arrays with unset or immutably set elements.
  * For best possible efficiency, `VarHandle` instances are `static final`.
  */
trait ArrayVarHandle[A]:
  /** Instantiates an array initialized to the undefined value specific to `A`. */
  def newArray(length: Int): Array[A]

  /** Reads the element and, if unset, poll for the set value.
    * Unset elements are identified by a `null` value or a specific constant primitive value for undefined.
    *
    * @return the element not equal to `null` respectively the undefined value for primitives.
    */
  def pollAcquire(array: Array[A], index: Int): A
  def setRelease(array: Array[A], index: Int, value: A): Unit

object ArrayVarHandle extends LowPriorityHandles:
  final private val intArrayHandle: VarHandle  = arrayElementVarHandle(classOf[Array[Int]])
  final private val longArrayHandle: VarHandle = arrayElementVarHandle(classOf[Array[Long]])

  given ArrayVarHandle[Int] with
    private inline val Undefined: Int.MinValue.type = Int.MinValue

    def newArray(length: Int): Array[Int] = Array.fill[Int](length)(Undefined)

    inline def pollAcquire(array: Array[Int], index: Int): Int =
      val elem = intArrayHandle.get(array, index).asInstanceOf[Int]
      if elem != Undefined then elem
      else
        @tailrec def loop: Int =
          Thread.onSpinWait()
          intArrayHandle.getAcquire(array, index).asInstanceOf[Int] match
            case Undefined => loop
            case elem      => elem
        loop

    inline def setRelease(array: Array[Int], index: Int, value: Int): Unit =
      intArrayHandle.setRelease(array, index, value)

  given ArrayVarHandle[Long] with
    private inline val Undefined: Long.MinValue.type = Long.MinValue

    def newArray(length: Int): Array[Long] = Array.fill[Long](length)(Undefined)

    inline def pollAcquire(array: Array[Long], index: Int): Long =
      val elem = longArrayHandle.get(array, index).asInstanceOf[Long]
      if elem != Undefined then elem
      else
        @tailrec def loop: Long =
          Thread.onSpinWait()
          longArrayHandle.getAcquire(array, index).asInstanceOf[Long] match
            case Undefined => loop
            case elem      => elem
        loop

    inline def setRelease(array: Array[Long], index: Int, value: Long): Unit =
      longArrayHandle.setRelease(array, index, value)

protected trait LowPriorityHandles:
  final private val anyRefArrayHandle: VarHandle = arrayElementVarHandle(classOf[Array[AnyRef]])

  given anyRefHandle[A <: AnyRef]: ArrayVarHandle[A] with
    def newArray(length: Int): Array[A] = new Array(length).asInstanceOf[Array[A]]

    def pollAcquire(array: Array[A], index: Int): A =
      val elem = anyRefArrayHandle.get(array, index).asInstanceOf[A]
      if elem ne null then elem
      else
        @tailrec def loop: A =
          Thread.onSpinWait()
          anyRefArrayHandle.getAcquire(array, index).asInstanceOf[A] match
            case null => loop
            case elem => elem
        loop

    inline def setRelease(array: Array[A], index: Int, value: A): Unit =
      anyRefArrayHandle.setRelease(array, index, value)
