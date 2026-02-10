package scalax.util.invoke

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Some specific, optimized `VarHandle` functionality to be used for arrays with unset or immutably set elements.
  * For best possible efficiency, `VarHandle` instances are `static final`.
  */
trait ArrayVarHandle[A]:
  def Undefined: A

  /** Instantiates an array initialized to the undefined value specific to `A`. */
  def newArray(length: Int): Array[A]

  def get(array: Array[A], index: Int): A

  /** Reads the element and, if unset, poll for the set value.
    * Unset elements are identified by a `null` value or a specific constant primitive value for undefined.
    *
    * @return the element not equal to `null` respectively the undefined value for primitives.
    */
  def pollAcquire(array: Array[A], index: Int): A
  def setRelease(array: Array[A], index: Int, value: A): Unit

object ArrayVarHandle extends LowPriorityHandles:
  given ArrayVarHandle[Int] with
    import VarHandleIntrinsics.{getAcquireInt, getInt, setReleaseInt}

    inline def Undefined: Int.MinValue.type = Int.MinValue

    def newArray(length: Int): Array[Int] = Array.fill[Int](length)(Undefined)

    def get(array: Array[Int], index: Int): Int =
      getInt(array, index)

    def pollAcquire(array: Array[Int], index: Int): Int =
      val elem = getInt(array, index)
      if elem != Undefined then elem
      else
        @tailrec def loop: Int =
          Thread.onSpinWait()
          getAcquireInt(array, index) match
            case Undefined => loop
            case elem      => elem
        loop

    def setRelease(array: Array[Int], index: Int, value: Int): Unit =
      setReleaseInt(array, index, value)

  given ArrayVarHandle[Long] with
    import VarHandleIntrinsics.{getAcquireLong, getLong, setReleaseLong}

    inline def Undefined: Long.MinValue.type = Long.MinValue

    def newArray(length: Int): Array[Long] = Array.fill[Long](length)(Undefined)

    def get(array: Array[Long], index: Int): Long =
      getLong(array, index)

    def pollAcquire(array: Array[Long], index: Int): Long =
      val elem = getLong(array, index)
      if elem != Undefined then elem
      else
        @tailrec def loop: Long =
          Thread.onSpinWait()
          getAcquireLong(array, index) match
            case Undefined => loop
            case elem      => elem
        loop

    def setRelease(array: Array[Long], index: Int, value: Long): Unit =
      setReleaseLong(array, index, value)

protected trait LowPriorityHandles:
  import VarHandleIntrinsics.{getAcquireObject, getObject, setReleaseObject}

  given anyRefHandle[A <: AnyRef]: ArrayVarHandle[A] with
    inline def Undefined: A = null.asInstanceOf[A]

    def newArray(length: Int): Array[A] = new Array(length).asInstanceOf[Array[A]]

    def get(array: Array[A], index: Int): A =
      scalax.util.invoke.VarHandleIntrinsics.getObject(array.asInstanceOf[Array[AnyRef]], index).asInstanceOf[A]

    def pollAcquire(array: Array[A], index: Int): A =
      val elem = getObject(array.asInstanceOf[Array[AnyRef]], index).asInstanceOf[A]
      if elem ne null then elem
      else
        @tailrec def loop: A =
          Thread.onSpinWait()
          getAcquireObject(array.asInstanceOf[Array[AnyRef]], index).asInstanceOf[A] match
            case null => loop
            case elem => elem
        loop

    def setRelease(array: Array[A], index: Int, value: A): Unit =
      setReleaseObject(array.asInstanceOf[Array[AnyRef]], index, value)
