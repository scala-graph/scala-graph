package scalax.collection.generic

import runtime._
import collection.immutable.WrappedString

/** Ordering for Any with the following rules:
  * numerical > string > any other reference type.
  * If a reference type is not a subclass of Ordered, it will be ordered according
  * to its toString value.
  *
  * @author Peter Empen
  */
class AnyOrdering[N] extends Ordering[N] {
  sealed protected trait Type
  sealed protected case class IntegerType(value: RichLong)     extends Type
  sealed protected case class FloatType(value: RichDouble)     extends Type
  sealed protected case class StringType(value: WrappedString) extends Type
  sealed protected case class RefType(value: AnyRef)           extends Type
  protected def typeOf(x: Any) = x match {
    case b: Byte                                    => IntegerType(new RichLong(b))
    case s: Short                                   => IntegerType(new RichLong(s))
    case i: Int                                     => IntegerType(new RichLong(i))
    case l: Long                                    => IntegerType(new RichLong(l))
    case f: Float                                   => FloatType(new RichDouble(f))
    case d: Double                                  => FloatType(new RichDouble(d))
    case _: Char | _: Unit | _: Boolean | _: String => StringType(new WrappedString(x.toString))
    case r: AnyRef                                  => RefType(r)
  }
  def compare(a: N, b: N): Int = (typeOf(a), typeOf(b)) match {
    case (IntegerType(a), IntegerType(b))                               => a.compare(b.self)
    case (IntegerType(a), FloatType(b))                                 => a.toDouble.compare(b.self)
    case (IntegerType(_), StringType(_)) | (IntegerType(_), RefType(_)) => -1

    case (FloatType(a), FloatType(b))                               => a.compare(b.self)
    case (FloatType(a), IntegerType(b))                             => a.compare(b.self)
    case (FloatType(_), StringType(_)) | (FloatType(_), RefType(_)) => -1

    case (StringType(a), StringType(b))                                  => a.toString.compare(b.toString)
    case (StringType(_), IntegerType(_)) | (StringType(_), FloatType(_)) => 1
    case (StringType(_), RefType(_))                                     => -1

    case (RefType(a), RefType(b)) =>
      def fallback = a.toString.compare(b.toString)
      (a, b) match {
        case (a: Ordered[_], b: Ordered[_]) =>
          try a.asInstanceOf[Ordered[AnyRef]].compare(b.asInstanceOf[AnyRef with Ordered[AnyRef]])
          catch {
            case e: ClassCastException => fallback
          }
        case _ => fallback
      }
    case (RefType(_), IntegerType(_)) | (RefType(_), FloatType(_)) | (RefType(_), StringType(_)) => 1
  }
}
