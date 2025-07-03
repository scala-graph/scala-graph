package scalax.collection.io.json.serializer

import net.liftweb.json._

class PositionedSerializer[A: Manifest](
    val fromJson: PartialFunction[JValue, A],
    val toJson: PartialFunction[Any, JValue]
) extends CustomSerializer[A](_ => (fromJson, toJson))

// Beware that serializers need be defined at top level.

/** Converts a `Boolean` to a `JBool` as opposed to `JField`.
  */
object BooleanSerializer extends PositionedSerializer[Boolean]({ case JBool(b) => b }, { case b: Boolean => JBool(b) })

/** Converts an `BigInt` to a `JInt` as opposed to `JField`.
  * To use this for `Int` or `Long`, import the corresponding implicit conversions provided by `BigInt`.
  */
object BigIntSerializer extends PositionedSerializer[BigInt]({ case JInt(b) => b }, { case b: BigInt => JInt(b) })

/** Converts a `Double` to a `JDouble` as opposed to `JField`.
  */
object DoubleSerializer extends PositionedSerializer[Double]({ case JDouble(d) => d }, { case d: Double => JDouble(d) })

/** Converts a `String` to a `JString` as opposed to `JField`.
  */
object StringSerializer extends PositionedSerializer[String]({ case JString(s) => s }, { case s: String => JString(s) })

/** Converts a `Char` to a `JString` as opposed to `JField`.
  */
object CharSerializer
    extends PositionedSerializer[Char]({ case JString(s) => s.head }, { case c: Char => JString(c.toString) })

/** Converts `(A, B)` to a `JArray` of any two `JValue`s as opposed to `JField`s.
  */
class Tuple2Serializer[A: Manifest, B: Manifest](
    aSer: PositionedSerializer[A],
    bSer: PositionedSerializer[B]
) extends CustomSerializer[(A, B)](_ =>
      (
        { case JArray(a :: b :: Nil) => (aSer.fromJson(a), bSer.fromJson(b)) },
        { case (a, b) => JArray(aSer.toJson(a) :: bSer.toJson(b) :: Nil) }
      )
    )

/** Converts `(A, B, C)` to a `JArray` of any three `JValue`s as opposed to `JField`s.
  */
class Tuple3Serializer[A: Manifest, B: Manifest, C: Manifest](
    aSer: PositionedSerializer[A],
    bSer: PositionedSerializer[B],
    cSer: PositionedSerializer[C]
) extends CustomSerializer[(A, B, C)](_ =>
      (
        { case JArray(a :: b :: c :: Nil) => (aSer.fromJson(a), bSer.fromJson(b), cSer.fromJson(c)) },
        { case (a, b, c) => JArray(aSer.toJson(a) :: bSer.toJson(b) :: cSer.toJson(c) :: Nil) }
      )
    )
