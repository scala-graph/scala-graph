package scalax.collection.io.jsoniter

import scala.compiletime.summonInline
import scala.quoted.*

import com.github.plokhotnyuk.jsoniter_scala.core.*

import scalax.collection.generic.Edge

object EdgeCodec:

  inline def makePolymorphicWithEmbeddedNodes[E <: Edge[_], Subtypes <: Tuple](
      onJsonNull: => E,
      discriminatorFieldName: String = "type"
  ): JsonValueCodec[E] =
    makePolymorphic[E, Subtypes](onJsonNull, discriminatorFieldName)

  private inline def makePolymorphic[A, Subtypes <: Tuple](
      onJsonNull: => A,
      discriminatorFieldName: String
  ): JsonValueCodec[A] =
    ${ makePolymorphicImpl[A, Subtypes]('onJsonNull, 'discriminatorFieldName) }

  private def makePolymorphicImpl[A: Type, Subtypes <: Tuple: Type](
      onJsonNull: Expr[A],
      discriminatorExpr: Expr[String]
  )(using Quotes): Expr[JsonValueCodec[A]] = {
    import quotes.reflect.*

    def extractTupleTypes(tuple: TypeRepr): List[TypeRepr] = tuple.asType match
      case '[EmptyTuple]   => Nil
      case '[head *: tail] => TypeRepr.of[head] :: extractTupleTypes(TypeRepr.of[tail])
      case _               => report.errorAndAbort(s"Expected tuple of types, but got: ${tuple.show}")

    val subtypes: List[TypeRepr] = extractTupleTypes(TypeRepr.of[Subtypes])

    val entries: List[Expr[(String, JsonValueCodec[_ <: A])]] = subtypes.map { subtype =>
      val name = subtype.classSymbol.map(_.name).getOrElse("Unknown")
      subtype.asType match {
        case '[s] =>
          val codecExpr = Expr.summon[JsonValueCodec[s]].getOrElse {
            report.errorAndAbort(s"Cannot find JsonValueCodec for ${Type.show[s]}")
          }
          '{ (${ Expr(name) }, $codecExpr.asInstanceOf[JsonValueCodec[_ <: A]]) }
      }
    }

    val decoderMapExpr: Expr[Map[String, JsonValueCodec[_ <: A]]] =
      '{ Map[String, JsonValueCodec[_ <: A]](${ Expr.ofList(entries) }: _*) }

    val encoderExpr: Expr[A => (String, JsonValueCodec[_ <: A])] = {
      val cases = subtypes.map { subtype =>
        val typeName = subtype.classSymbol.map(_.name).getOrElse("Unknown")
        subtype.asType match {
          case '[s] =>
            val patSym = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, subtype)
            val pat    = Bind(patSym, Typed(Ref(patSym), TypeTree.of[s]))
            val rhs    = '{
              (${ Expr(typeName) }, summonInline[JsonValueCodec[s]])
            }.asTerm
            CaseDef(pat, None, rhs)
        }
      }

      val lambdaExpr = Lambda(
        owner = Symbol.spliceOwner,
        tpe = MethodType(List("x"))(
          _ => List(TypeRepr.of[A]),
          _ => TypeRepr.of[(String, JsonValueCodec[_ <: A])]
        ),
        rhsFn = (owner, params) => {
          val xRef = params.head.asInstanceOf[Term]
          Match(xRef, cases).changeOwner(owner)
        }
      )

      lambdaExpr.asExprOf[A => (String, JsonValueCodec[_ <: A])]
    }

    '{
      codecForTrait[A](
        $decoderMapExpr,
        $encoderExpr,
        $onJsonNull,
        $discriminatorExpr
      )
    }
  }

  private def codecForTrait[T](
      decoderMap: Map[String, JsonValueCodec[_ <: T]],
      encoder: T => (String, JsonValueCodec[_ <: T]),
      onJsonNull: => T,
      discriminatorFieldName: String
  ): JsonValueCodec[T] = new JsonValueCodec[T] {

    override def decodeValue(in: JsonReader, default: T): T =
      if in.isNextToken('{') then
        val keyLen = in.readKeyAsCharBuf()
        if in.isCharBufEqualsTo(keyLen, discriminatorFieldName) then
          val discriminator = in.readString(null)
          if !in.isNextToken(',') then in.objectEndOrCommaError()
          decoderMap.get(discriminator) match {
            case Some(codec) =>
              val decoded = codec.decodeValue(in, codec.nullValue)
              if !in.isNextToken('}') then in.objectEndOrCommaError()
              decoded
            case None => in.discriminatorError()
          }
        else in.unexpectedKeyError(keyLen)
      else in.objectStartOrNullError()

    override def encodeValue(x: T, out: JsonWriter): Unit =
      val (discriminator, codec: JsonValueCodec[T @unchecked]) = encoder(x): @unchecked
      out.writeObjectStart()
      out.writeKey(discriminatorFieldName)
      out.writeVal(discriminator)
      codec.encodeValue(x, out)
      out.writeObjectEnd()

    override def nullValue: T = onJsonNull
  }
