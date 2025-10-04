package scalax.collection.io.jsoniter

import scala.compiletime.summonInline
import scala.quoted.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalax.collection.generic.Edge

object EdgeCodec:

  /** Macro to generate codec for polymorphic but non-ADT edge type.
    *
    * This macro is provided because predefined edges do not build an ADT and
    * jsoniter-scala does not deal with polymorphic, non-ADT types.
    * You can invoke this macro by providing the root type along with all subtypes that are used in the Graph.
    * For instance, provided the node type `String` and a mixed Graph using `DiEdge` and `UnDiEdge`, call
    *
    * `EdgeCodec.makePolymorphicWithEmbeddedNodes[AnyEdge[String], (UnDiEdge[String], DiEdge[String])]()`
    *
    * @param discriminatorFieldName the key of the JSON discriminator with default `"type"`.
    * @tparam E the root edge type.
    * @tparam Subtypes all subtypes of `E` used in the Graph.
    */
  inline def makePolymorphicWithEmbeddedNodes[E <: Edge[_], Subtypes <: Tuple](
      discriminatorFieldName: String = "type"
  ): JsonValueCodec[E] =
    makePolymorphic[E, Subtypes](discriminatorFieldName)

  private inline def makePolymorphic[E, Subtypes <: Tuple](
      discriminatorFieldName: String
  ): JsonValueCodec[E] =
    ${ makePolymorphicImpl[E, Subtypes]('discriminatorFieldName) }

  private def makePolymorphicImpl[E: Type, Subtypes <: Tuple: Type](
      discriminatorExpr: Expr[String]
  )(using Quotes): Expr[JsonValueCodec[E]] = {
    import quotes.reflect.*

    def extractTupleTypes(tuple: TypeRepr): List[TypeRepr] = tuple.asType match
      case '[EmptyTuple]   => Nil
      case '[head *: tail] => TypeRepr.of[head] :: extractTupleTypes(TypeRepr.of[tail])
      case _               => report.errorAndAbort(s"Expected tuple of types, but got: ${tuple.show}")

    val subtypes: List[TypeRepr] = extractTupleTypes(TypeRepr.of[Subtypes])

    val entries: List[Expr[(String, JsonValueCodec[_ <: E])]] = subtypes.map { subtype =>
      val name = subtype.classSymbol.map(_.name).getOrElse("Unknown")
      subtype.asType match {
        case '[s] =>
          val codecExpr = Expr.summon[JsonValueCodec[s]].getOrElse {
            report.errorAndAbort(s"Cannot find JsonValueCodec for ${Type.show[s]}")
          }
          '{ (${ Expr(name) }, $codecExpr.asInstanceOf[JsonValueCodec[_ <: E]]) }
      }
    }

    val decoderMapExpr: Expr[Map[String, JsonValueCodec[_ <: E]]] =
      '{ Map[String, JsonValueCodec[_ <: E]](${ Expr.ofList(entries) }: _*) }

    val encoderExpr: Expr[E => (String, JsonValueCodec[_ <: E])] = {
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
          _ => List(TypeRepr.of[E]),
          _ => TypeRepr.of[(String, JsonValueCodec[_ <: E])]
        ),
        rhsFn = (owner, params) => {
          val xRef = params.head.asInstanceOf[Term]
          Match(xRef, cases).changeOwner(owner)
        }
      )

      lambdaExpr.asExprOf[E => (String, JsonValueCodec[_ <: E])]
    }

    '{
      codecForTrait[E](
        $decoderMapExpr,
        $encoderExpr,
        $discriminatorExpr
      )
    }
  }

  private def codecForTrait[E](
      decoderMap: Map[String, JsonValueCodec[_ <: E]],
      encoder: E => (String, JsonValueCodec[_ <: E]),
      discriminatorFieldName: String
  ): JsonValueCodec[E] = new JsonValueCodec[E] {

    override def decodeValue(in: JsonReader, default: E): E =
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

    override def encodeValue(edge: E, out: JsonWriter): Unit =
      val (discriminator, codec: JsonValueCodec[E @unchecked]) = encoder(edge): @unchecked
      out.writeObjectStart()
      out.writeKey(discriminatorFieldName)
      out.writeVal(discriminator)
      codec.encodeValue(edge, out)
      out.writeObjectEnd()

    override def nullValue: E = null.asInstanceOf[E]
  }
