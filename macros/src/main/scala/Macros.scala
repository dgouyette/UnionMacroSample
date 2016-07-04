import scala.reflect.macros.blackbox
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object union {


  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def argByXY(idx: Int, i: Int) = if (idx == i) {
      q"""Some(t)"""
    }
    else {
      q"""None"""
    }


    val result = {
      annottees.map(_.tree).toList match {
        case q"class $name[..$tpes](..$lFields) extends ..$parents  { ..$body }" :: Nil =>
          val typesName: Seq[c.universe.TypeName] = tpes.map { case TypeDef(_, typeName, _, _) => typeName }
          q"""
            class $name[..$tpes](..$lFields) extends ..$parents   {
              ..$body
             ..${
            typesName.zipWithIndex.map { case (currentTypeName, y) =>
              val currentType2Union = TermName("toUnion" + y)
              val unionTermName = TermName("Union" + tpes.size)
              val constructorArgs = for (x <- typesName.indices) yield q"""${argByXY(x, y)}"""

              q"""
               implicit def $currentType2Union(t: $currentTypeName) = $unionTermName[..$typesName](..$constructorArgs);
               """
            }
          }
            }
          """
      }
    }
    println(result)

    c.Expr[Any](result)
  }
}

class union extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro union.impl
}
