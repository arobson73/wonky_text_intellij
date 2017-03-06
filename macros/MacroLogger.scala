package macrologgers
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    c.Expr[Unit](q"""println("Hello World")""")
  }
  def hello: Unit = macro impl
}


object SimpleMacroLogger {
  private val on = true

  def info(msg: String): Unit = macro info_impl

  def info_impl(c: Context)(msg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    if (on) {
      reify {
        println(msg.splice)
      }
    } else {
      reify {
        // Nothing
      }
    }
  }
}
