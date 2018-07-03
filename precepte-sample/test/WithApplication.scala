import org.specs2.execute._
import org.specs2.mutable._
import org.specs2.specification.Scope

import play.api._

abstract class WithApplication extends Around with Scope {
  lazy val context = ApplicationLoader.createContext(
    new Environment(new java.io.File("."), ApplicationLoader.getClass.getClassLoader, Mode.Test)
  )

  lazy val applicationEnv = new env.ApplicationEnv(context)
  implicit lazy val app   = applicationEnv.application

  lazy val Controllers = applicationEnv.Controllers
  lazy val Services    = applicationEnv.Services

  def around[T: AsResult](t: => T): Result = {
    play.api.test.Helpers.running(app)(AsResult.effectively(t))
  }
}
