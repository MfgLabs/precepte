package env

import play.api.ApplicationLoader.Context
import play.api.db.{DBComponents}
import play.api.db.evolutions.EvolutionsComponents
import router.Routes
import play.api.{Application, ApplicationLoader, Environment, Logger}

final class BootstrapLoader extends ApplicationLoader {
  def load(context: Context): Application = {
    // Bootstrap the injected application
    Logger.info("All good")
    new env.ApplicationEnv(context).application
  }
}

final class ApplicationEnv(context: Context)
    extends play.api.BuiltInComponentsFromContext(context)
    with controllers.AssetsComponents
    with DBComponents
    with EvolutionsComponents {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val connectionPool =
    new play.api.db.HikariCPConnectionPool(Environment.simple())
  val db = dbApi.database("default")

  object Services {
    val companyDB = new models.CompanyDB(db)
    val computerDB = new models.ComputerDB(db)
  }

  object Controllers {
    val application = new controllers.Application(controllerComponents,
                                                  Services.companyDB,
                                                  Services.computerDB)
  }

  val httpFilters = Seq.empty
  val router = new Routes(httpErrorHandler, Controllers.application, assets)

  applicationEvolutions.start()
}
