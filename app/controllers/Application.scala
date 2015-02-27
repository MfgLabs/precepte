package controllers

import scala.concurrent.Future
import play.api._
import play.api.mvc._
import monitor._

import scalaz.std.scalaFuture._
import scalaz.syntax.applicative._

import play.api.libs.concurrent.Execution.Implicits._

object Application extends Controller {
	import models._
	import scalaz.syntax.monad._

  def index = Action { r =>
  	Ok
  }
}