package controllers

import scala.concurrent.Future

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._

import com.mfglabs.precepte._
import corescalaz._
import com.mfglabs.precepte.default._
import Macros.callee

import commons.Monitoring._

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.std.scalaFuture._
import scalaz.std.option._

trait Commons {
  import commons.Monitoring.env
  private def initialState = ()
  private def version = env.version
  private def environment = env.environment
  private def host = env.host

  val syntax = PreActionSyntax(initialState, version, environment, host)
}

/**
 * Manage a database of computers
 */
object Application extends Controller with Commons {

  import syntax._

  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list(0, 2, ""))

  /**
   * Describe the computer form (used in both edit and create screens).
   */
  val computerForm = Form(
    mapping(
      "id" -> ignored[Option[Long]](None),
      "name" -> nonEmptyText,
      "introduced" -> optional(date("yyyy-MM-dd")),
      "discontinued" -> optional(date("yyyy-MM-dd")),
      "company" -> optional(longNumber)
    )(Computer.apply)(Computer.unapply)
  )

  // -- Actions

  /**
   * Handle default path requests, redirect to computers list
   */
  def index =
    future(TimedAction) { _ =>
      Future.successful(Home)
    }

  /**
   * Display the paginated list of computers.
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on computer names
   */
  def list(page: Int, orderBy: Int, filter: String) = async(TimedAction) { req =>
    implicit val r = req._2
    for {
      cs <- Computer.list(page = page, orderBy = orderBy, filter = ("%"+filter+"%"))
    } yield Ok(html.list(cs, orderBy, filter))
  }

  /**
   * Display the 'edit form' of a existing Computer.
   *
   * @param id Id of the computer to edit
   */
  def edit(id: Long) = async(TimedAction) { _ =>
    (for {
      computer <- trans(Computer.findById(id))
      options <- trans(Company.options.lift[Option])
    } yield {
      Ok(html.editForm(id, computerForm.fill(computer), options))
    }).run.map(_.getOrElse(NotFound))
  }

  /**
   * Handle the 'edit form' submission
   *
   * @param id Id of the computer to edit
   */
  def update(id: Long) = async(TimedAction) { req =>
    implicit val r = req._2
    computerForm.bindFromRequest.fold(
      formWithErrors =>
        for {
          options <- Company.options
        } yield BadRequest(html.editForm(id, formWithErrors, options)),
      computer =>
        for {
          _ <- Computer.update(id, computer)
        } yield Home.flashing("success" -> "Computer %s has been updated".format(computer.name)))
  }

  /**
   * Display the 'new computer form'.
   */
  def create = async(TimedAction) { _ =>
    for {
      options <- Company.options
    } yield Ok(html.createForm(computerForm, options))
  }

  /**
   * Handle the 'new computer form' submission.
   */
  def save = async(TimedAction) { req =>
    implicit val r = req._2
    computerForm.bindFromRequest.fold(
      formWithErrors =>
        for {
          options <- Company.options
        } yield BadRequest(html.createForm(formWithErrors, options)),
      computer => {
        for {
          _ <- Computer.insert(computer)
        } yield Home.flashing("success" -> "Computer %s has been created".format(computer.name))
      }
    )
  }

  /**
   * Handle computer deletion.
   */
  def delete(id: Long) = async(TimedAction) { _ =>
    for {
      _ <- Computer.delete(id)
    } yield Home.flashing("success" -> "Computer has been deleted")
  }

}

