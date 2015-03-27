package controllers

import scala.concurrent.Future

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import anorm._

import views._
import models._

import commons.Monitoring.TimedAction
import com.mfglabs.monitoring.{ Monitored, Call }
import Monitored._, Call._

import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.std.scalaFuture._
import scalaz.std.option._

/**
 * Manage a database of computers
 */
object Application extends Controller {

  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list(0, 2, ""))

  /**
   * Describe the computer form (used in both edit and create screens).
   */
  val computerForm = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
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
  def index = TimedAction { _ =>
    Future.successful(Home)
  }

  /**
   * Display the paginated list of computers.
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on computer names
   */
  def list(page: Int, orderBy: Int, filter: String) = TimedAction { implicit request =>
    for {
      cs <- Computer.list(page = page, orderBy = orderBy, filter = ("%"+filter+"%"))
    } yield Ok(html.list(cs, orderBy, filter))
  }

  /**
   * Display the 'edit form' of a existing Computer.
   *
   * @param id Id of the computer to edit
   */
  def edit(id: Long) = TimedAction { _ =>
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
  def update(id: Long) = TimedAction { implicit request =>
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
  def create = TimedAction { _ =>
    for {
      options <- Company.options
    } yield Ok(html.createForm(computerForm, options))
  }

  /**
   * Handle the 'new computer form' submission.
   */
  def save = TimedAction { implicit request =>
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
  def delete(id: Long) = TimedAction { _ =>
    for {
      _ <- Computer.delete(id)
    } yield Home.flashing("success" -> "Computer has been deleted")
  }

}

