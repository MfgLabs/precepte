package models

import java.util.Date
import anorm._
import anorm.SqlParser._
import scala.language.postfixOps
import scala.concurrent.Future

import com.mfglabs.precepte._
import default._
import commons.Monitoring
import Monitoring._
import Macros._

case class Company(id: Option[Long] = None, name: String)
case class Computer(id: Option[Long] = None, name: String, introduced: Option[Date], discontinued: Option[Date], companyId: Option[Long])

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

object Models {
  def Timed[A](f: ST[Unit] => Future[A])(implicit callee: Callee): DPre[Future, Unit, A] =
    Pre(BaseTags(callee, Category.Database)) { (st: ST[Unit]) => f(st) }
}

class ComputerDB (
  db: play.api.db.Database
) {
  /**
   * Retrieve a computer from the id.
   */
  def findById(id: Long) =
    Models.Timed { (st: ST[Unit]) =>
      val ctx = MonitoringContext(st)
      import ctx._
      logger.debug(s"Finding computer with id", Macros.param(id))
      Future.successful {
        db.withConnection { implicit connection =>
          SQL("select * from computer where id = {id}").on('id -> id).as(ComputerDB.simple.singleOpt)
        }
      }
    }

  /**
   * Return a page of (Computer,Company).
   *
   * @param page Page to display
   * @param pageSize Number of computers per page
   * @param orderBy Computer property used for sorting
   * @param filter Filter applied on the name column
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%") =
    Models.Timed { (st: ST[Unit]) =>
      val ctx = MonitoringContext(st)
      import ctx._
      logger.debug(s"Listing all computers", Macros.params(page, pageSize, orderBy, filter))
      Future.successful {
        val offest = pageSize * page

        db.withConnection { implicit connection =>

          val computers = SQL(
            """
              select * from computer
              left join company on computer.company_id = company.id
              where computer.name like {filter}
              order by {orderBy} nulls last
              limit {pageSize} offset {offset}
            """
          ).on(
            'pageSize -> pageSize,
            'offset -> offest,
            'filter -> filter,
            'orderBy -> orderBy
          ).as(ComputerDB.withCompany *)

          val totalRows = SQL(
            """
              select count(*) from computer
              left join company on computer.company_id = company.id
              where computer.name like {filter}
            """
          ).on(
            'filter -> filter
          ).as(scalar[Long].single)

          Page(computers, page, offest.toLong, totalRows)
        }
      }

    }

  /**
   * Update a computer.
   *
   * @param id The computer id
   * @param computer The computer values.
   */
  def update(id: Long, computer: Computer) = Models.Timed { (st: ST[Unit]) =>
    val ctx = MonitoringContext(st)
    import ctx._
    logger.info(s"updating computer with id", Macros.params(id, computer))
    Future.successful {
      db.withConnection { implicit connection =>
        SQL(
          """
            update computer
            set name = {name}, introduced = {introduced}, discontinued = {discontinued}, company_id = {company_id}
            where id = {id}
          """
        ).on(
          'id -> id,
          'name -> computer.name,
          'introduced -> computer.introduced,
          'discontinued -> computer.discontinued,
          'company_id -> computer.companyId
        ).executeUpdate()
      }
    }
  }

  /**
   * Insert a new computer.
   *
   * @param computer The computer values.
   */
  def insert(computer: Computer) = Models.Timed { (st: ST[Unit]) =>
    val ctx = MonitoringContext(st)
    import ctx._
    logger.info(s"inserting computer", Macros.param(computer))
    Future.successful {
      db.withConnection { implicit connection =>
        SQL(
          """
            insert into computer values (
              (select next value for computer_seq),
              {name}, {introduced}, {discontinued}, {company_id}
            )
          """
        ).on(
          'name -> computer.name,
          'introduced -> computer.introduced,
          'discontinued -> computer.discontinued,
          'company_id -> computer.companyId
        ).executeUpdate()
      }
    }
  }

  /**
   * Delete a computer.
   *
   * @param id Id of the computer to delete.
   */
  def delete(id: Long) = Models.Timed { (st: ST[Unit]) =>
    val ctx = MonitoringContext(st)
    import ctx._
    logger.info(s"deleting computer", Macros.param(id))
    Future.successful {
      db.withConnection { implicit connection =>
        SQL("delete from computer where id = {id}").on('id -> id).executeUpdate()
      }
    }
  }
}

object ComputerDB {
  /**
   * Parse a Computer from a ResultSet
   */
  val simple = {
    get[Option[Long]]("computer.id") ~
    get[String]("computer.name") ~
    get[Option[Date]]("computer.introduced") ~
    get[Option[Date]]("computer.discontinued") ~
    get[Option[Long]]("computer.company_id") map {
      case id ~ name ~ introduced ~ discontinued ~ companyId => Computer(id, name, introduced, discontinued, companyId)
    }
  }

  /**
   * Parse a (Computer,Company) from a ResultSet
   */
  val withCompany = ComputerDB.simple ~ (CompanyDB.simple ?) map {
    case computer ~ company => (computer,company)
  }
}

class CompanyDB (db: play.api.db.Database) {
  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options = Models.Timed { (st: ST[Unit]) =>
    val ctx = MonitoringContext(st)
    import ctx._
    logger.debug("Listing options")
    Future.successful {
      db.withConnection { implicit connection =>
        SQL("select * from company order by name").as(CompanyDB.simple *)
          .filter(_.id.isDefined)
          .map(c => c.id.get.toString -> c.name)
      }
    }
  }

}

object CompanyDB {
  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Option[Long]]("company.id") ~
    get[String]("company.name") map {
      case id~name => Company(id, name)
    }
  }
}
