package com.mfglabs
package precepte


import scala.concurrent.{ Future, ExecutionContext }
import scalaz.std.scalaFuture._
import scalaz.syntax.monad._

import Default._


object Business {

  case class Config(value: Map[String, String])
  case class Content(value: String)

  case class Ctx(
    cfg: Config
  , content: Content
  )

  val scope = PreScope[Future, Ctx]

  import scope._

  def logic(implicit exe: ExecutionContext) = for {
    a <-  Pre(tags("phase_1")){ (s, ctx) => 
            Future { s"${ctx.content.value} - phase_1" }
          }
    b <-  Pre(tags("phase_2")){ (s, ctx) => 
            Future { s"$a - phase_2" }
          }
  } yield (b)
}

object Main extends App {


}