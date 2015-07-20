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
    a <- Pre(tags("f1")){ case PState(s, ctx) => 
      Future { s"Received ${ctx.content.value}" }
    }
  } yield (a)
}

object Main extends App {


}