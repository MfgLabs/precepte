/*
Copyright 2015 Mfg labs.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package com.mfglabs
package precepte
package corecats

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import scala.language.higherKinds

class SamplesSpec extends FlatSpec with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import cats.instances.future._
  import cats.instances.option._
  import cats.Applicative
  import cats.data.OptionT
  import cats.syntax.flatMap._
  import cats.syntax.apply._

  import default._
  import default.Macros._

  import state._

  val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))

  /** Metric Service
    * Expecting no injection so UnmanagedState = Unit
    */
  class Metric {
    // Type alias are always advised to make syntax nicer and help sometimes scalac with HK types
    type Pre[A] = DefaultPre[Future, Unit, A]

    // using a Precepte of future for the logger... yes this is expensive as it will as the exectx for a thread
    // but let's be pure for samples :D
    def metric(m: String): Pre[Unit] = Applicative[Pre].pure(println(m))
  }

  /** Logger Service
    * Expecting no injection so UnmanagedState = Unit
    */
  class Logger {
    // Type alias are always advised to make syntax nicer and help sometimes scalac with HK types
    type Pre[A] = DefaultPre[Future, Unit, A]

    // using a Precepte of future for the logger... yes this is expensive as it will as the exectx for a thread
    // but let's be pure for samples :D
    def info(m: String): Pre[Unit] = Applicative[Pre].pure(println(m))
  }

  case class Stuff(id: Long, name: String)

  case class DBCtx(
    logger: Logger
  )

  /** DB Service
    * Expecting a logger injection so UnmanagedState = Logger
    */
  class DB {
    // PreMP uses this implicit category to build Précepte in this scope
    implicit val category = Category.Database

    // Type alias are always advised to make syntax nicer and help sometimes scalac with HK types
    type Pre[A] = DefaultPre[Future, DBCtx, A]

    def findById(id: Long): Pre[Option[Stuff]] = PreMP { s:ST[DBCtx] =>
      for {
        // Logger returns a DefaultPre[Future, Unit, A] so you need to xmapState to convert injectors
        _ <- s.um.logger.info(s"searching $id").unify(s)
        // Performs a fake effect returning a value
        r <- Pre.liftF(Applicative[Future].pure(Some(Stuff(id, "toto"))))
      } yield (r)
    }
  }

  sealed trait PayResult
  case class Paid(id: Long, amount: Double) extends PayResult
  case class NotPaid(id: Long, amount: Double) extends PayResult


  case class PayCtx(
    logger: Logger,
    metric: Metric
  )
  // Payment Service that returns PreUnit
  class PayService {
    // PreMP uses this implicit category to build Précepte in this scope
    implicit object PaymentService extends Category("payment_service")

    // Type alias are always advised to make syntax nicer and help sometimes scalac with HK types
    type Pre[A] = DefaultPre[Future, PayCtx, A]

    def pay(stuff: Stuff, amount: Double): Pre[PayResult] = PreMP { s:ST[PayCtx] =>
      // IsoState is meant to allow conversions between Précepte with different UnmanagedState
      // val iso = IsoState(s); import iso._

      for {
        _ <- s.um.logger.info(s"$stuff going to pay $amount").unify(s)
        _ <- s.um.metric.metric(s"$stuff going to pay $amount").unify(s)
        r <- Pre.liftF(Future(Paid(stuff.id, amount)))
        _ <- s.um.logger.info(s"$stuff paid $amount").unify(s)
      } yield (r)
    }
  }

  /** The container to be injected at compile-time */
  case class Ctx (
    db: DB,
    logger: Logger,
    metric: Metric,
    payment: PayService
  )

  object Ctx {
    // Precepte requires Unmanaged to be a Semigroup to accumulate states along the workflow
    // Ctx is just a service container injected at compile-time so it won't change in anyway
    // so semigroup appends nothing and just keeps first Ctx...
    implicit val ctxSemigroup = new cats.Semigroup[Ctx] {
      def combine(f1: Ctx, f2: Ctx) = f1
    }
  }

  object ApiController {
    import scala.language.implicitConversions

    // PreMP uses this implicit category to build Précepte in this scope
    implicit val category = Category.Api

    type Pre[A] = DefaultPre[Future, Ctx, A]

    /** a Sample managing in a monadic flow */
    def pay0(id: Long, amount: Double): DefaultPre[Future, Ctx, PayResult] =
      // PreMP is a macro oriented helper that extracts the name of the function and the category from scope
      PreMP { s: ST[Ctx] =>
        (for {
          stuff <-  s.um.db.findById(id).unify(s)
          r     <-  stuff match {
                      case None => for {
                        _ <- s.um.logger.info(s"Could not find stuff $stuff").unify(s)
                      } yield (NotPaid(id, amount):PayResult)

                      case Some(stuff) => for {
                        _ <- s.um.logger.info(s"found stuff $stuff").unify(s)
                        r <- s.um.payment.pay(stuff, amount).unify(s)
                      } yield (r)
                    }
        } yield (r))
      }

    /** a Sample with OptionT monad transformer */
    def pay(id: Long, amount: Double): DefaultPre[Future, Ctx, Option[PayResult]] = PreMP { (s: ST[Ctx]) =>
      (for {
        stuff <- trans(s.um.db.findById(id).unify(s))
        _     <- trans(s.um.logger.info(s"found stuff $stuff").map(Option(_)).unify(s))
        r     <- trans(s.um.payment.pay(stuff, amount).map(Option(_)).unify(s))
      } yield (r)).value
    }
  }

  "Precepte" should "sample compile-time DI" in {

    // 1 - Describe
    val p = ApiController.pay0(12345L, 23.45)

    // 2 - Build initial state
    // build context to be injected
    val db = new DB()
    val logger = new Logger()
    val metric = new Metric()
    val payService = new PayService()
    val env = BaseEnv(Host("localhost"), Environment.Test, Version("1.0"))
    // inject context and initiate Span
    val s0 = ST(Span.gen, env, Vector.empty, Ctx(db, logger, metric, payService))

    // 3 - Execute
    val (graph, r) = p.graph(Graph.empty).eval(s0).futureValue

    // copy that string into http://www.webgraphviz.com/ and see you graph execution
    println(s"${graph.viz}")

    r should ===(Paid(12345L, 23.45))
  }
}
