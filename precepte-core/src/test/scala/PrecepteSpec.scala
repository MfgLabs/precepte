package com.mfglabs.monitoring

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span}

import scala.language.higherKinds

class PrecepteSpec extends FlatSpec with ScalaFutures {

  import Precepte._
  import Call.Tags
  trait Log {
    def debug(s: String): Unit
  }


  implicit val defaultPatience =
    PatienceConfig(timeout =  Span(300, Seconds), interval = Span(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scalaz.std.scalaFuture._
  import scalaz.std.option._
  import scalaz.syntax.monad._
  import scalaz.EitherT

  val env = Call.BaseEnv(Call.Tags.Host("localhost"), Call.Tags.Environment.Test, Call.Tags.Version("1.0"))

  def Logged[F[_]: scalaz.Functor, A](tags: Call.BaseTags)(f: Log => F[A]): Precepte[Call.BaseEnv, Call.BaseTags, (Call.Span, Call.Path[Call.BaseTags]) => Log, F, A] =
    Precepte(tags) { (state: Call.State[Call.BaseEnv, Call.BaseTags, (Call.Span, Call.Path[Call.BaseTags]) => Log]) =>
      f(state.value(state.span, state.path))
    }

  private def tags(n: String) = Call.BaseTags(Tags.Callee(n), Tags.Category.Database)

  case class Board(pin: Option[Int])
  object BoardComp {
    def get() = Logged(tags("BoardComp.get")) { (logger: Log) =>
      logger.debug("BoardComp.get")
      Board(Option(1)).point[Future]
    }
  }

  case class Community(name: String)
  case class Card(name: String)

  object CardComp {
    def getPin(id: Int) = Logged(tags("BoardComp.getPin")) { (logger: Log) =>
      logger.debug("CardComp.getPin")
      Option(1 -> Card("card 1")).point[Future]
    }

    def countAll() = Logged(tags("CardComp.countAll")) { (logger: Log) =>
      logger.debug("CardComp.countAll")
      Set("Edito", "Video").point[Future]
    }

    def rank() = Logged(tags("CardComp.rank")) { (logger: Log) =>
      logger.debug("CardComp.rank")
      List(1 -> Card("foo"), 1 -> Card("bar")).point[Future]
    }

    def cardsInfos(cs: List[(Int, Card)], pin: Option[Int]) = Logged(tags("CardComp.cardsInfos")) { (logger: Log) =>
      logger.debug("CardComp.cardsInfos")
      List(
        Card("foo") -> List(Community("community 1"), Community("community 2")),
        Card("bar") -> List(Community("community 2"))).point[Future]
    }
  }

  import java.net.URL
  case class Highlight(title: String, cover: URL)
  object HighlightComp {
    def get() = Logged(tags("HighlightComp.get")) { (logger: Log) =>
      logger.debug("HighlightComp.get")
      Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")).point[Future]
    }
  }

  def p[C, G <: Call.Graph[Call.BaseTags, C, G]](g: G, before: String = ""): Unit = {
    val txt = g match {
      case Call.Root(span, _) =>
        s"Root[$span]"
      case Call.GraphNode(id, _, tags, _) =>
        s"GraphNode[$id]: $tags"
    }

    println(before + txt)

    for (c <- g.children) {
      c match {
        case node@Call.GraphNode(_, _, _, _) =>
          p[C, Call.GraphNode[Call.BaseTags, C]](node, before + "  ")
      }
    }
  }

  def toStates[C](g: Call.Root[Call.BaseTags, C]): Seq[Call.State[Call.BaseEnv, Call.BaseTags, C]] = {
    def go(g: Call.GraphNode[Call.BaseTags, C], span: Call.Span, path: Call.Path[Call.BaseTags], states: Seq[Call.State[Call.BaseEnv, Call.BaseTags, C]]): Seq[Call.State[Call.BaseEnv, Call.BaseTags, C]] = {
      val Call.GraphNode(id, value, tags, cs) = g
      val p = path :+ Call(id, tags)
      val st = Call.State[Call.BaseEnv, Call.BaseTags, C](span, env, p, value)
      val cst = cs.map{ c =>
        go(c, span, p, Seq.empty)
      }.flatten
      (states :+ st) ++ cst
    }

    g.children.map { c =>
      go(c, g.span, Vector.empty, Seq.empty)
    }.flatten
  }


  def nostate = Call.State[Call.BaseEnv, Call.BaseTags, Unit](Call.Span.gen, env, Vector.empty, ())
  import Call.Tags
  import Tags.Callee

  "Precepte" should "trivial" in {

    def f1 = Precepte(tags("trivial.f1")).apply0{ (_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => 1 }
    def f2(i: Int) = Precepte(tags("trivial.f2")).apply0{(_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => s"foo $i"}
    def f3(i: Int) = Precepte(tags("trivial.f3")).apply0{(_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => i + 1}

    val (graph0, result0) = f1.run(nostate)
    result0 should ===(1)

    println("-- graph0 --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph0)
    println("----")

    val (graphm, _) = Precepte(tags("graphm0"))(Precepte(tags("graphm"))(f1)).run(nostate)
    println("-- graphm --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graphm)
    println("----")

    val res =
      for {
        i <- f1
        r <- f2(i)
      } yield r

    val (graph, result) = res.run(nostate)
    result should ===("foo 1")

    println("-- graph --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph)
    println("----")

    val (graph1, result1) = Precepte(tags("trivial.anon"))(res).run(nostate)
    println("-- graph1 --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph1)
    println("----")

    val res2 =
      for {
        i <- Precepte(tags("trivial.anon2"))(f1)
        r <- f2(i)
      } yield r

    val (graph2, result2) = res2.run(nostate, (1 to 30).map(i => Call.Id(i.toString)).toStream)
    println("-- graph2 --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph2)
    println("----")
  }

  it should "simple" in {
    def f1 = Precepte(tags("simple.f1")){(_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => 1.point[Future]}
    def f2(i: Int) = Precepte(tags("simple.f2")){(_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => s"foo $i".point[Future]}

    val res = for {
      i <- f1
      r <- f2(i)
    } yield r

    res.eval(nostate).futureValue should ===("foo 1")
  }

  it should "optT" in {
    val f1 = Precepte(tags("opt"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => Option("foo").point[Future])
    val f2 = Precepte(tags("opt"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => Option(1).point[Future])
    val f3 = Precepte(tags("opt"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => (None: Option[Int]).point[Future])


    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(Some(("foo",1)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(None)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.run.eval(nostate).futureValue should ===(None)
  }

  it should "listT" in {
    val f1 = Precepte(tags("listT"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => List("foo", "bar").point[Future])
    val f2 = Precepte(tags("listT"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => List(1, 2).point[Future])
    val f3 = Precepte(tags("listT"))((_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => List[Int]().point[Future])

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(List(("foo",1), ("foo",2), ("bar",1), ("bar",2)))

    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(List())

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    res3.run.eval(nostate).futureValue should ===(List())
  }

  it should "EitherT" in {
    import scalaz.{ \/ , \/-, -\/}
    import EitherT.eitherTFunctor

    val f1: Precepte[Call.BaseEnv, Call.BaseTags, Unit, Future, String \/ String] =
      Precepte(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: Precepte[Call.BaseEnv, Call.BaseTags, Unit, Future, String \/ Int] =
      Precepte(tags("f2"))(_ => \/-(1).point[Future])
    val f3: Precepte[Call.BaseEnv, Call.BaseTags, Unit, Future, String \/ String] =
      Precepte(tags("f3"))(_ => -\/("Error").point[Future])

    type Foo[A] = EitherT[Future, String, A]

    val res = for {
      e1 <- trans(f1)
      e2 <- trans(f2)
    } yield (e1, e2)

    res.run.eval(nostate).futureValue should ===(\/-("foo" -> 1))

    val error = -\/("Error")
    val res2 = for {
      e1 <- trans(f1)
      e2 <- trans(f3)
    } yield (e1, e2)

    res2.run.eval(nostate).futureValue should ===(error)

    val res3 = for {
      e1 <- trans(f3)
      e2 <- trans(f2)
    } yield (e1, e2)

    val (graph, rr) = res3.run.run(nostate).futureValue
    rr should ===(error)
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph)
  }

  it should "pass context" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Call.BaseEnv, Call.BaseTags, Unit]]()

    def push(state: Call.State[Call.BaseEnv, Call.BaseTags, Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      1.point[Future]
    }

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)
    ctxs.length should ===(1)

    ctxs.toList should ===(toStates(graph).toList)
  }

  it should "preserve context on map" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Call.BaseEnv, Call.BaseTags, Unit]]()

    def push(state: Call.State[Call.BaseEnv, Call.BaseTags, Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      1.point[Future]
    }.map(identity).map(identity).map(identity).map(identity)

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)

    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)

    ctxs.toList should ===(toStates(graph).toList)
  }

  it should "preserve context on flatMap" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Call.BaseEnv, Call.BaseTags, Unit]]()

    def push(state: Call.State[Call.BaseEnv, Call.BaseTags, Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    def f3(s: String) = Precepte(tags("f3")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      s"f3 $s".point[Future]
    }

    val f = Precepte(tags("anon0"))(f1
      .flatMap(i => f2(i))
      .flatMap(s => f3(s)))

    val (graph, res) = f.run(nostate).futureValue
    res should ===("f3 foo 1")

    ctxs.length should ===(3)
    ctxs.toList should ===(toStates(graph).toList.drop(1))
  }

  it should "stack contexts" in {
    def f1 = Precepte(tags("f1")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      1.point[Future]
    }

    val stacked = Precepte(tags("stacked"))(f1)
    val (graph, r) = stacked.run(nostate).futureValue
    r should ===(1)

    graph.children should have length 1
    graph.children.head.children should have length 1
  }

  it should "provide context to C" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Call.BaseEnv, Call.BaseTags, Unit]]()

    def push(state: Call.State[Call.BaseEnv, Call.BaseTags, Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      1.point[Future]
    }

    def f2(i: Int) = Precepte(tags("f2")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      s"foo $i".point[Future]
    }

    val (graph, res) = f1.run(nostate).futureValue
    res should ===(1)
    ctxs.length should ===(1)
    ctxs.head.path.length should ===(1)


    ctxs.clear()

    val res2 = f1.map(identity)
    res2.eval(nostate)

    ctxs should have length(1)
    forAll(ctxs.map(_.path.length == 1)){_  should ===(true) }


    ctxs.clear()

    val r = for {
      i <- f1
      r <- f2(i)
    } yield r

    r.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 1)){ _ should ===(true) }

    ctxs.clear()

    val res3 = Precepte(tags("res3"))(f1)
    res3.eval(nostate).futureValue should ===(1)

    ctxs should have length(1)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

    ctxs.clear()

    val res4 = Precepte(tags("res4")) {
      for {
        i <- f1
        r <- f2(i)
      } yield r
    }

    res4.eval(nostate).futureValue should ===("foo 1")

    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }
  }

  it should "not stack context on trans" in {
    val ctxs = scala.collection.mutable.ArrayBuffer[Call.State[Call.BaseEnv, Call.BaseTags, Unit]]()

    def push(state: Call.State[Call.BaseEnv, Call.BaseTags, Unit]): Unit = {
      ctxs += state
      ()
    }

    def f1 = Precepte(tags("f1")) { (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      Option(1).point[Future]
    }

    def f2(i: Int) = Precepte(tags("f1")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
      push(c)
      Option(s"foo $i").point[Future]
    }

    val res4 = Precepte(tags("res4")) {
      (for {
        i <- trans(f1)
        r <- trans(f2(i))
      } yield r).run
    }

    res4.eval(nostate).futureValue should ===(Some("foo 1"))

    ctxs should have length(2)
    ctxs.map(_.span).toSet.size should ===(1) // span is unique
    forAll(ctxs.map(_.path.length == 2)){ _ should ===(true) }

  }

  it should "real world wb.fr home" in {

    val logs = scala.collection.mutable.ArrayBuffer[String]()

    case class Logger(span: Call.Span, path: Call.Path[Call.BaseTags]) extends Log {
      def debug(s: String): Unit = {
        logs += s"[DEBUG] ${span.value} -> /${path.mkString("/")} $s"
        ()
      }
    }

    val getPin =
      (for {
        b   <- trans(BoardComp.get().lift[Option])
        id  <- trans(Precepte(tags("point"))((_: Call.State[Call.BaseEnv, Call.BaseTags, (Call.Span, Call.Path[Call.BaseTags]) => Log]) => b.pin.point[Future]))
        pin <- trans(CardComp.getPin(id))
      } yield pin).run


    val res = for {
      pin            <- getPin
      cs             <- CardComp.rank()
      cards          <- CardComp.cardsInfos(cs, pin.map(_._1))
      availableTypes <- CardComp.countAll()
      h              <- HighlightComp.get()
    } yield (pin, cs, cards, availableTypes, h)

    def logger(span: Call.Span, path: Call.Path[Call.BaseTags]): Log =
      Logger(span, path)

    val initialState = Call.State[Call.BaseEnv, Call.BaseTags, (Call.Span, Call.Path[Call.BaseTags]) => Log](Call.Span.gen, env, Vector.empty, logger _)
    res.eval(initialState).futureValue should ===(
      (Some((1, Card("card 1"))),
        List((1, Card("foo")), (1, Card("bar"))),
        List(
          (Card("foo"), List(
            Community("community 1"),
            Community("community 2"))),
          (Card("bar"),List(
            Community("community 2")))),
        Set("Edito", "Video"),
        Highlight("demo", new URL("http://nd04.jxs.cz/641/090/34f0421346_74727174_o2.png")))
    )

    for(l <- logs)
    println(l)
  }

  it should "implement mapK" in {

    type Pre[A] = Precepte[Call.BaseEnv, Call.BaseTags, Unit, Future, A]

    def f1: Pre[Int] =
      Precepte(tags("f1")) { (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        1.point[Future]
      }

    def f2: Pre[Int] =
      Precepte(tags("f2")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        Future { throw new RuntimeException("ooopps f2") }
      }

    def f3(i: Int): Pre[String] =
      Precepte(tags("f3")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        "foo".point[Future]
      }

    def f4(i: Int): Pre[String] =
      Precepte(tags("f4")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        Future { throw new RuntimeException("ooopps f4") }
      }

    (for {
      i <- f2
      r <- f3(i)
    } yield r)
      .flatMapK(_.map(_.point[Pre]).recover { case _ => "recovered".point[Pre] })
      .eval(nostate).futureValue should ===("recovered")

    (for {
      i <- f1
      r <- f4(i)
    } yield r)
      .flatMapK(_.map(_.point[Pre]).recover { case _ => "recovered".point[Pre] })
      .eval(nostate).futureValue should ===("recovered")

  }

  /*
  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.OptionT._
    import Call._

    // This problem seems to be caused by the classic bug on dependent types on the Scala compiler.
    // Since the conversion MonadPlusOpsUnapply uses Unapply, and withFilter does not have an explicit return type,
    // the compiler infer something like res90.M[res90.A] for withFilter return type
    // From there it's not capable to find instances of typeclasses for res90.M
    //
    // see the following REPL session:
    // scala> type Pre[A] = Precepte[BaseEnv, BaseTags, Unit, Future, A]
    // scala> scalaz.OptionT.optionTMonadPlus[Pre]
    // res88: scalaz.MonadPlus[[α]scalaz.OptionT[Pre,α]] = scalaz.OptionTInstances0$$anon$2@6f6029a9
    // scala> scalaz.Unapply.unapplyMFA[scalaz.MonadPlus, scalaz.OptionT, Pre, Int](res88)
    // res90: scalaz.Unapply[scalaz.MonadPlus,scalaz.OptionT[Pre,Int]]{type M[X] = scalaz.OptionT[Pre,X]; type A = Int} = scalaz.Unapply_0$$anon$11@7020e2a1
    // scala> scalaz.syntax.monadPlus.ToMonadPlusOpsUnapply(optionT(f1))(res90)
    // res93: scalaz.syntax.MonadPlusOps[res90.M,res90.A] = scalaz.syntax.MonadPlusOps@628d6048
    // scala> res93.withFilter _
    // res94: (res90.A => Boolean) => res90.M[res90.A] = <function1>

    type Pre[A] = Precepte[BaseEnv, BaseTags, Unit, Future, A]
    val f1 = Option(1).point[Pre]
    optionT(f1).withFilter(_ => true).withFilter(_ => true) should ===(f1)
  }
  */

}
