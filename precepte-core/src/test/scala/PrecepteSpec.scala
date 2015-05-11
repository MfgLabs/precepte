package com.mfglabs.monitoring

import org.scalatest._
import Matchers._
import Inspectors._

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span}

import scala.language.higherKinds



class PrecepteSpec extends FlatSpec with ScalaFutures {

  import Call.Tags

  implicit val defaultPatience =
    PatienceConfig(timeout =  Span(300, Seconds), interval = Span(5, Millis))

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scalaz.std.scalaFuture._
  // import scalaz.std.option._
  import scalaz.syntax.monad._
  import scalaz.EitherT

  val taggingContext = new TaggingContext[Call.BaseEnv, Call.BaseTags, Unit, Future]
  import taggingContext._
  import Precepte._

  val env = Call.BaseEnv(Call.Tags.Host("localhost"), Call.Tags.Environment.Test, Call.Tags.Version("1.0"))

  private def tags(n: String) = Call.BaseTags(Tags.Callee(n), Tags.Category.Database)

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

/*
  "Precepte" should "trivial" in {

    def f1 = Precepte(tags("trivial.f1")){ (_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => 1.point[Future] }
    def f2(i: Int) = Precepte(tags("trivial.f2")){ (_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => s"foo $i".point[Future] }
    def f3(i: Int) = Precepte(tags("trivial.f3")){ (_: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) => (i + 1).point[Future] }

    val (graph0, result0) = f1.run(nostate).futureValue
    result0 should ===(1)

    println("-- graph0 --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph0)
    println("----")

    val (graphm, _) = Precepte(tags("graphm0"))(Precepte(tags("graphm"))(f1)).run(nostate).futureValue
    println("-- graphm --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graphm)
    println("----")

    val res =
      for {
        i <- f1
        r <- f2(i)
      } yield r

    val (graph, result) = res.run(nostate).futureValue
    result should ===("foo 1")

    println("-- graph --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph)
    println("----")

    val (graph1, result1) = Precepte(tags("trivial.anon"))(res).run(nostate).futureValue
    println("-- graph1 --")
    p[Unit, Call.Root[Call.BaseTags, Unit]](graph1)
    println("----")

    val res2 =
      for {
        i <- Precepte(tags("trivial.anon2"))(f1)
        r <- f2(i)
      } yield r

    val (graph2, result2) = res2.run(nostate, (1 to 30).map(i => Call.Id(i.toString)).toStream).futureValue
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

    val f1: Precepte[String \/ String] =
      Precepte(tags("f1"))(_ => \/-("foo").point[Future])
    val f2: Precepte[String \/ Int] =
      Precepte(tags("f2"))(_ => \/-(1).point[Future])
    val f3: Precepte[String \/ String] =
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

    type ST = (Call.Span, Call.Path[Call.BaseTags]) => Log

    val taggingContext = new TaggingContext[Call.BaseEnv, Call.BaseTags, ST, Future]
    import taggingContext._
    import Precepte._
    import scalaz.std.option._

    trait Log {
      def debug(s: String): Unit
    }

    def Logged[A](tags: Call.BaseTags)(f: Log => Future[A]): Precepte[A] =
      Precepte(tags) { (state: Call.State[Call.BaseEnv, Call.BaseTags, ST]) =>
        f(state.value(state.span, state.path))
      }

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
        id  <- trans(Precepte(tags("point"))((_: Call.State[Call.BaseEnv, Call.BaseTags, ST]) => b.pin.point[Future]))
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

    val initialState = Call.State[Call.BaseEnv, Call.BaseTags, ST](Call.Span.gen, env, Vector.empty, logger _)
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

    def f1: Precepte[Int] =
      Precepte(tags("f1")) { (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        1.point[Future]
      }

    def f2: Precepte[Int] =
      Precepte(tags("f2")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        Future { throw new RuntimeException("ooopps f2") }
      }

    def f3(i: Int): Precepte[String] =
      Precepte(tags("f3")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        "foo".point[Future]
      }

    def f4(i: Int): Precepte[String] =
      Precepte(tags("f4")){ (c: Call.State[Call.BaseEnv, Call.BaseTags, Unit]) =>
        Future { throw new RuntimeException("ooopps f4") }
      }

    (for {
      i <- f2
      r <- f3(i)
    } yield r)
      .flatMapK(_.map(_.point[Precepte]).recover { case _ => "recovered".point[Precepte] })
      .eval(nostate).futureValue should ===("recovered")

    (for {
      i <- f1
      r <- f4(i)
    } yield r)
      .flatMapK(_.map(_.point[Precepte]).recover { case _ => "recovered".point[Precepte] })
      .eval(nostate).futureValue should ===("recovered")

  }

  it should "not break type inference" in {
    import scalaz.syntax.monadPlus._
    import scalaz.OptionT._
    val f1 = Option(1).point[Precepte]
    optionT(f1).withFilter(_ => true).withFilter(_ => true).run.eval(nostate).futureValue should ===(Some(1))
  }
*/

  it should "do" in {
    import scalaz.{Monad, Bind, MonadPlus, Leibniz, Foldable, Bifoldable, OptionT, OptionTMonadPlus}

    // import scalaz.syntax.monadPlus._
    // import scalaz.std.option._
    // import scalaz.OptionT._
    // import scalaz.Id._
    // import scalaz.std.list._

    // import shapeless._


    import scala.language.implicitConversions
    // import scalaz.syntax.monadPlus._

/*
    trait Generic1[F[_], FR[_[_]]] {
      type R[t]

      implicit lazy val fr: FR[R] = mkFrr

      lazy val ff: FR[F] = mkFrf

      // def to[T](ft: F[T]): R[T]
      // def lift[T](rt: R[T]): F[T]

      def mkFrr: FR[R]

      def mkFrf: FR[F]
    }

    object Generic1 {
      type Aux[F[_], FR[_[_]], R0[_]] = Generic1[F, FR] { type R[t] = R0[t] }

      def apply[F[_], FR[_[_]]](implicit gen: Generic1[F, FR]): Aux[F, FR, gen.R] = gen

      // implicit def materialize[T[_], FR[_[_]]]: Generic1[T, FR] = macro Generic1Macros.materialize[T, FR]
    }

    implicit def optionTG1[R0[_]](implicit lmr0: Lazy[Monad[R0]]) = new Generic1[({type λ[A] = OptionT[R0, A]})#λ, Monad] {
      type R[t] = R0[t]

      def mkFrr = lazily[Monad[R0]]

      // def to[T](ft: OptionT[R0, T]): R0[T] = ft.runn
      // def lift[T](rt: R[T]): OptionT[R, T] = optionT(fr.map(rt)(Some(_)))

      def mkFrf = lazily[Monad[({type λ[A] = OptionT[R0, A]})#λ]]
    }

    implicit def generic[F[_]](implicit gen: Generic1[F, Monad]): Monad[F] = gen.ff
*/

      // new Monad[F] {
      //   override def point[A](a: => A): F[A] = gen.ff.point(a)

      //   override def map[A, B](m: F[A])(f: A => B): F[B] = gen.ff.map(m)(f)

      //   override def bind[A, B](m: F[A])(f: A => F[B]): F[B] = gen.ff.bind(m)(f)
      // }

    // final class MonadPlusOps[F[_],A0](val self: F[A0])(implicit val F: MonadPlus[F]) extends scalaz.syntax.Ops[F[A0]] {
    //   ////
    //   import Leibniz.===

    //   def filter(f: A0 => Boolean) =
    //     F.filter(self)(f)
      
    //   def withFilter(f: A0 => Boolean) =
    //     filter(f)

    //   final def uniteU[T](implicit T: Unapply[Foldable, A0]): F[T.A] =
    //     F.uniteU(self)(new scalaz.Unapply[Foldable, A0]{
    //       type M[x] = T.M[x]
    //       type A = T.A
    //       def TC = T.TC

    //       def leibniz = scalaz.Leibniz.force[Nothing, Any, A0, T.M[A]]
    //     })

    //   def unite[T[_], B](implicit ev: A0 === T[B], T: Foldable[T]): F[B] = {
    //     val ftb: F[T[B]] = ev.subst(self)
    //     F.unite[T, B](ftb)
    //   }

    //   final def separate[G[_, _], B, C](implicit ev: A0 === G[B, C], G: Bifoldable[G]): (F[B], F[C]) =
    //     F.separate(ev.subst(self))

    //   ////
    // }



    // implicit def ToMonadPlusOpsUnapply[FA](v: FA)(implicit F0: com.mfglabs.monitoring.Unapply[MonadPlus, FA]) =
    //   new MonadPlusOps[F0.M,F0.A](F0.subst(v))(F0.TC)

    // implicit def ToMonadPlusOps[F[_],A](v: F[A])(implicit F0: MonadPlus[F]) =
    //   new MonadPlusOps[F,A](v)

    type P2[A] = P[Int, A]
    // val a = Option(1).point[P2]
    // val b: OptionT[({type λ[A] = P[Future, A]})#λ, Int] = optionT(a).withFilter(_ => true)
    // b.withFilter(_ => true)

    // type PP2[A] = PP.Aux[Future, A]
    // val a = Option(1).point[P2]
    // val b: OptionT[({type λ[A] = P[Int, A]})#λ, Int] =  optionT(a).withFilter(_ => true)
    // b.withFilter(_ => true)
    // implicitly[Monad[Future]]
    // implicitly[MonadPlus[P2]]
    // implicitly[Unapply[MonadPlus, OptionT[P2, Int]]]
    // implicitly[Unapply[MonadPlus, OptionT[({type λ[A] = P[Int, A]})#λ, Int]]]
    // implicitly[Monad[List]]
    // Unapply.Aux2MT[Monad, OptionT[List, Int], OptionT, List, Int]
    // implicitly[Monad[P2]]
    // implicitly[Monad[({type λ[A] = P[Int, A]})#λ]]
    // implicitly[MonadPlus[Id]]
    // implicitly[Unapply[Monad, OptionT[scalaz.Id.Id, Int]]]

    // implicitly[Monad[({type λ[A] = OptionT[Future, A]})#λ]]


// implicit val a = {
//   import shapeless.lazily

//   final class fresh$macro$2 extends _root_.utils.$u2206$u2206[Option, scalaz.Monad] {
//     type R[fresh$macro$1] = Option[fresh$macro$1];
//     def mkFrr: scalaz.Monad[R] = lazily[scalaz.Monad[R]]
//   };
//   new fresh$macro$2()
// }
      
    // ∆∆[Future, Monad]

    // import utils.∆∆

    // ∆∆[({type λ[A] = P[Int, A]})#λ, Monad]

    // implicitly[Monad[({type λ[A] = P[Int, A]})#λ]]
    // ∆∆[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ, Monad]
    // val dd = ∆∆.materialize2[OptionT, Monad, ({type λ[A] = P[Int, A]})#λ]
    // implicit val r = ∆∆∆(dd)
    // ∆∆∆[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]
    // implicitly[Monad[({type λ[A] = OptionT[Future, A]})#λ]]
    // trait SaveUrAss {

      // import utils.∆∆
      // import scalaz.Monad
      // implicit def ∆∆∆[F[_]](implicit gen: ∆∆[F, Monad]): Monad[F] = gen.fr.asInstanceOf[Monad[F]]
      // implicit def ∆∆∆[T[_[_], _], F[_, _], I](
      //   implicit gen: ∆∆[({type λ[A] = T[({type λ[A] = F[I, A]})#λ, A]})#λ, Monad]
      // ): Monad[({type λ[A] = T[({type λ[A] = F[I, A]})#λ, A]})#λ] = gen.fr.asInstanceOf[Monad[({type λ[A] = T[({type λ[A] = F[I, A]})#λ, A]})#λ]]

      // implicit def ∆∆∆∆[T[_[_], _], F[_, _, _], I1, I2](
      //   implicit gen: ∆∆[({type λ[A] = T[({type λ[A] = F[I1, I2, A]})#λ, A]})#λ, Monad]
      // ): Monad[({type λ[A] = T[({type λ[A] = F[I1, I2, A]})#λ, A]})#λ] = gen.fr.asInstanceOf[Monad[({type λ[A] = T[({type λ[A] = F[I1, I2, A]})#λ, A]})#λ]]
    // }

    import scalaz.OptionT
    import utils.{UnLambda1, GenericMat}

    // implicit def Unl[F[_], R[_]](implicit gen: UnLambda1[F]): Monad[F] = shapeless.lazily[Monad[R]].asInstanceOf[Monad[F]]

    object GenericMat2 {
      def apply[F[_], FR[_[_]]](implicit unl: UnLambda1[F]) = {
        type R[t] = unl.R[t]
        GenericMat.materialize[R, FR]
        // unl
      }

      // def materialize[F[_], FR[_[_]]]: GenericMat[F, FR] = 
      //   macro GenericMatMacros.materialize[F, FR]
    }

    UnLambda1[({type λ[A] = P[Int, A]})#λ]
    UnLambda1[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]
    
    val a : Int = GenericMat2[({type λ[A] = P[Int, A]})#λ, Monad]

    // implicitly[Monad[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]]
    // object Toto extends SaveUrAss {

      // import utils.∆∆
      // ∆∆[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ, Monad]
      // ∆∆∆[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]
      // ∆∆∆[OptionT, ({type λ[A] = P[Int, A]})#λ]
      // val m = implicitly[Monad[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]]
      // println("RES:"+m.point(5))
    // }

    // ∆∆.materialize1[P, Monad, Int]
    // implicit val d = optionTG1[({type λ[A] = P[Int, A]})#λ]
    // implicitly[Generic1[({type λ[A] = OptionT[P2, A]})#λ, Monad]]
    // implicitly[Monad[({type λ[A] = OptionT[Future, A]})#λ]]


    // implicitly[Unapply[Monad, ({type λ[A] = P[Int, A]})#λ[Int]]]
    // Unapply.unapply2Aux21MT[Monad, OptionT, P, Int, Int]
    // implicitly[Monad[P2]]
    // implicitly[Monad[({type λ[A] = OptionT[P2, A]})#λ]]
    // implicitly[Monad[({type λ[A] = OptionT[({type λ[A] = P[Int, A]})#λ, A]})#λ]]
    // implicitly[Unapply[Monad, OptionT[({type λ[A] = P[Int, A]})#λ, Int]]]
    0 should ===(0)
  }
}



    // trait PP[A] { self =>
    //   type F[_]

    //   def map[B](f: A => B): PP[B] = new PP[B] { type F[x] = self.F[x] }
    //   def flatMap[B](f: A => PP[B]): PP[B] = new PP[B] { type F[x] = self.F[x] }
    // }

    // object PP {
    //   type Aux[F0[_], A] = PP[A] { type F[x] = F0[x] }

    //   implicit def pInstances[F0[_]](implicit B: Bind[F0]): Monad[({type λ[A] = PP.Aux[F0, A]})#λ] =
    //     new Monad[({type λ[A] = PP.Aux[F0, A]})#λ] {
    //       override def point[A](a: => A): PP.Aux[F0, A] = new PP[A] { type F[x] = F0[x] }

    //       override def map[A, B](m: PP.Aux[F0, A])(f: A => B): PP.Aux[F0, B] = m.map(f).asInstanceOf[PP.Aux[F0, B]]
    //       override def bind[A, B](m: PP.Aux[F0, A])(f: A => PP.Aux[F0, B]): PP.Aux[F0, B] = m.flatMap(f).asInstanceOf[PP.Aux[F0, B]]
    //     }

    // }

    // abstract class Fix[F0[_]](implicit bindF: Bind[F0]) {

    //   type P[A] = PP[A] { type F[x] = F0[x] }

    //   implicit def pInstances: Monad[P] = PP.pInstances[F0]
    // }
