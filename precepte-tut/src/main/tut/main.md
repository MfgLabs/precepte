# Précepte core

The core module contains all the basic features.

## Prerequisites

Using Précepte is not different from using a `Future` or any other monadic data type.
You should understand the concepts of `Monad`, `Functor` and `Applicative`, and be familiar with Scalaz.

## From Future to Précepte

A Précepte is always parameterized by a type `F` representing the effect.
most of this documentation is using `Future`, since it's familiar to most Scala developers.

Let's say you've written the following code:

```tut:invisible
import scala.language._
```

```tut:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def f1: Future[Int] = Future.successful(42)
def f2(s: Int): Future[String] = Future.successful(s"The answer to life the universe and everything is $s")
```

If you were to "chain" the calls to f1 and f2, that is call f1 and feed it's return to f2, you'd be writing something like:

```tut:silent
val ultimateAnswer: Future[String] =
  for {
    s <- f1
    i <- f2(s)
  } yield i
```

Let's define a simple await function to test our code:

```tut:silent
import scala.concurrent.Await
import scala.concurrent.duration._
def await[A](f: Future[A]) = Await.result(f, 10 seconds)
```

Now we can test it:

```tut
await(ultimateAnswer)
```


All is well, but you'd really like to be able to collect insights on your code, like how fast it is. Of course you choose to use Précepte for that!

Let's rewrite you code to use Précepte, and attach some metadata to it.

```tut:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.mfglabs.precepte._
import default._

type Pre[A] = DPre[Future, Unit, A]
object Demo extends Category("demo") {
  def apply[A](f: ST[Unit] => Future[A])(implicit c: Callee) =
    Pre(BaseTags(c, Demo))(f)
}

import Macros.callee

def f1: Pre[Int] =
  Demo { st =>
    Future.successful(42)
  }

def f2(s: Int): Pre[String] =
  Demo { st =>
    Future.successful(s"The answer to life the universe and everything is $s")
  }
```

What we've done so far is not particularly exiting. We wrapped everything in a Précepte, and added metadata: the name of the current method, and their "categories". A category is just a group or layer of things in your code. It could be "Database" for all the methods accessing a Database, or "WS" for the methods calling a webservice, or anything you like.

One interesting point is that, at call site, apart from the return type, everything stays identical to our old `Future` version (pun totally intended).

```tut:silent
val ultimateAnswerPre: Pre[String] =
  for {
    s <- f1
    i <- f2(s)
  } yield i
```

### The state

You probably have noticed a little `st` parameter sneaked into our code. This `st` is the current state. By default, Précepte will add the interesting metadata it collected in that state (Category, function name, ...). You can also use Précepte to carry a "custom" state.
The type of the custom state is fixed in our type definition for Pre. We said it was Unit.

Now if we want to run our code, we need to provide an initial state. Précepte will also ask you to provide information about the environment in which our application is executing.

```tut:silent
val env = BaseEnv(Host("localhost"), Environment.Dev, Version("1.0-DEMO"))
val nostate = ST(Span.gen, env, Vector.empty, ())

implicit val unitSG = new scalaz.Semigroup[Unit] {
  def append(f1: Unit, f2: => Unit) = ()
}

import scalaz.std.scalaFuture._
```

// TODO: explain semigroup

Ok so we've added a bit of code, and finally, we're able to test the execution:

```tut
val eventuallyUltimateAnswer = ultimateAnswerPre.eval(nostate)
await(eventuallyUltimateAnswer)
```

// TODO: Précepte and the State Monad

### Error handling

TODO

## Contextualized logs

Now that you have access to metadata about the executing code, you can do pretty interesting things, like hava contextualized logs.
Précepte has a module to use Logback. all you have to to is add it into your `build.sbt` file.

```scala
libraryDependencies += "com.mfglabs" %% "precepte-logback" % precepteVersion
```

Once you have that, you can start using you contextualized Logger :)

```tut:silent
val logback = Logback(env)

object WithLogger extends Category("demo") {
  def apply[A](f: logback.Logger => Future[A])(implicit c: Callee) = {
    val withLogger = (st: ST[Unit]) => f(logback.Logger(st.managed.span, st.managed.path))
    Pre(BaseTags(c, Demo))(withLogger)
  }
}

import Macros.callee

def f1: Pre[Int] =
  WithLogger { logger =>
    logger.info("Computing a value")
    Future.successful(42)
  }

def f2(s: Int): Pre[String] =
  WithLogger { logger =>
    println("prout")
    logger.info("Performing string concatenation!", Macros.param(s))
    Future.successful(s"The answer to life the universe and everything is $s")
  }

val ultimateAnswerPre: Pre[String] =
  for {
    s <- f1
    i <- f2(s)
  } yield i
```

And now when we run the code:

```tut:silent
val ultimateAnswer = ultimateAnswerPre.eval(nostate)
await(ultimateAnswer)
```

Assuming logback is configured properly (see [the example logback.xml](../precepte-tut/src/main/resources/logback.xml)), the following should appear in your console:

```json
{
  "@timestamp": "2015-11-13T16:29:47.656+01:00",
  "@version": 1,
  "message": "Computing a value",
  "logger_name": "application",
  "thread_name": "run-main-11",
  "level": "INFO",
  "level_value": 20000,
  "HOSTNAME": "Juliens-MacBook-Pro.local",
  "span": "c8e9b61b-cf7c-4272-85da-7c320effb037",
  "callees": "/f1",
  "path": "/3_0",
  "environment": "dev",
  "parameters": {}
}
{
  "@timestamp": "2015-11-13T16:29:47.717+01:00",
  "@version": 1,
  "message": "Performing string concatenation!",
  "logger_name": "application",
  "thread_name": "ForkJoinPool-1-worker-13",
  "level": "INFO",
  "level_value": 20000,
  "HOSTNAME": "Juliens-MacBook-Pro.local",
  "span": "c8e9b61b-cf7c-4272-85da-7c320effb037",
  "callees": "/f1/f2",
  "path": "/3_0/4_0",
  "environment": "dev",
  "parameters": {
    "s": "42"
  }
}
```

## Graph it!

## Monitoring with InfluxDB and Grafana

## Précepte and the Free monad.
