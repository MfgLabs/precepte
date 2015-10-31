# Précepte core

The core module contains all the basic features.

## Prerequisites

Using Précepte is not different from using a `Future` or any other monadic data type.
You should understand the concepts of `Monad`, `Functor` and `Applicative`, and have some familiarities with Scalaz.

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


All il well, but you'd really like to be able to collect insights on your code, like how fast it is. Of course you choose to use Précepte for that!

Let's rewrite you code to use Précepte, and attach some metadata to it.

```tut:silent
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.mfglabs.precepte._
import default._

type Pre[A] = DPre[Future, Unit, A]
object Demo extends Category("demo")

def f1: Pre[Int] =
  Pre(BaseTags(Callee("f1"), Demo)) { st =>
    Future.successful(42)
  }
def f2(s: Int): Pre[String] =
  Pre(BaseTags(Callee("f2"), Demo)) { st =>
    Future.successful(s"The answer to life the universe and everything is $s")
  }
```

What we've done so far is not particularly exiting. We wrapped everything in a Précepte, and added metadata: the name of the current method, and their "categories". A category is just a group or layer of things in your code. It could be "Database" for all the methods accessing a Database, or "WS" for the methods calling a webservice, or anything you like.

One interesting point is that, at call site, apart from the return type, everything stays indentical to our old `Future` version (pun totally intended).

```tut:silent
val ultimateAnswerPre: Pre[String] =
  for {
    s <- f1
    i <- f2(s)
  } yield i
```

### The state

You probably have noticed a little `st` parameter sneaked into our code. This `st` is the current state. By default, Précepte will add the interesting metadatas it collected in that state (Category, function name, ...). You can also use Précepte to carry a "custom" state.
The type of the custom state is fixed in our type definition for Pre. We said it was Unit.

Now if we want ti run our code, we need to provide an initial state. Précepte will also ask you to provide information about the environment in which our application is executing.

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

### Executing the code

### Error handling

TODO

## Scrapping some boilerplate with `Macros`, or have a way to save errors

## Contextualized logs

## Graph it!

## Monitoring with InfluxDB and Grafana

## Précepte and the Free monad.
