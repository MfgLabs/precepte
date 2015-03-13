# MFG Labs Monitored

## What is it

Monitored is a simple library that helps passing state between the layers of your application. In the process, it also generate a tagged path of the monitored calls.

## Basics

Consider this minimal example:

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.std.scalaFuture._
import com.mfglabs.monitoring._, Monitored._, Call._, Tags._

val f1 = Monitored(Tags(Callee("f1"))){ (_: Call.State[Unit]) =>
	Future.successful("foo")
}
```

The two function f1 and f2 are "Monitored" function. Those function when executed will receive a call state. This call state contains:

- A span. It's a global request identifier
- A path. A path is a succession of call ids from the parent functions.

To be called, a Monitored block needs to be passed an initial `State`.

```scala
val initialState = Monitored.Call.State(Call.Span.gen, Vector.empty, ())
f1.eval(initialState)
```


So far, nothing of interest. Now let's add a little twist:

```scala

def printState[C](st: Call.State[C]) =
	println(s"""${st.span.value}/${st.path.map(_.id.value).mkString("/")}""")

val f1 = Monitored(Tags(Callee("f1"))){ (st: Call.State[Unit]) =>
	printState(st)
	Future.successful("foo")
}

def f2(s: String) = Monitored(Tags(Callee("f1"))){ (st: Call.State[Unit]) =>
	printState(st)
	Future.successful("f2 " + s)
}
```

Note that f1 ans f2 are both returning with `Future`. Suppose that I want to call f1 and then f2.

```scala
val mon: Monitored[Unit, Future, String] =
	for {
		r1 <- f1
		r2 <- f2(r1)
	} yield r2
```

So far so good, I get a new `Monitored`. Note that it automatically took care of the `Future`.

Just like the previous example, I can `eval` it to get the result:

```scala
mon.eval(initialState)
res0: scala.concurrent.Future[String] = scala.concurrent.impl.Promise$DefaultPromise@51fba78d
d973de8a-55fd-432b-bfe8-7a5d91457f8f/DkdoYf9
d973de8a-55fd-432b-bfe8-7a5d91457f8f/z43zso6
```

Note that it printed two calls. both have the same span since they are from the same request.

## Stacking monitored

TODO

## Passing State

TODO

## Transformers

TODO

