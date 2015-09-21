# Precepte, let your code be state-aware & make runtime effect observation acceptable...

Precepte is an opinionated purely functional & lazy API stacking some useful monads to help you observe the execution of your runtime effects by instrumenting your code with a managed state propagated all along your business workflow.

Precepte embraces the concept that observing has a cost but let you control explicitly the balance between precision and performance without
sacrifying code cleanness and FP purity & laziness.

Precepte's idea is simply to make your code _state-aware_ by allowing you to obtain a State anywhere in your code.
This state will provide you with:
  - a context defining from where this code comes and where it is (function name, custom tags, etc..)
  - any values you want to measure in your observation

The state provided by Precepte is composed of:
  - a managed part that is managed by Precepte and that consists in:
      * the Span: a global ID uniquely identifying the full execution workflow
      * the Call Path: a sequence of tuples of PId (local step ID) and Tags (defined at compile-time)
                       accumulated by Precepte all along the execution of the workflow. This is what
                        provides you with the place from where you come and where you are...

  - an unmanaged part that is just a container in which you can put anything you want and also perform some compile-time DI.

```scala
// A Sample with Future effects (the ugliest of the effects)
// For now, we use scalaz (cats coming soon) so you need some monads from Scalaz
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalaz.std.scalaFuture._
import scalaz.syntax.monad._

// import the default Precepte representation using our provided Tags & ManagedState type
import precepte.default._

// create some effectful steps in which you can
// ST[Int] represents a State with an unmanaged part in which you will inject an Int at execution 
def f1 = Precepte(tags("simple.f1", DB)){(_: ST[Int]) => 1.point[Future]}
def f2(i: Int) = Precepte(tags("simple.f2", API)){(_: ST[Int]) => s"foo $i".point[Future]}

// Lazy definition of your effectful workflow
val res = for {
  i <- f1
  r <- f2(i)
} yield r

// create the initial state
// Span is the global unique ID
// 42 is your injected unmanaged state part (could be something much more interesting ;))
val state0 = ST(Span.gen, env, Vector.empty, 42)

// execute your effectful workflow starting with state0
val (s, a) = res.run(state0)
```

Precepte is a custom purely functional structure based on:
  - A State Monad to represent the pure propagation of the State in the workflow
  - A Free Monad to represent our monadic & applicative effectful workflow
  - Some helpers to make Precepte usable with Monad Transformers
  - Coyoneda to reduce the requirement of effects to be Functor in Free Monads
  - some custom optimization to reduce the burden of Free Monads classic model (right associatio, map fusion, structure squashing, )
