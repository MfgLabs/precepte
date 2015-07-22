package com.mfglabs
package precepte

import scala.language.higherKinds
import scalaz.{Functor, Applicative, IndexedStateT}
import scalaz.syntax.monad._


object Default {

  case class ManagedState[E <: Env, T <: Tags](env: E, span: Span, path: Call.Path[T], ids: PIdSeries = PIdStream())
    
  type Tags = BaseTags
  type Env = BaseEnv
  type MS = ManagedState[Env, Tags]

  trait PreScope[F[_], C] {

    type MS = ManagedState[Env, Tags]
    type ST[C] = PState[Tags, MS, C]
    type Pre[A] = Precepte[Tags, MS, C, F, A]

    object Pre {
      def apply[Tags](_tags: Tags): PrecepteBuilder[Tags, MS, C, F] = new PrecepteBuilder[Tags, MS, C, F] {
        val tags = _tags
      }

      trait PrecepteBuilder[Tags, M, U, F[_]] {
        val tags: Tags

        type P[M, U, F[_], A] = Precepte[Tags, M, U, F, A]

        def apply[A](λ: (M, U) => F[A])(implicit F: Functor[F]): P[M, U, F, A] =
          Step[Tags, M, U, F, A](
            IndexedStateT { (st: P[M, U, F, A]#S) =>
              for (a <- λ(st.managed, st.unmanaged))
              yield st -> Return(a)
            }, tags)

        def fromState[A](λ: P[M, U, F, A]#S => F[A])(implicit F: Functor[F]): P[M, U, F, A] =
          Step[Tags, M, U, F, A](
            IndexedStateT { (st: P[M, U, F, A]#S) =>
              for (a <- λ(st))
              yield st -> Return(a)
            }, tags)

        def fromStateU[A](λ: P[M, U, F, A]#S => F[(U, A)])(implicit F: Functor[F], upd: PStateUpdater[Tags, M, U]): P[M, U, F, A] =
          Step[Tags, M, U, F, A](
            IndexedStateT { (st: P[M, U, F, A]#S) =>
              for (ca <- λ(st))
              yield {
                val (unmanaged, a) = ca
                upd.updateUnmanaged(st, unmanaged) -> Return(a)
              }
            }, tags)

        def wrap[A](m: P[M, U, F, A])(implicit A: Applicative[F]): P[M, U, F, A] =
          Step(IndexedStateT[F, P[M, U, F, A]#S, P[M, U, F, A]#S, P[M, U, F, A]]{ st =>
            (st -> m).point[F]
          }, tags)
      }

    }

    def tags(n: String): Tags = BaseTags(Tags.Callee(n), Tags.Category.Database)

  }

  object PreScope {
    def apply[F[_], C] = new PreScope[F, C] {}
  }

}
