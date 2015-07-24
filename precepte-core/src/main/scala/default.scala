// package com.mfglabs
// package precepte

// import scala.language.higherKinds


// object default {

//   case class BaseTags(callee: Callee, category: Category) extends Tags  {
//     override def toString = s"($callee, $category)"
//   }

//   case class Callee(override val value: String) extends Tag("callee", value) {
//     override def toString = s"callee($value)"
//   }

//   abstract class Category(value: String) extends Tag("category", value) {
//     override def toString = s"category($value)"
//   }

//   object Category {
//     def unapply(c: Category) = Some(c.value)
//     object Api extends Category("api")
//     object Database extends Category("database")
//   }

//   abstract class Environment(value: String) extends Tag("environment", value)
//   object Environment {
//     object Test extends Environment("test")
//     object Dev extends Environment("dev")
//     object Staging extends Environment("staging")
//     object Production extends Environment("production")
//   }

//   case class Host(override val value: String) extends Tag("host", value)
//   case class Version(override val value: String) extends Tag("version", value)

//   /** The typed environment in which an event happens */
//   trait Env
//   case class BaseEnv(host: Host, environment: Environment, version: Version) extends Env

//   /** A call representing a micro-event in the scope of a macro-event.
//    * A micro-event happens (generally) locally to a system and is not distributed
//    * It is identified by a local Id and enhanced with a few tags
//    */
//   case class Call[T <: Tags](id: PId, tags: T)

//   object Call {
//     type Path[T <: Tags] = Vector[Call[T]]
//   }

//   case class ManagedState[E <: Env, T <: Tags](env: E, span: Span, path: Call.Path[T], ids: PIdSeries = PIdStream())

//   type MS = ManagedState[BaseEnv, BaseTags]
//   type ST[C] = PState[BaseTags, MS, C]

//   object ST {
//     def apply[C](span: Span, env: BaseEnv, path: Call.Path[BaseTags], value: C): ST[C] =
//       PState[BaseTags, MS, C] (
//         ManagedState(env, span, path),
//         value
//       )
//   }

//   implicit def pstateUpdater[C] = new PStateUpdater[BaseTags, MS, C] {
//     def appendTags(s: ST[C], tags: BaseTags) = {
//       val (id, next) = s.managed.ids.run()
//       val is0 = ManagedState(s.managed.env, s.managed.span, s.managed.path :+ Call(id, tags), next)
//       s.copy(managed = is0)
//     }
//     def updateUnmanaged(s: ST[C], unmanaged: C): ST[C] = s.copy(unmanaged = unmanaged)
//   }

//   type Pre[F[_], C, A] = Precepte[BaseTags, MS, C, F, A]

// }