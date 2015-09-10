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
package default


/** Abstract Name/Value Tag used to classify your Call */
abstract class Tag(val name: String, val value: String)

case class Callee(override val value: String) extends Tag("callee", value) {
  override def toString = s"callee($value)"
}

abstract class Category(value: String) extends Tag("category", value) {
  override def toString = s"category($value)"
}

object Category {
  def unapply(c: Category) = Some(c.value)
  object Api extends Category("api")
  object Database extends Category("database")
}

abstract class Environment(value: String) extends Tag("environment", value)
object Environment {
  object Test extends Environment("test")
  object Dev extends Environment("dev")
  object Staging extends Environment("staging")
  object Production extends Environment("production")
}

case class Host(override val value: String) extends Tag("host", value)
case class Version(override val value: String) extends Tag("version", value)

/** The typed environment in which an event happens */
trait Env
case class BaseEnv(host: Host, environment: Environment, version: Version) extends Env

case class BaseTags(callee: Callee, category: Category) extends Tags  {
  override def toString = s"($callee, $category)"
}

/** A call representing a micro-event in the scope of a macro-event.
 * A micro-event happens (generally) locally to a system and is not distributed
 * It is identified by a local Id and enhanced with a few tags
 */
case class Call[T <: Tags](id: PId, tags: T)

object Call {
  type Path[T <: Tags] = Vector[Call[T]]
}


/** Span uniquely identifies a macro-event managed by Precepte
 * and potentially constituted of multiple micro-events.
 * A macro-event doesn't need to be local to a system, can be distributed in space & time...
 */
case class Span(value: String) extends AnyVal {
  override def toString = s"span($value)"
}

object Span {
  def gen = Span(java.util.UUID.randomUUID.toString)
}

/** Id uniquely identifies a micro-event in the scope of a macro-event managed by Precepte */
case class PId(value: String) extends AnyVal {
  override def toString = s"pid($value)"
}

object PId {
  val i = new java.util.concurrent.atomic.AtomicInteger(1)
  def gen = PId(i.getAndIncrement().toString) //scala.util.Random.alphanumeric.take(7).mkString)
}


trait PIdSeries {
  def run(): (PId, PIdSeries)
}

case object DefaultPIdSeries extends PIdSeries {
  def run() = PId.gen -> DefaultPIdSeries
}

case class PIdStream(ids: Stream[PId] = Stream.continually(PId.gen)) extends PIdSeries {
  def run() = ids.head -> PIdStream(ids.tail)
}

case class ManagedState[E <: Env, T <: Tags](env: E, span: Span, path: Call.Path[T], ids: PIdSeries = DefaultPIdSeries)


import scala.language.higherKinds
trait DefaultPreBuilder[F[_], C, P[_] <: DefaultPre[F, C, _]] {
  def apply[A](tags: BaseTags) = Precepte[BaseTags](tags)
}