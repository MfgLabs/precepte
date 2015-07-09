package com.mfglabs
package precepte


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
  def gen = PId(scala.util.Random.alphanumeric.take(7).mkString)
}

/** Abstract Name/Value Tag used to classify your Call */
abstract class Tag(val name: String, val value: String)

/** A call representing a micro-event in the scope of a macro-event.
 * A micro-event happens (generally) locally to a system and is not distributed
 * It is identified by a local Id and enhanced with a few tags
 */
case class Call[T <: Tags](id: PId, tags: T)

object Call {
  type Path[T <: Tags] = Vector[Call[T]]
}

/** A typed group of tags */
trait Tags
object NoTags extends Tags
case class BaseTags(callee: Tags.Callee, category: Tags.Category) extends Tags  {
  override def toString = s"($callee, $category)"
}

object Tags {
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
}

/** The state gathering all data concerning current execution context */
case class PState[Tags, ManagedState, UnmanagedState](
  managed: ManagedState,
  unmanaged: UnmanagedState)

trait PIdSeries {
  def run(): (PId, PIdSeries)
}

trait PStateUpdater[Tags, MS, FS] {
  type S = PState[Tags, MS, FS]
  def appendTags(s: S, t: Tags): S
  def updateUnmanaged(s: S, ext: FS): S
}


/** The typed environment in which an event happens */
trait Env
case class BaseEnv(host: Tags.Host, environment: Tags.Environment, version: Tags.Version) extends Env

case class PIdStream(ids: Stream[PId] = Stream.continually(PId.gen)) extends PIdSeries {
  def run() = ids.head -> PIdStream(ids.tail)
}