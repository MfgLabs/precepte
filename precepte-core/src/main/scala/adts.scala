package com.mfglabs
package precepte

/** Span uniquely identifies a macro-event managed by Precepte
 * and potentially constituted of multiple micro-events.
 * A macro-event doesn't need to be local to a system, can be distributed in space & time...
 */
case class Span(value: String) extends AnyVal

object Span {
  def gen = Span(java.util.UUID.randomUUID.toString)
}

/** Id uniquely identifies a micro-event in the scope of a macro-event managed by Precepte */
case class CId(value: String) extends AnyVal

object CId {
  def gen = CId(scala.util.Random.alphanumeric.take(7).mkString)
}

/** Abstract Name/Value Tag used to classify your Call */
abstract class Tag(val name: String, val value: String)

/** A call representing a micro-event in the scope of a macro-event.
 * A micro-event happens (generally) locally to a system and is not distributed
 * It is identified by a local Id and enhanced with a few tags
 */
case class Call[T <: Tags](id: CId, tags: T)

object Call {
  type Path[T <: Tags] = Vector[Call[T]]
}

/** A typed group of tags */
trait Tags
object NoTags extends Tags
case class BaseTags(callee: Tags.Callee, category: Tags.Category) extends Tags

object Tags {
  case class Callee(override val value: String) extends Tag("callee", value)

  abstract class Category(value: String) extends Tag("category", value)
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


/** The typed environment in which an event happens */
trait Env
case class BaseEnv(host: Tags.Host, environment: Tags.Environment, version: Tags.Version) extends Env

/** The state gathering all data concerning current execution context */
trait PState[T <: Tags]


trait PStatable[T <: Tags, S <: PState[T]] {
  def run(s: S, id: CId, tags: T): S
}

case class PStateBase[E <: Env, T <: Tags, C](span: Span, env: E, path: Call.Path[T], value: C) extends PState[T]

object PStateBase {
  implicit def pstatable[E <: Env, T <: Tags, C] = new PStatable[T, PStateBase[E, T, C]] {
    def run(s: PStateBase[E, T, C], id: CId, tags: T): PStateBase[E, T, C] = s.copy(path = s.path :+ Call(id, tags))
  }

  implicit def rootable[E <: Env, T <: Tags, C] = new Rootable[T, PStateBase[E, T, C]] {
    def toRoot(s: PStateBase[E, T, C]): Root[T, PStateBase[E, T, C]] = Root[T, PStateBase[E, T, C]](s.span, Vector.empty)
  }
}

/** The graph of execution of a Call */
// sealed trait Graph[T <: Tags, C, G <: Graph[T, C, G]] {
sealed trait Graph[T <: Tags, S <: PState[T], G <: Graph[T, S, G]] {
  val children: Vector[GraphNode[T, S]]
  def addChildren(cs: Vector[GraphNode[T, S]]): G
  def addChild(c: GraphNode[T, S]): G =
    addChildren(Vector(c))
}

// case class GraphNode[T <: Tags, C](id: CId, value: C, tags: T, children: Vector[GraphNode[T, C]]) extends Graph[T, C, GraphNode[T, C]] {
case class GraphNode[T <: Tags, S <: PState[T]](id: CId, value: S, tags: T, children: Vector[GraphNode[T, S]]) extends Graph[T, S, GraphNode[T, S]] {
  def addChildren(cs: Vector[GraphNode[T, S]]): GraphNode[T, S] =
    this.copy(children = children ++ cs)
}

case class Root[T <: Tags, S <: PState[T]](span: Span, children: Vector[GraphNode[T, S]]) extends Graph[T, S, Root[T, S]] {
  def addChildren(cs: Vector[GraphNode[T, S]]): Root[T, S] =
    this.copy(children = children ++ cs)
}

trait Rootable[T <: Tags, S <: PState[T]] {
  def toRoot(s: S): Root[T, S]
}
