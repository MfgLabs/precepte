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
case class Call[T <: Tags](id: CId, tags: T) {
  def map[T2 <: Tags](f: T => T2) = Call[T2](id, f(tags))
}

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
case class State[E <: Env, T <: Tags, C](span: Span, env: E, path: Call.Path[T], value: C) {
  def mapE[E2 <: Env](f: E => E2): State[E2, T, C] = State[E2, T, C](span, f(env), path, value)
  def mapT[T2 <: Tags](f: T => T2): State[E, T2, C] = State[E, T2, C](span, env, path.map(_.map(f)), value)
}

/** The graph of execution of a Call */
sealed trait Graph[T <: Tags, C, G <: Graph[T, C, G]] {
  val children: Vector[GraphNode[T, C]]
  def addChildren(cs: Vector[GraphNode[T, C]]): G
  def addChild(c: GraphNode[T, C]): G =
    addChildren(Vector(c))
}

case class GraphNode[T <: Tags, C](id: CId, value: C, tags: T, children: Vector[GraphNode[T, C]]) extends Graph[T, C, GraphNode[T, C]] {
  def addChildren(cs: Vector[GraphNode[T, C]]): GraphNode[T, C] =
    this.copy(children = children ++ cs)
}

case class Root[T <: Tags, C](span: Span, children: Vector[GraphNode[T, C]]) extends Graph[T, C, Root[T, C]] {
  def addChildren(cs: Vector[GraphNode[T, C]]): Root[T, C] =
    this.copy(children = children ++ cs)
}
