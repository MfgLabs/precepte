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
trait PState[T <: Tags]

case class PState0[Tags, ManagedState, UnmanagedState](
  managed: ManagedState,
  unmanaged: UnmanagedState
)

trait PIdSeries {
  def run(): (PId, PIdSeries)
}

/** A hidden State Monad */
trait PStatable[T <: Tags, S <: PState[T]] {
  def run(s: S, ids: PIdSeries, tags: T): (S, PIdSeries)
}

trait PStateUpdater[Tags, MS, FS] {
  type S = PState0[Tags, MS, FS]
  def appendTags(s: S, t: Tags): S
  def updateUnmanaged(s: S, ext: FS): S
}

/** A hidden State Monad */
trait PGraphStatable[T <: Tags, S <: PState[T]] {
  def run(s: S, ids: PIdSeries, tags: T): (S, PIdSeries, GraphNode[T, S])
}


/** The typed environment in which an event happens */
trait Env
case class BaseEnv(host: Tags.Host, environment: Tags.Environment, version: Tags.Version) extends Env


case class PStateBase[E <: Env, T <: Tags, C](span: Span, env: E, path: Call.Path[T], value: C) extends PState[T]

object PStateBase {
  
  implicit def pstatable[E <: Env, T <: Tags, C] = new PStatable[T, PStateBase[E, T, C]] {
    def run(s: PStateBase[E, T, C], ids: PIdSeries, tags: T): (PStateBase[E, T, C], PIdSeries) = {
      val (id, next) = ids.run()
      s.copy(path = s.path :+ Call(id, tags)) -> next
    }
  }

  implicit def pgraphstatable[E <: Env, T <: Tags, C] = new PGraphStatable[T, PStateBase[E, T, C]] {
    def run(s: PStateBase[E, T, C], ids: PIdSeries, tags: T): (PStateBase[E, T, C], PIdSeries, GraphNode[T, PStateBase[E, T, C]]) = {
      val (id, next) = ids.run()
      val g0 = GraphNode(id, s, tags, Vector.empty)
      (s.copy(path = s.path :+ Call(id, tags)), next, g0)
    }
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
  def addChild(c: GraphNode[T, S]): G = addChildren(Vector(c))
}

case class GraphNode[T <: Tags, S <: PState[T]](id: PId, value: S, tags: T, children: Vector[GraphNode[T, S]]) extends Graph[T, S, GraphNode[T, S]] {
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

case class PIdStream(ids: Stream[PId] = Stream.continually(PId.gen)) extends PIdSeries {
  def run() = ids.head -> PIdStream(ids.tail)
}

sealed trait Node
case class NodeR(span: Span) extends Node {
  override def toString = s"root($span)"
}
case class Node0[T <: Tags](id: PId, tags: T) extends Node {
  override def toString = s"node($tags, $id)"
}

object Node {
  implicit def show[T <: Tags] = scalaz.Show.showA[Node]
}