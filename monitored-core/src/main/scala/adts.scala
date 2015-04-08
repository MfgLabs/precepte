package com.mfglabs.monitoring

case class Call[T <: Call.Tags](id: Call.Id, tags: T)

object Call {
	case class Span(value: String) extends AnyVal
  case class Id(value: String) extends AnyVal

  trait Tags
  object NoTags extends Tags
  case class BaseTags(callee: Tags.Callee, category: Tags.Category) extends Tags

  object Tags {
    abstract class Tag(val name: String, val value: String)
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

  type Path[T <: Tags] = Vector[Call[T]]

  sealed trait Graph[T <: Tags, C, G <: Graph[T, C, G]] {
    val children: Vector[GraphNode[T, C]]
    def addChildren(cs: Vector[GraphNode[T, C]]): G
    def addChild(c: GraphNode[T, C]): G =
      addChildren(Vector(c))
  }

  case class GraphNode[T <: Tags, C](id: Call.Id, value: C, tags: T, children: Vector[GraphNode[T, C]]) extends Graph[T, C, GraphNode[T, C]] {
    def addChildren(cs: Vector[GraphNode[T, C]]): GraphNode[T, C] =
      this.copy(children = children ++ cs)
  }

  case class Root[T <: Tags, C](span: Call.Span, children: Vector[GraphNode[T, C]]) extends Graph[T, C, Root[T, C]] {
    def addChildren(cs: Vector[GraphNode[T, C]]): Root[T, C] =
      this.copy(children = children ++ cs)
  }

  trait Env
  case class BaseEnv(host: Tags.Host, environment: Tags.Environment, version: Tags.Version) extends Env
  case class State[E <: Env, T <: Tags, C](span: Call.Span, env: E, path: Path[T], value: C)

  object Id {
    def gen = Id(scala.util.Random.alphanumeric.take(7).mkString)
  }
  object Span {
    def gen = Span(java.util.UUID.randomUUID.toString)
  }
}