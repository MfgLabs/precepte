package com.mfglabs.monitoring

case class Call(id: Call.Id, tags: Call.Tags) {
  def toJson = ???
}

object Call {
	case class Span(value: String) extends AnyVal
  case class Id(value: String) extends AnyVal

  case class Tags(values: Tags.Tag*) {
    override def toString = s"Tags(${values.toList})"
    def ++(ts: Tags) = Tags((values ++ ts.values):_*)
  }

  object Tags {
    val empty = Tags()
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
  }

  type Path = Vector[Call]

  sealed trait Graph[C, G <: Graph[C, G]] {
    val children: Vector[GraphNode[C]]
    def addChildren(cs: Vector[GraphNode[C]]): G
    def addChild(c: GraphNode[C]): G =
      addChildren(Vector(c))
  }

  case class GraphNode[C](id: Call.Id, value: C, tags: Tags, children: Vector[GraphNode[C]]) extends Graph[C, GraphNode[C]] {
    def addChildren(cs: Vector[GraphNode[C]]): GraphNode[C] =
      this.copy(children = children ++ cs)
  }

  case class Root[C](span: Call.Span, children: Vector[GraphNode[C]]) extends Graph[C, Root[C]] {
    def addChildren(cs: Vector[GraphNode[C]]): Root[C] =
      this.copy(children = children ++ cs)
  }

  trait Env
  case class BaseEnv(host: Tags.Host, environment: Tags.Environment) extends Env
  case class State[E <: Env, C](span: Call.Span, env: E, path: Path, value: C)

  object Id {
    def gen = Id(scala.util.Random.alphanumeric.take(7).mkString)
  }
  object Span {
    def gen = Span(java.util.UUID.randomUUID.toString)
  }
}