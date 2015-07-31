package com.mfglabs
package precepte

import scala.language.higherKinds

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

/** The state gathering all data concerning current execution context */
case class PState[Ta, ManagedState, UnmanagedState](
  managed: ManagedState,
  unmanaged: UnmanagedState)

trait PIdSeries {
  def run(): (PId, PIdSeries)
}

trait PStateUpdater[Ta, MS, FS] {
  type S = PState[Ta, MS, FS]
  def appendTags(s: S, t: Ta, idx: Int): S
  def updateUnmanaged(s: S, ext: FS): S
}

case class PIdStream(ids: Stream[PId] = Stream.continually(PId.gen)) extends PIdSeries {
  def run() = ids.head -> PIdStream(ids.tail)
}

trait Node {
  val id: String
  val value: String
  def viz: String
}
case class Leaf(id: String, value: String) extends Node {
  def viz = s"""$id [label = "$value"]"""
}
case class Sub(id: String, value: String, graph: Graph) extends Node {
  private def nodesG =
    graph.nodes.map(_.viz).mkString("\n")

  def viz = s"""
    |subgraph $id {
    |  label = "$value"
    |  $nodesG
    |}
  """.stripMargin
}
case class Edge(from: String, to: String) {
  def viz = s"$from -> $to"
}
case class Graph(nodes: Set[Node], edges: Set[Edge]) {
  private def allEdges: Seq[Edge] =
    edges.toSeq ++ nodes.flatMap {
      case Sub(_, _, g) => g.allEdges
      case _ => Seq.empty
    }

  private def allEdgesG = allEdges.map(_.viz).mkString("\n  ")
  private def allNodesG = nodes.map(_.viz).mkString("\n  ")

  lazy val children: Set[Node] = {
    val m = edges.map(e => e.from -> e.to).toMap
    def step(nodes: Set[Node], m: Map[String, String]): Set[Node] = {
      nodes.flatMap { n => n match {
        case Leaf(id, _) if !m.contains(id) => Seq(n)
        case Sub(_, _, sub) => step(sub.children, m)
        case _ => Seq()
      } }
    }
    step(nodes, m)
  }

  lazy val parents: Set[Node] = {
    val m = edges.map(e => e.to -> e.from).toMap
    def step(nodes: Set[Node], m: Map[String, String]): Set[Node] = {
      nodes.flatMap { n => n match {
        case Leaf(id, _) if !m.contains(id) => Seq(n)
        case Sub(_, _, sub) => step(sub.parents, m)
        case _ => Seq()
      } }
    }
    step(nodes, m)
  }

  def viz = s"""
    |digraph G {
    |  $allNodesG
    |  $allEdgesG
    |}
  """.stripMargin
}

trait ToNode[S] {
  def toNode(s: S): Node
}

trait *->*[F0[_]] {}

object *->* {
  implicit def fKindEv[F0[_]] = new *->*[F0] {}
}

trait *->*->*[F0[_, _]] {}
object *->*->* {
  implicit def fKindEv2[F0[_, _]] = new *->*->*[F0] {}
}