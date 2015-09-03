package com.mfglabs
package precepte

import scala.language.higherKinds

/** A typed group of tags */
trait Tags
object NoTags extends Tags

/** The state gathering all data concerning current execution context */
case class PState[Ta, ManagedState, UnmanagedState](
  managed: ManagedState,
  unmanaged: UnmanagedState)

trait PStateUpdater[Ta, MS, FS] {
  type S = PState[Ta, MS, FS]
  def appendTags(s: S, t: Ta, idx: Int): S
  def updateUnmanaged(s: S, ext: FS): S
}

trait Node {
  val id: String
  val value: String
  def viz: String
}
case class Leaf(id: String, value: String) extends Node {
  def viz = s""""$id" [label = "$value"]"""
}

class Sub private (val id: String, val value: String, val graph: Graph) extends Node {
  private def nodesG =
    graph.nodes.map(_.viz).mkString("\n")

  def viz = s"""
    |subgraph "$id" {
    |  label = "$value"
    |  $nodesG
    |}
  """.stripMargin

  override def toString = s"Sub($id, $value, $graph)"
}

object Sub {
  def apply(id: String, value: String, graph: Graph) =
    new Sub("cluster_" + id, value, graph)
  def unapply(g: Sub) = Option((g.id, g.value, g.graph))
}

case class Edge(from: String, to: String) {
  def viz = s""""$from" -> "$to"""" // "
}

object Graph {
  val empty = Graph(Set.empty, Set.empty)
}

case class Graph(nodes: Set[Node], edges: Set[Edge]) {
  private def allEdges: Seq[Edge] =
    edges.toSeq ++ nodes.flatMap {
      case Sub(_, _, g) => g.allEdges
      case _ => Seq.empty
    }

  def +(n: Node) = n match {
    case Sub(id, value, g) =>
      val ns = nodes + n
      val es = edges ++
        (for {
          c <- children
          p <- g.parents
        } yield Edge(c.id, p.id))
      Graph(ns, es)
    case _ =>
      val ns = nodes + n
      val es = edges ++ (for { c <- children } yield Edge(c.id, n.id))
      Graph(ns, es)
  }

  def addBranches(gs: Graph*) = {
    val es =
      edges ++
        (for {
          c <- children
          g <- gs
          p <- g.parents
        } yield Edge(c.id, p.id))
    val ns =
      nodes ++
        (for {
          g <- gs
          n <- g.nodes
        } yield n)
    Graph(ns, es)
  }

  private def allEdgesG = allEdges.map(_.viz).mkString("\n  ")
  private def allNodesG = nodes.map(_.viz).mkString("\n  ")

  lazy val children: Set[Node] = {
    val m = edges.map(e => e.from -> e.to).toMap
    def step(nodes: Set[Node], m: collection.Map[String, String]): Set[Node] = {
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
    def step(nodes: Set[Node], m: collection.Map[String, String]): Set[Node] = {
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