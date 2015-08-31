package com.mfglabs
package precepte

import scala.language.higherKinds

/** A typed group of tags */
trait Tags
object NoTags extends Tags

object SGraph {
  val Zero = SGraph(Vector.empty)
  sealed trait Step
  case class Sub(id: String, value: String) extends Step
  case class Simple(id: String, value: String) extends Step
  case class Branch(g1: SGraph, g2: SGraph) extends Step
  case object Up extends Step {
    override def toString = "Up"
  }

  private def append(g: Graph, s: Step) =
    (g, s) match {
      case (g, Simple(id, value)) =>
        val node = Leaf(id, value)
        val edges = for { c <- g.children } yield Edge(c.id, node.id)
        Graph(g.nodes + node, g.edges ++ edges)
      case (g, Branch(sg1, sg2)) =>
        val (g1, None) = toGraph(sg1)
        val (g2, None) = toGraph(sg2)
        val edges1 =
          for {
            c <- g.children
            p <- g1.parents
          } yield Edge(c.id, p.id)
        val edges2 =
          for {
            c <- g.children
            p <- g2.parents
          } yield Edge(c.id, p.id)
        Graph(
          g.nodes ++ g1.nodes ++ g2.nodes,
          g.edges ++ g1.edges ++ g2.edges ++ edges1 ++ edges2)
      case e => throw new IllegalArgumentException(e.toString)
    }

  private def toGraph(sgraph: SGraph): (Graph, Option[precepte.Sub]) =
    sgraph.ss.foldLeft[(Graph, Option[precepte.Sub])](Graph(Set.empty, Set.empty) -> None) {
      case ((g, None), s@Simple(id, value)) =>
        append(g, s) -> None
      case ((g, None), s@Branch(sg1, sg2)) =>
        append(g, s) -> None
      case ((g, None), Sub(id, value)) =>
        g -> Some(precepte.Sub(id, value, Graph(Set.empty, Set.empty)))
      case ((g, Some(sub)), Up) =>
        val edges =
          for {
            c <- g.children
            p <- sub.graph.parents
          } yield Edge(c.id, p.id)
        Graph(g.nodes + sub, g.edges ++ edges) -> None
      case ((g, Some(sub)), s) =>
        val sub2 = append(sub.graph, s)
        (g, Some(precepte.Sub(sub.id, sub.value, sub2)))
      case e => throw new IllegalStateException(e.toString)
    }
}

case class SGraph(ss: Vector[SGraph.Step]) {
  def >>(s: SGraph.Step) = SGraph(ss :+ s)
  def >>>(g: SGraph) = SGraph(ss ++ g.ss)
  def >>>:(s: SGraph.Step) = SGraph(s +: ss)
  override def toString = s"Graph(${ss.mkString(" >> ")})"

  final def graph = SGraph.toGraph(this)._1
}

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
case class Sub(id: String, value: String, graph: Graph) extends Node {
  private def nodesG =
    graph.nodes.map(_.viz).mkString("\n")

  def viz = s"""
    |subgraph cluster_$id {
    |  label = "$value"
    |  $nodesG
    |}
  """.stripMargin
}
case class Edge(from: String, to: String) {
  def viz = s""""$from" -> "$to"""" // "
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