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

/**
  * Custom Graph ADT for precepte to graphviz manipulations
  */
sealed abstract class Node {
  val id: String
  val value: String
  def viz: String
}

final case class Leaf(id: String, value: String) extends Node {
  def viz = s""""$id" [label = "$value"]"""
}

final class Sub private (val id: String, val value: String, val graph: Graph)
    extends Node {
  private def nodesG =
    graph.nodes.map(_.viz).mkString("\n")

  def viz = s"""
    |subgraph "$id" {
    |  label = "$value"
    |  $nodesG
    |}
  """.stripMargin

  override def toString = s"Sub($id, $value, ${graph.toString})"
}

object Sub {
  @inline def apply(id: String, value: String, graph: Graph): Sub =
    new Sub("cluster_" + id, value, graph)
  @inline def unapply(g: Sub): Option[(String, String, Graph)] =
    Option((g.id, g.value, g.graph))
}

final case class Edge(from: String, to: String) {
  @inline def viz: String = s""""$from" -> "$to"""" // "
}

object Graph {
  @inline val empty: Graph = Graph(Set.empty, Set.empty)
}

final case class Graph(nodes: Set[Node], edges: Set[Edge]) {
  private def allEdges: Seq[Edge] =
    edges.toSeq ++ nodes.flatMap {
      case Sub(_, _, g) => g.allEdges
      case _            => Seq.empty
    }

  def +(n: Node): Graph =
    n match {
      case Sub(_, _, g) =>
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

  def addBranches(gs: Graph*): Graph = {
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

  private def allEdgesG: String = allEdges.map(_.viz).mkString("\n  ")
  private def allNodesG: String = nodes.map(_.viz).mkString("\n  ")

  lazy val children: Set[Node] = {
    val m = edges.map(e => e.from -> e.to).toMap
    def step(nodes: Set[Node], m: collection.Map[String, String]): Set[Node] = {
      nodes.flatMap { n =>
        n match {
          case Leaf(id, _) if !m.contains(id) => Seq(n)
          case Sub(_, _, sub)                 => step(sub.children, m)
          case _                              => Seq()
        }
      }
    }
    step(nodes, m)
  }

  lazy val parents: Set[Node] = {
    val m = edges.map(e => e.to -> e.from).toMap
    def step(nodes: Set[Node], m: collection.Map[String, String]): Set[Node] = {
      nodes.flatMap { n =>
        n match {
          case Leaf(id, _) if !m.contains(id) => Seq(n)
          case Sub(_, _, sub)                 => step(sub.parents, m)
          case _                              => Seq()
        }
      }
    }
    step(nodes, m)
  }

  @inline def viz: String = s"""
    |digraph G {
    |  $allNodesG
    |  $allEdgesG
    |}
  """.stripMargin
}

trait ToNode[S] {
  def toNode(s: S): Node
}
