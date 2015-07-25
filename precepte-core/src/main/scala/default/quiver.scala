package com.mfglabs
package precepte
package default

import scala.language.higherKinds

import quiver._
import scalaz.{Monad, Semigroup}
import scalaz.syntax.monad._


object PreQuiver {

  val nil = empty[String, String, Unit]

  // (parents, graph, children)
  type PGraph = Graph[String, String, Unit]
  type G = (Vector[String], PGraph, Vector[String])
  
  def node[F[_], C, A](s: Pre[F, C, A]#S): G = {
    val id = s.managed.path.last.tags.callee.value + "_" + s.managed.path.last.id.value
    val self = Vector(id)
    (self, nil & Context(Vector(), id, s.managed.path.last.tags.callee.value, Vector()), self)
  }

  def append(cs: Vector[G], g0: G): G = {
    val children = cs.flatMap(_._3)
    cs.foldLeft(g0){ (g, c) =>
      val edges =
        for {
          gChildren <- g._3
          cRoot <- c._1
        } yield LEdge(gChildren, cRoot, ())
      (g._1, (g._2 union c._2).addEdges(edges), children)
    }
  }

  implicit class preQuiver[F[_]: Monad, C : Semigroup, A](val pre: Pre[F, C, A]) {
    def scan2Quiver(s: ST[C]): F[(ST[C], A, Graph[String, String, Unit])] =
      pre.scan(node _, append _)(s, (Vector.empty, nil, Vector.empty)).map{ case (s, a, g) => (s, a, g._2) }
  }
}
