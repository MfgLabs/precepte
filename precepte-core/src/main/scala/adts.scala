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

case class Node(id: String, value: String)
case class Edge(from: String, to: String)
case class Graph(nodes: Set[Node], edges: Set[Edge])

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