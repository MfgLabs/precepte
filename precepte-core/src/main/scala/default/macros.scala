package com.mfglabs
package precepte
package default

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.language.implicitConversions


object Macros {
  implicit def callee: Callee = macro Macros.calleeMacro

  def params[T](ts: T*): Seq[(String, String)] = macro Macros.paramsMacro[T]
  def param[T](t: T): (String, String) = macro Macros.paramMacro[T]

  def calleeMacro(c: Context) = {
  	import c.universe._
  	q"""_root_.com.mfglabs.precepte.default.Callee(${c.internal.enclosingOwner.fullName})"""
  }

  def paramMacro[T](c: Context)(t: c.Tree) = {
  	import c.universe._
  	val name = t.symbol
  	if(t.symbol != null && t.symbol.isTerm && (t.symbol.asTerm.isVal || t.symbol.asTerm.isVar))
  		q"""(${name.name.encodedName.toString} -> $name.toString)"""
  	else if(t.symbol == null)
  		throw new scala.reflect.internal.FatalError(s"$t is not a val or a var")
  	else
  		throw new scala.reflect.internal.FatalError(s"$name is not a val or a var")
  }

  def paramsMacro[T](c: Context)(ts: c.Tree*) = {
  	import c.universe._
  	val tuples = for(t <- ts) yield paramMacro(c)(t)
  	q"Seq(..$tuples)"
  }
}