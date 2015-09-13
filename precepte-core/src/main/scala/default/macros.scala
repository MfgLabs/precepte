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
package default

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.language.implicitConversions

/** Some macro helpers for Precepte
  *
  * To use it:
  *
  * `import com.mfglabs.precepte.default.Macros._`
  * 
  */
object Macros {
  /** An implicit macro defining a Callee Precepte Tag evaluated at compile-time using the `enclosingOwner.fullName` (function name + package path) */
  implicit def callee: Callee = macro Macros.calleeMacro

  /** A macro creating at compile-time a sequence of (name, value) for the values you pass to it */
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