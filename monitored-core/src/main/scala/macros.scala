package com.mfglabs.monitoring.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import com.mfglabs.monitoring.Call.Tags.Callee

object Macros {
  implicit def callee: Callee = macro Macros.calleeMacro

  def calleeMacro(c: Context) = {
  	import c.universe._
  	q"""Callee(${c.internal.enclosingOwner.fullName})"""
  }
}