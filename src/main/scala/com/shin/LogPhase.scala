package com.shin

import java.sql.Timestamp

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

class LogPhase(override val global: Global) extends Plugin {
  override val name = "logphase"
  override val description = "Add debugging logs to a function annotated with @spy."
  override val components = List(new LogPhaseComponent(global))
}

class LogPhaseComponent(val global: Global)
  extends PluginComponent with TypingTransformers {

  import global._

  override val phaseName = "logphase"
  override val runsAfter = List("parser")

  override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
    override def apply(unit: CompilationUnit) {
      unit.body = new LogTransformer(unit).transform(unit.body)
    }
  }

  class LogTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {
    def logEmbedding(rhs: Tree, name: TermName, params: List[ValDef]): global.Block = {
      val (entryLog, exitLog) = getLogs2(name, params)
      Block(
        entryLog,
        DefDef(Modifiers(), TermName("runMethod"), List(), List(), TypeTree(), rhs),
        q"val _logRun = runMethod",
        exitLog,
        q"_logRun"
      )
    }

    override def transform(tree: Tree): global.Tree = tree match {
      case dd: DefDef =>
        if (shouldSpy(dd.mods)) {
          treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt,
            logEmbedding(dd.rhs, dd.name, dd.vparamss.flatten))
        } else dd
      case _ => super.transform(tree)
    }

    def getLogs(name: TermName, params: List[ValDef]): (global.Tree, global.Tree) = {
      //datetime
      val timeString = q"""(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")).format(new java.sql.Timestamp(System.currentTimeMillis()))"""

      //firstlog
      val startStringIQ = q"""${s" - [ENTRY] ${name.decodedName.toString}"}"""
      val paramsListIQ = q"""${params.map(x => x.name.decodedName)}.map(_.toString).mkString(", ")"""

      //secondlog
      val startStringOQ = q"${s" - [EXIT] ${name.decodedName.toString} - %s"}.format(_logRun)"

      //join
      val logI =
        q"""
          $timeString + ..$startStringIQ + "(" + ..$paramsListIQ + ")"
        """
      val logO =
        q"""
          $timeString + $startStringOQ
       """

      //print
      val logIn =q"println($logI)"
      val logOut = q"println($logO)"
      (logIn, logOut)
    }

    def shouldSpy(dd: global.Modifiers): Boolean =
      dd.hasAnnotationNamed(TypeName(typeOf[annotations.spy].typeSymbol.name.toString))

    def intersperse[E](x: E, xs:List[E]): List[E] = (x, xs) match {
      case (_, Nil)     => Nil
      case (_, List(x))  => List(x)
      case (sep, y::ys) => y+:sep+:intersperse(sep, ys)
    }

    def newTransformer(unit: CompilationUnit) = new LogTransformer(unit)
  }
}