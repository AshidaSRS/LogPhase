package com.shin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._

class LogPhase(override val global: Global) extends Plugin {
  override val name = "logphase"
  override val description = "Compiler plugin"
  override val components = List(new CompilerPluginComponent(global))
}

class CompilerPluginComponent(val global: Global)
  extends PluginComponent with TypingTransformers {
  import global._
  override val phaseName = "logphase"
  override val runsAfter = List("parser")
  override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
    override def apply(unit: CompilationUnit) {
      unit.body = new MyTypingTransformer(unit).transform(unit.body)
    }
  }

  class MyTypingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {
    def methodWrapper(rhs: Tree, name: TermName, params: List[ValDef]): global.Block = {
      val (a, b) = getLogs(name, params)
      Block(
        a,
        DefDef(Modifiers(), TermName("runMethod"), List(), List(), TypeTree(), rhs),
        q"val _logRun = runMethod",
        b,
        q"_logRun"
      )
    }

    override def transform(tree: Tree): global.Tree = tree match {
      case dd: DefDef =>
          val wrappedMethod = treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams,
            dd.vparamss, dd.tpt, methodWrapper(dd.rhs, dd.name, dd.vparamss.flatten))
          println(wrappedMethod)
          wrappedMethod
      case _ => super.transform(tree)
    }
  }

  def getLogs(name: TermName, params: List[ValDef]): (global.Tree, global.Tree) = {
    val logI = StringContext(s"[ENTRY] ${name.decodedName.toString} - ${params.map(x => "$" + s"${x.name.decodedName}").mkString(", ")}")
    val logO = StringContext(s"[EXIT] ${name.decodedName.toString} - - $$_logRun")
    val logIn = cq"""logger.debug($logI)"""
    val logOut = cq"""logger.debug($logO)"""
    (logIn, logOut)
  }

  def newTransformer(unit: CompilationUnit) = new MyTypingTransformer(unit)
}