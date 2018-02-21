package com.shin

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.reflect.macros.whitebox.Context
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
        if (dd.mods.hasAnnotationNamed(TypeName(typeOf[annotations.spy].typeSymbol.name.toString))) {
          val wrappedMethod = treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams,
            dd.vparamss, dd.tpt, methodWrapper(dd.rhs, dd.name, dd.vparamss.flatten))
          wrappedMethod
        } else dd
      case _ => super.transform(tree)
    }

    def getLogs(name: TermName, params: List[ValDef]): (global.Tree, global.Tree) = {
      val meh = params.map(x => q"""print("%s ".format(${x.name.decodedName}))""")
      val logI = s"[ENTRY] ${name.decodedName.toString} - "
      val logO = q"${s"[EXIT] ${name.decodedName.toString} - %s"}.format(_logRun)"
      val logIn = q"""print($logI); $meh; println("")"""
      val logOut = q"println($logO)"
      (logIn, logOut)
    }

    def newTransformer(unit: CompilationUnit) = new MyTypingTransformer(unit)
  }

}