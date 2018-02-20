package com.shin
//
//import scala.tools.nsc.plugins.{Plugin, PluginComponent}
//import scala.tools.nsc.transform.TypingTransformers
//import scala.tools.nsc.{Global, Phase}
//
//
//class LogPhase(val global: Global) extends Plugin {
//  val name: String = "logphase"
//  val components: List[PluginComponent] = List(Component)
//  val description: String = "Add debug logs to all functions with the mask @spy"
//
//  object Component extends PluginComponent with TypingTransformers {
//    val global: LogPhase.this.global.type = LogPhase.this.global
//    val runsAfter: List[String] = List("parser")
//    val phaseName: String = LogPhase.this.name
//    def newPhase(_prev: Phase) = new StdPhase(_prev) {
//      override def apply(unit: global.CompilationUnit): Unit = {
//        new StdPhase(prev) {
//          override def apply(unit: global.CompilationUnit) {
//            unit.body = AddLogsTransformer(unit).transform(unit.body)
//          }
//        }
//      }
//    }
//
//    case class AddLogsTransformer(unit: global.CompilationUnit) extends TypingTransformer(unit) {
//      import global._
//      def methodWrapper(rhs: global.Tree) = {
//        Block(
//          q"""println("Inside - before")""",
//          DefDef(Modifiers(), TermName("runMethod"), List(), List(), TypeTree(), rhs),
//          q"val r = runMethod",
//          q"""println("Inside - after")""",
//          q"r"
//        )
//      }
//
//      override def transform(tree: Tree) = tree match {
//        case dd: DefDef =>
//          println(dd)
//          val wrappedMethod = treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams,
//            dd.vparamss, dd.tpt, methodWrapper(dd.rhs))
//          println(wrappedMethod)
//          wrappedMethod
//        case _ => super.transform(tree)
//      }
//    }
//
//    def newTransformer(unit: global.CompilationUnit): global.Transformer = AddLogsTransformer(unit)
//  }
//}
//
//


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
  override def newPhase(prev: Phase) = new StdPhase(prev) {
    override def apply(unit: CompilationUnit) {
      unit.body = new MyTypingTransformer(unit).transform(unit.body)
    }
  }

  class MyTypingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {
    def methodWrapper(rhs: Tree, name: TermName, params: List[ValDef]) = {
      Block(
        q"""
           logger.debug("[ENTRY] " + ${name.decodedName.toString} + " - " + ${params.map(x => "$" + s"${x.name.decodedName}").mkString(", ")})
          """,
        DefDef(Modifiers(), TermName("runMethod"), List(), List(), TypeTree(), rhs),
        q"val _logRun = runMethod",
        q"""
           logger.debug(s + "[EXIT] " + ${name.decodedName.toString} + " - " + "$$_logRun")
          """,
        q"_logRun"
      )
    }

    override def transform(tree: Tree) = tree match {
      case dd: DefDef =>
          //println(dd)
          val wrappedMethod = treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams,
            dd.vparamss, dd.tpt, methodWrapper(dd.rhs, dd.name, dd.vparamss.flatten))
          println(wrappedMethod)
          wrappedMethod
      case _ => super.transform(tree)
    }
  }

  def newTransformer(unit: CompilationUnit) = new MyTypingTransformer(unit)
}