name := "LogPhase"
version := "0.1.0"
scalaVersion := "2.12.0"
organization := "com.shin"
crossScalaVersions := Seq("2.11.11", scalaVersion.value)
mainClass in (Compile,run) := Some("default.Main")
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.4"