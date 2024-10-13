val scala3Version = "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "clarinet-metrics-analysis",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

libraryDependencies += "org.scala-lang" %% "toolkit" % "0.4.0"
libraryDependencies += "org.apache.fury" % "fury-scala_3" % "0.8.0"