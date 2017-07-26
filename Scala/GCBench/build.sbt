import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "GCBench",
    mainClass in assembly := Some("GCBench"),
    test in assembly := {},

    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.rogach" %% "scallop" % "3.0.3"
  )
