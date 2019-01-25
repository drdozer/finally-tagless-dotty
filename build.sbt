val dottyVersion = "0.13.0-bin-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finally-tagless-dotty",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
