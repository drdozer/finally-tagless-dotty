val dottyVersion = 
//  "0.13.0-bin-SNAPSHOT" // locally built
  dottyLatestNightlyBuild.get // autofetched nightly - doesn't compile though
//  "0.13.0-bin-20190129-05cdff8-NIGHTLY"
//  "0.13.0-bin-20190128-36740bf-NIGHTLY"


lazy val root = project
  .in(file("."))
  .settings(
    name := "finally-tagless-dotty",
    version := "0.1.0",

    scalaVersion := dottyVersion,

//    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
    libraryDependencies += ("org.scalacheck" %% "scalacheck" % "1.14.0" % "test").withDottyCompat(scalaVersion.value)
  )
