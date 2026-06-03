enablePlugins(ScalaJSPlugin)

ThisBuild / scalaVersion := "3.4.2"
ThisBuild / organization := "presifeur"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings"
)

lazy val sharedSrc = Def.setting((ThisBuild / baseDirectory).value / "src" / "main" / "scala")
lazy val sharedTestSrc = Def.setting((ThisBuild / baseDirectory).value / "src" / "test" / "scala")

lazy val root = (project in file("."))
  .settings(
    name := "presifeur",
    Compile / mainClass := Some("presifeur.Main"),
    Compile / unmanagedSources / excludeFilter := new SimpleFileFilter(file =>
      file.getPath.contains("/presifeur/server/") ||
      file.getPath.contains("/presifeur/io/") ||
      file.getName == "ConsoleMain.scala" ||
      file.getName == "ServerMain.scala"
    ),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    ),
    scalaJSUseMainModuleInitializer := true
  )

lazy val serverJVM = (project in file("serverJVM"))
  .settings(
    name := "presifeur-server",
    Compile / mainClass := Some("presifeur.ServerMain"),
    Compile / unmanagedSourceDirectories += sharedSrc.value,
    Test / unmanagedSourceDirectories += sharedTestSrc.value,
    Compile / unmanagedSources / excludeFilter := new SimpleFileFilter(file =>
      file.getPath.contains("/presifeur/web/") ||
      file.getName == "Main.scala"
    ),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"       % "2.1.14",
      "dev.zio" %% "zio-http"  % "3.0.1",
      "dev.zio" %% "zio-json"  % "0.7.3",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )

addCommandAlias("server", "serverJVM/runMain presifeur.ServerMain")
addCommandAlias("cli",    "serverJVM/runMain presifeur.ConsoleMain")
