ThisBuild / scalaVersion := "3.4.2"
ThisBuild / organization := "presifeur"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "presifeur",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"       % "2.1.14",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    )
  )
