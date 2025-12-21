// SPDX-License-Identifier: MIT
ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "tw.edu.ncku"

val chiselVersion = "3.6.1"

lazy val root = (project in file("4-soc"))
  .settings(
    name := "mycpu-ai-soc",
    // 移除之前的 unmanagedSourceDirectories，只編譯 4-soc 內部的檔案
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.6.0" % "test",
      "edu.berkeley.cs" %% "firrtl" % "1.6.0",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-feature",
      "-Xcheckinit",
      "-Wconf:cat=deprecation:s",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  )