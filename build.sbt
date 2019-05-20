import Dependencies._

ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "puzzle-n",
    libraryDependencies ++= Seq(
      catsCore,
      catsEffect,
      scalaTest % Test,
      scalaCheck % Test
    )
  )

