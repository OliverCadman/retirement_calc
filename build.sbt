ThisBuild / organization := "com.retirement_calculator"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / version := "0.1.0"
ThisBuild / name := "retirement_calc"

lazy val root = (project in file("."))
  .settings(
    name := "retirement_calc",
    Compile / mainClass := Some("app.SimulatePlanApp"),
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.19",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.typelevel" %% "cats-core" % "2.13.0"
    ),
    assembly / assemblyJarName := "retirement_calc.jar"
  )

scalacOptions ++= Seq("-Ydelambdafy:inline", "-Ypartial-unification")

