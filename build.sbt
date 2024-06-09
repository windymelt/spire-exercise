val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "spire-exercise",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "spire" % "0.18.0",
    ),
  )
