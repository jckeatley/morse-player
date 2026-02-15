enablePlugins(JavaAppPackaging)

lazy val morsePlayer = (project in file("."))
  .settings(
    name := "morse-player",
    organization := "us.keatley.morse",
    version := "1.0.1",
    scalaVersion := "3.7.4",
    libraryDependencies ++= Seq(
      "jline" % "jline" % "2.14.6",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    ),
    resolvers ++= Seq(
      Resolver.DefaultMavenRepository,
      Resolver.typesafeRepo("releases")
    )
  )
