enablePlugins(JavaAppPackaging)

lazy val morsePlayer = (project in file("."))
  .settings(
    name := "morse-player",
    maintainer := "jckeatley@gmail.com",
    organization := "us.keatley.morse",
    version := "1.0.4",
    scalaVersion := "3.9.0",
    libraryDependencies ++= Seq(
      "jline" % "jline" % "2.14.6",
      "org.scalatest" %% "scalatest" % "3.2.20" % "test"
    ),
    resolvers ++= Seq(
      Resolver.DefaultMavenRepository,
      Resolver.typesafeRepo("releases")
    )
  )
