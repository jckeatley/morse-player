/*
 * Copyright (C) 2026 Jonathan Keatley
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program. If not, see <https://www.gnu.org/licenses/>.
 */

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
