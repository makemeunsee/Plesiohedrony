name := "Plesiohedrony"

version := "1.0"

scalaVersion := "2.10.2"

scalaSource in Compile := file("src")

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

unmanagedResourceDirectories in Compile <+=
    baseDirectory { _ / "resources" }

seq(lwjglSettings: _*)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.2.3"