name := "Plesiohedrony"

version := "1.0"

scalaVersion := "2.10.2"

scalaSource in Compile := file("src")

unmanagedResourceDirectories in Compile <+=
    baseDirectory { _ / "resources" }

seq(lwjglSettings: _*)