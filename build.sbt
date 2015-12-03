name := "Plesiohedrony"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

seq(lwjglSettings: _*)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.1"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.4.1"

libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.4.1"

libraryDependencies += "org.iq80.leveldb"            % "leveldb"          % "0.7"

libraryDependencies += "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8"