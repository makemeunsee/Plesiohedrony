lazy val root = (project in file("."))
  .settings(
    name := "Plesiohedrony",
    scalaVersion := "3.2.1",
    resolvers += "mvnrepository.com" at "https://mvnrepository.com/artifact/",
    libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl" % "2.9.3",
    libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl_util" % "2.9.3",
    libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.9.3",
    run / fork := true,
    run / javaOptions += "-Djava.library.path=libs/"
  )
