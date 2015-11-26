lazy val root = project.in( file(".") ).dependsOn( lwjglPlugin )
lazy val lwjglPlugin = uri("git://github.com/philcali/sbt-lwjgl-plugin")