# Plesiohedrony

Scala + LWJGL tessellation demo.

## Requirements

* running with a __JRE8__ JVM.
* LWJGL natives libraries (v2.9.3) in a folder `libs` under the root of the project.

## Usage

`sbt run` to start.

Default key bindings are WASD for x,y movements, space to go up, left control to go down, left shift to go faster, F11 to toggle fullscreen.

Left click on a face adds a new polyhedron matching the tessellation in the scene, right click removes it, middle click outputs face info.
