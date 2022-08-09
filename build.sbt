Global / scalaVersion := "2.13.8"

lazy val root: Project = (project in file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "Pineapple-Scala",
    scalacOptions += "-Xsource:3",
    // Set to false or remove if you want to show stubs as linking errors
    nativeLinkStubs := true
    // nativeMode := "release-full"
  )
