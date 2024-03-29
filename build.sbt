name := "Scala Data Structures"

version := "1"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "io.estatico" %% "newtype" % "0.4.4",
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.roaringbitmap" % "RoaringBitmap" % "0.9.23",
  "org.scalatest" %% "scalatest" % "3.2.0",
)

scalacOptions ++= Seq(
  "-Ymacro-annotations", // Needed by newtype
  "-deprecation", // Warn about deprecated features
  "-encoding", "UTF-8", // Specify character encoding used by source files
  "-feature", // Emit warning and location for usages of features that should be imported explicitly
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:higherKinds", // Allow higher-kinded types
  "-unchecked", // Enable additional warnings where generated code depends on assumptions
  "-Xlint:_", // Enable all available style warnings
  "-Ywarn-macros:after", // Only inspect expanded trees when generating unused symbol warnings
  "-Ywarn-unused:_", // Enables all unused warnings
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
)

enablePlugins(JmhPlugin)
