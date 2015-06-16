name := "jsentric"

organization := "org.higherState"

version := "1.0.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.6",
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "io.argonaut" %% "argonaut" % "6.1-M4",
  "com.chuusai" %% "shapeless" % "2.2.1",
  "joda-time" % "joda-time" % "2.1",
  "org.joda" % "joda-convert" % "1.2"
)