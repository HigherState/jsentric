import sbt._
import Keys._

object build extends Build {

  val scala = "2.11.8"

  lazy val jsentric = Project(
    id = "jsentric",
    base = file("."),
    settings = Defaults.defaultSettings ++ commonSettings ++ Publishing.settings
  )

  lazy val commonSettings = Seq(
    organization := "io.higherstate",
    version := "1.0.2",
    scalaVersion := scala,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps", "-language:reflectiveCalls",
      "-unchecked",
      "-Xfatal-warnings",
      "-Yinline-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Xfuture"
    ),
    javacOptions ++= Seq("-target", "1.8", "-source", "1.8", "-Xlint:deprecation"),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.1",
      "io.argonaut" %% "argonaut" % "6.2-M1",
      "com.chuusai" %% "shapeless" % "2.3.0",
      "joda-time" % "joda-time" % "2.9.2",
      "org.joda" % "joda-convert" % "1.8",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    ),
    resolvers ++= Seq(
      "Maven Central Server" at "http://repo1.maven.org/maven2",
      "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
    )
  )
}