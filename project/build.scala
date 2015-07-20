import sbt._
import Keys._

object build extends Build {

  val scala = "2.11.7"

  lazy val jsentric = Project(
    id = "jsentric",
    base = file("."),
    settings = Defaults.defaultSettings ++ commonSettings ++ Publishing.settings
  )

  lazy val commonSettings = Seq(
    organization := "org.higherState",
    version := "1.0.0",
    scalaVersion := scala,
    scalacOptions ++= Seq("-feature", "-deprecation","-language:implicitConversions","-language:reflectiveCalls","-unchecked"),
    javacOptions ++= Seq("-target", "1.7", "-source", "1.7", "-Xlint:deprecation"),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.2",
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
      "io.argonaut" %% "argonaut" % "6.1",
      "com.chuusai" %% "shapeless" % "2.2.2",
      "joda-time" % "joda-time" % "2.1",
      "org.joda" % "joda-convert" % "1.2"
    ),
    resolvers ++= Seq(
      "Maven Central Server" at "http://repo1.maven.org/maven2",
      "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/",
      "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
    )
  )
}