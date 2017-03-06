import sbt._
import Keys._

object Dependencies {
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"
  val scalalogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.2"
  //val slf4j = "org.slf4j" % "slf4j-api" % "1.7.10"
 val reflect = "org.scala-lang" % "scala-reflect" % "2.12.1"
  //val slf4j = "com.typesafe.scala-logging" % "scala-logging-api_2.11" % "2.1.2"
 // val slf4j = "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11.0-RC4" % "2.0.2"
}
