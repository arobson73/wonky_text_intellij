import Resolvers._
import Dependencies._

// factor out common settings into a sequence
lazy val buildSettings = Seq(
 organization := "com.andyr",
 version := "0.1.0",
 scalaVersion := "2.12.1"
)

// Sub-project specific dependencies
lazy val commonDeps = Seq(
 scalalogging,
  logback,
//  slf4j,
 scalatest % Test
)

lazy val macroDeps = Seq(
  reflect
)

lazy val root = (project in file(".")).
  //aggregate().
  //dependsOn(foo,bar).
  dependsOn(macros).
  settings(buildSettings: _*).
 settings(
  resolvers := oracleResolvers,
  libraryDependencies ++= commonDeps 
 )
 lazy val macros = (project in file("macros"))
   .settings(buildSettings: _*)
   .settings(
     resolvers := oracleResolvers,
     libraryDependencies ++= macroDeps
)
//needed since seems like an issue with sbt run and loading a wav file
//so this will spawn a seprate jvm. one for sbt the other for the 
//running program. (I think)
fork in run := true


//lazy val foo = (project in file("Foo"))  
//lazy val bar = (project in file("Bar"))
