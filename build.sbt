name := "prover"

version := "0.1"

scalaVersion := "2.12.0"

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.springframework.boot" % "spring-boot-starter-web" % "1.4.2.RELEASE",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4",
  "com.typesafe.akka" %% "akka-actor" % "2.5.3",
  "org.webjars" % "jquery" % "3.1.1",
  "org.webjars" % "bootstrap" % "3.3.7",
  "org.webjars" % "angularjs" % "1.6.4",
  "org.webjars" % "lodash" % "4.15.0",
  "org.webjars.bower" % "mark.js" % "8.4.0",
  "commons-io" % "commons-io" % "2.5",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  "org.specs2" %% "specs2-mock" % "3.8.6" % "test")

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

TwirlKeys.templateImports += "net.prover.model._"
