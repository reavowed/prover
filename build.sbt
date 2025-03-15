name := "prover"

version := "0.1"

scalaVersion := "2.13.12"

val monocleVersion = "2.1.0"

addCompilerPlugin("io.tryp" % "splain" % "1.0.3" cross CrossVersion.patch)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.13",
  "org.springframework.boot" % "spring-boot-starter-web" % "3.4.3",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.3",
  "commons-io" % "commons-io" % "2.18.0",
  "com.googlecode.concurrent-locks" % "concurrent-locks" % "1.0.0",
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.3.8",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "4.20.9" % "test",
  "org.specs2" %% "specs2-mock" % "4.20.9" % "test")
