name := "prover"

version := "0.1"

scalaVersion := "3.6.4"

val monocleVersion = "3.3.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "shapeless3-deriving" % "3.5.0",
  "org.springframework.boot" % "spring-boot-starter-web" % "3.4.4",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.18.3",
  "commons-io" % "commons-io" % "2.18.0",
  "com.googlecode.concurrent-locks" % "concurrent-locks" % "1.0.0",
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.3.8",
  "dev.optics" %% "monocle-core" % monocleVersion,
  "dev.optics" %% "monocle-macro" % monocleVersion)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "5.5.8" % "test",
  "org.mockito" % "mockito-core" % "4.11.0" % "test",
  "org.hamcrest" % "hamcrest" % "2.2" % "test")
