
organization := "reversible-concurrency"

version := "0.0.1"

scalaVersion := "2.10.0"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers ++= Seq(
  "typesafe repo"      at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scala-stm"           %%  "scala-stm" % "0.7",
  "com.typesafe.akka"       %%  "akka-actor"    % "2.1.0",
  "com.typesafe.akka"       %%  "akka-agent"    % "2.1.0",
  "com.typesafe.akka"       %%  "akka-slf4j"    % "2.1.0",
  "ch.qos.logback" % "logback-classic" % "1.0.9"
)


