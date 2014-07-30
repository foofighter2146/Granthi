//
// Copyright 2014 Dr. Thomas Richert
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

name := "Granthi"

organization := "granthi"

version := "0.2.0"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "neo4j-releases" at "http://m2.neo4j.org/releases/",
  "opencast-public" at "http://repository.opencastproject.org/nexus/content/repositories/public",
  "anormcypher" at "http://repo.anormcypher.org/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.neo4j" % "neo4j" % "2.1.2",
  "org.anormcypher" %% "anormcypher" % "0.5.1",
  "org.scala-lang" % "scala-reflect" % "2.11.1",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2" % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.2" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
)
