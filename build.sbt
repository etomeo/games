name := "games"

version := "1.0"

scalaVersion:="2.10.2"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Classpaths.sbtPluginReleases

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "2.1.7" % "test")

ScoverageKeys.excludedPackages in ScoverageCompile := "gameoflife\\.GameOfLifeApp.*" // not working... used comments in class

ScoverageKeys.minimumCoverage := 80

ScoverageKeys.failOnMinimumCoverage := true

ScoverageKeys.highlighting := true

instrumentSettings