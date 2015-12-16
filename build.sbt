organization  := "pl.bka"

version       := "0.1"

scalaVersion  := "2.11.6"

resolvers += "Spray" at "http://repo.spray.io"

scalacOptions += "-deprecation"

libraryDependencies ++= {
  Seq(
    "com.typesafe.akka" %% "akka-http-experimental" % "2.0-M2",
    "com.lihaoyi" %% "upickle" % "0.2.8",
    "org.scalatest" %% "scalatest" % "2.2.5" % "test"
  )
}

