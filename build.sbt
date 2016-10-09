organization  := "pl.bka"

version       := "0.1"

scalaVersion  := "2.11.8"

enablePlugins(JavaAppPackaging)

resolvers += "Spray" at "http://repo.spray.io"

scalacOptions += "-deprecation"

libraryDependencies ++= {
  Seq(
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.11",
    "com.lihaoyi" %% "upickle" % "0.2.8",
    "org.scalatest" %% "scalatest" % "2.2.5" % "test"
  )
}

