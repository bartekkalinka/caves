organization  := "pl.bka"

version       := "0.1"

scalaVersion  := "2.11.6"

resolvers += "Spray" at "http://repo.spray.io"

scalacOptions += "-deprecation"

Revolver.settings

libraryDependencies ++= {
  val akkaV = "2.3.12"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-http-experimental" % "1.0",
    "com.lihaoyi" %% "upickle" % "0.2.8"
  )
}

