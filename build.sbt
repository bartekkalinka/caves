organization  := "pl.bka"

version       := "0.1"

scalaVersion  := "2.11.6"

resolvers += "Spray" at "http://repo.spray.io"

scalacOptions += "-deprecation"

Revolver.settings

libraryDependencies ++= {
  val akkaV = "2.3.11"
  val sprayV = "1.3.3"
  Seq(
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-json"    % "1.3.2",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.wandoulabs.akka" %%  "spray-websocket" % "0.1.4"
  )
}

